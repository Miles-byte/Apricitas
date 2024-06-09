pacman::p_load(ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

??beaGet

list <- beaSets(beaKey = Sys.getenv("BEA_KEY"))

params <- beaParams(beaKey = Sys.getenv("BEA_KEY"), "FixedAssets")

paramvals <- beaParamVals(beaKey = Sys.getenv("BEA_KEY"), setName = "FixedAssets", paramName = "TableName")

list$Dataset

params$Parameter

BEA_GOV_FIXED_INVEST_NOM_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "FixedAssets", # Specify dataset
  "TableName" = "FAAt705", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "Year" =  paste(seq(from = 1900, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GOV_FIXED_INVEST_NOM <- beaGet(BEA_GOV_FIXED_INVEST_NOM_SPECS, iTableStyle = FALSE) %>%
  select(`FAAt705 i3gstlc1sn00 65 Transportation Historical Cost Level 6`, `FAAt705 i3gstlc1sq00 67 Highways and streets Historical Cost Level 6`) %>%
  setNames(c("Transportation_N","Highways_N")) %>%
  mutate(date = (seq(as.Date("1901-01-01"), length.out = nrow(.), by = "1 year")))

BEA_GOV_FIXED_INVEST_QUAN_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "FixedAssets", # Specify dataset
  "TableName" = "FAAt706", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "Year" =  paste(seq(from = 1900, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_GOV_FIXED_INVEST_QUAN <- beaGet(BEA_GOV_FIXED_INVEST_QUAN_SPECS, iTableStyle = FALSE) %>%
  select(`FAAt706 icgstlc1sn00 65 Transportation Fisher Quantity Index Level 0`, `FAAt706 icgstlc1sq00 67 Highways and streets Fisher Quantity Index Level 0`) %>%
  setNames(c("Transportation_Q","Highways_Q")) %>%
  mutate(date = (seq(as.Date("1901-01-01"), length.out = nrow(.), by = "1 year")))

BEA_GOV_FIXED_INVEST_REAL <- merge(BEA_GOV_FIXED_INVEST_NOM, BEA_GOV_FIXED_INVEST_QUAN, by = "date") %>%
  transmute(date, Highways = Highways_N[117]*Highways_Q/100, Transportation = Transportation_N[117]*Transportation_Q/100)

POPULATION <- fredr("B230RC0A052NBEA") %>%
  select(date,value)

BEA_GOV_FIXED_INVEST_REAL_POPULATION <- merge(BEA_GOV_FIXED_INVEST_REAL,POPULATION, by = "date") %>%
  mutate(Highways = Highways/value, Transportation = Transportation/value)

BEA_GOV_FIXED_INVEST_REAL_graph <- ggplot() + #plotting fixed gov investment
  geom_line(data= BEA_GOV_FIXED_INVEST_REAL_POPULATION, aes(x=date,y= Highways*1000, color= "Highways & Streets"), size = 1.25) +
  geom_line(data= BEA_GOV_FIXED_INVEST_REAL_POPULATION, aes(x=date,y= Transportation*1000, color= "Mass Transit (Including Airports)"), size = 1.25) +
  xlab("Year") +
  ylab("2017 Dollars Per Capita") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), breaks = c(0,100,200,300,400,500,600), limits = c(0,600), expand = c(0,0)) +
  ggtitle("US Transportation Investment Has Stagnated") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "American Real Per-Capita Investment In Highways & Mass Transit Has Been Flat for Decades") +
  theme_apricitas + theme(legend.position = c(.71,.87), plot.title = element_text(size = 26)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Real Per Capita Government Fixed Investment",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1929-01-01")-(.1861*(today()-as.Date("1929-01-01"))), xmax = as.Date("1929-01-01")-(0.049*(today()-as.Date("1929-01-01"))), ymin = 0-(.3*600), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_GOV_FIXED_INVEST_REAL_graph, "BEA Gov Fixed Invest Real.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

NY_PUMAS <- pumas(state = "NY", cb = TRUE, year = 2020)
NJ_PUMAS <- pumas(state = "NJ", cb = TRUE, year = 2019)
CT_PUMAS <- pumas(state = "CT", cb = FALSE, year = 2023)

?pumas

library(tigris)
library(acs)
library(stringr)
library(leaflet)
library(htmlwidgets)

geo <- geo.make(state=c('MI'),
                puma='*')

NY_TEST <- st_read("C:/Users/Joseph/Downloads/cb_2020_36_puma20_500k/cb_2020_36_puma20_500k.shp") %>%
  filter(startsWith(NAMELSAD20, "NYC"))

INFO_TECH_EMPLOYMENT_PCT_MAP <- ggplot() +
  geom_sf(data = NY_TEST, aes(fill = ALAND20)) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.35) + # Black borders for states
  scale_fill_viridis_c(breaks = c(-.4,-.2,.0,.2,.4,.6), labels = c("-40%","-20%","0%","20%","40%","60%")) +
  ggtitle(" Change In Information Tech Jobs Since Dec 2019") +
  geom_text(data = filter(states_labels, state_abbv %in% c("CA","TX","WA","NY","FL")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Info_Tech_Pct_Change >= 0, "+", ""), sprintf("%.0f", round(Info_Tech_Pct_Change*100, 0)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  labs(caption = "Graph created by @JosephPolitano using BLS data\nNOTE: Info Tech Calculated as Information Sector (NAICS 51) Excluding Movie/Sound Recording (512) and Telecom (517)") +
  labs(fill = NULL) +
  theme_apricitas + theme(plot.title = element_text(size = 27),legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
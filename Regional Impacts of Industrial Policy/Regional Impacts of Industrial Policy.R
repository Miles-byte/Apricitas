pacman::p_load(ggpubr,prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

REAL_PRIVATE_FIXED_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U50406',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_PRIVATE_FIXED_INVEST_BULK <- beaGet(REAL_PRIVATE_FIXED_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2007-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

REAL_PUBLIC_FIXED_INVEST_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T30906',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2002, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

REAL_PUBLIC_FIXED_INVEST_BULK <- beaGet(REAL_PUBLIC_FIXED_INVEST_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2007-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()


REAL_US_CONSTRUCTION_GROWTH_Graph <- ggplot() + #indexed investment growth
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = filter(REAL_PUBLIC_FIXED_INVEST_BULK, date >= as.Date("2018-01-01")), aes(x=date, y = t30906_a760rx_4_structures_chained_dollars_level_6/t30906_a760rx_4_structures_chained_dollars_level_6[5]-1, color = "Public Infrastructure\n(Roads, Schools, etc)"), size = 1.25) + 
  geom_line(data = filter(REAL_PRIVATE_FIXED_INVEST_BULK, date >= as.Date("2018-01-01")), aes(x=date, y = u50406_lb001174_18_alternative_electric_chained_dollars_level_6/u50406_lb001174_18_alternative_electric_chained_dollars_level_6[5]-1, color = "Alternative Electric Power\n(Wind, Solar, etc)"), size = 1.25) + 
  geom_line(data = filter(REAL_PRIVATE_FIXED_INVEST_BULK, date >= as.Date("2018-01-01")), aes(x=date, y = u50406_c307rx_14_manufacturing_chained_dollars_level_6/u50406_c307rx_14_manufacturing_chained_dollars_level_6[5]-1, color = "Manufacturing Structures\n(Factories, Fabs, etc)"), size = 1.25) + 
  annotate("vline", x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Inflation\nReduction\n& CHIPS Acts", x = as.Date("2022-09-16"), y = 1.35, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-11-15"), xintercept = as.Date("2021-11-15"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Bipartisan\nInfrastructure\nLaw", x = as.Date("2021-10-15"), y = 1.35, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  geom_text(data = REAL_PRIVATE_FIXED_INVEST_BULK %>% filter(date >= as.Date("2018-01-01")) %>% mutate(u50406_c307rx_14_manufacturing_chained_dollars_level_6 = u50406_c307rx_14_manufacturing_chained_dollars_level_6/u50406_c307rx_14_manufacturing_chained_dollars_level_6[5]-1) %>% filter(date == max(date)), aes(label = paste0("+",round(u50406_c307rx_14_manufacturing_chained_dollars_level_6,2)*100,"%"), x = date + 120, y = u50406_c307rx_14_manufacturing_chained_dollars_level_6), color = "#FFE98F", size = 5) + 
  geom_text(data = REAL_PRIVATE_FIXED_INVEST_BULK %>% filter(date >= as.Date("2018-01-01")) %>% mutate(u50406_lb001174_18_alternative_electric_chained_dollars_level_6 = u50406_lb001174_18_alternative_electric_chained_dollars_level_6/u50406_lb001174_18_alternative_electric_chained_dollars_level_6[5]-1) %>% filter(date == max(date)), aes(label = paste0("+",round(u50406_lb001174_18_alternative_electric_chained_dollars_level_6,2)*100,"%"), x = date + 120, y = u50406_lb001174_18_alternative_electric_chained_dollars_level_6), color = "#00A99D", size = 5) + 
  geom_text(data = REAL_PUBLIC_FIXED_INVEST_BULK %>% filter(date >= as.Date("2018-01-01")) %>% mutate(t30906_a760rx_4_structures_chained_dollars_level_6 = t30906_a760rx_4_structures_chained_dollars_level_6/t30906_a760rx_4_structures_chained_dollars_level_6[5]-1) %>% filter(date == max(date)), aes(label = paste0("+",round(t30906_a760rx_4_structures_chained_dollars_level_6,2)*100,"%"), x = date + 80, y = t30906_a760rx_4_structures_chained_dollars_level_6), color = "#EE6055", size = 5) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-.25,1.50), expand = c(0,0)) +
  ylab("Growth Since Q1 2019") +
  ggtitle("Growth in Real US Construction") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Real Investment in Key Areas Has Grown Significantly Since the Passage of BIF, IRA, and CHIPS") +
  theme_apricitas + theme(legend.position = c(.18,.775), legend.key.height = unit(1.2,"cm"), legend.spacing.y = unit(0, "cm")) +
  scale_color_manual(name= "Growth Since Q1 2019",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Manufacturing Structures\n(Factories, Fabs, etc)","Alternative Electric Power\n(Wind, Solar, etc)","Public Infrastructure\n(Roads, Schools, etc)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.25-(.3*1.75), ymax = -.25) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REAL_US_CONSTRUCTION_GROWTH_Graph, "Real US Construction Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


STATE_PRIVATE_CONSTRUCTION_BULK <- read.xlsx("https://www.census.gov/construction/c30/xlsx/nrstate.xlsx") 

STATE_PRIVATE_CONSTRUCTION <- STATE_PRIVATE_CONSTRUCTION_BULK %>%
  slice(-1) %>%
  select(-`Annual.Value.of.Private.Nonresidential.Construction.Put.in.Place.by.State,.2008-2023`, -X19) %>%
  row_to_names(1) %>%
  rename(NAME = 1) %>%
  drop_na() %>%
  mutate(across(`2008`:`2023`, as.numeric)) %>%
  mutate(`2019-2023` = (`2023`-`2019`)/`2019`, `2021-2023` = (`2023`-`2021`)/`2021`) %>%
  mutate(`2019_minus_2023` = (`2023`-`2019`))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") #moves Alaska and Hawaii below the map

STATE_PRIVATE_CONSTRUCTION_MAP <- merge(STATE_PRIVATE_CONSTRUCTION,State_map, by = "NAME") %>%
  st_as_sf()
  
STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS <- STATE_PRIVATE_CONSTRUCTION_MAP %>%
  st_centroid()


michigan_index <- which(STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Michigan")
michigan_centroid <- STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[michigan_index]

florida_index <- which(STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Florida")
florida_centroid <- STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[florida_index]

california_index <- which(STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "California")
california_centroid <- STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[california_index]

louisiana_index <- which(STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Louisiana")
louisiana_centroid <- STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[louisiana_index]

hawaii_index <- which(STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Hawaii")
hawaii_centroid <- STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[hawaii_index]


adjusted_michigan_centroid <- st_geometry(michigan_centroid) + c(60000, -130000)
adjusted_florida_centroid <- st_geometry(florida_centroid) + c(70000, 0)
adjusted_california_centroid <- st_geometry(california_centroid) + c(-60000, 0)
adjusted_louisiana_centroid <- st_geometry(louisiana_centroid) + c(-60000, 0)
adjusted_hawaii_centroid <- st_geometry(hawaii_centroid) + c(120000, -100000)


STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid


  
STATE_PRIVATE_CONSTRUCTION_MAP_GRAPH <- STATE_PRIVATE_CONSTRUCTION_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_PRIVATE_CONSTRUCTION_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_PRIVATE_CONSTRUCTION_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = `2019-2023`, size = `2023`/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_b(name = "2019-2023 Growth, %",breaks = c(0,.25,.5,.75,1), labels = c("0%","25%","50%","75%","100%")) +
  scale_size_area(name = "2023 Construction",
                  max_size = 17,
                  breaks = c(20,40,60,80),
                  labels = c("$20B","$40B","$60B","$80B"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("  Private Nonresidential Construction by State, 2023") +
  labs(caption = "Graph created by @JosephPolitano using Census data. NOTE: Dollars Not Adjusted for Inflation") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_PRIVATE_CONSTRUCTION_MAP_GRAPH, "States Private Nonresidential Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STATE_PUBLIC_CONSTRUCTION_BULK <- read.xlsx("https://www.census.gov/construction/c30/xlsx/slstate.xlsx") 

STATE_PUBLIC_CONSTRUCTION <- STATE_PUBLIC_CONSTRUCTION_BULK %>%
  slice(-1) %>%
  select(-`Annual.Value.of.State.and.Local.Construction.Put.in.Place.by.State,.2006-2023`, -X21) %>%
  row_to_names(1) %>%
  rename(NAME = 1) %>%
  drop_na() %>%
  mutate(across(`2008`:`2023`, as.numeric)) %>%
  mutate(`2019-2023` = (`2023`-`2019`)/`2019`, `2021-2023` = (`2023`-`2021`)/`2021`)

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") #moves Alaska and Hawaii below the map

STATE_PUBLIC_CONSTRUCTION_MAP <- merge(STATE_PUBLIC_CONSTRUCTION,State_map, by = "NAME") %>%
  st_as_sf()

STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS <- STATE_PUBLIC_CONSTRUCTION_MAP %>%
  st_centroid()


michigan_index <- which(STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$NAME == "Michigan")
michigan_centroid <- STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[michigan_index]

florida_index <- which(STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$NAME == "Florida")
florida_centroid <- STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[florida_index]

california_index <- which(STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$NAME == "California")
california_centroid <- STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[california_index]

louisiana_index <- which(STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$NAME == "Louisiana")
louisiana_centroid <- STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[louisiana_index]

hawaii_index <- which(STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$NAME == "Hawaii")
hawaii_centroid <- STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[hawaii_index]


adjusted_michigan_centroid <- st_geometry(michigan_centroid) + c(60000, -130000)
adjusted_florida_centroid <- st_geometry(florida_centroid) + c(70000, 0)
adjusted_california_centroid <- st_geometry(california_centroid) + c(-60000, 0)
adjusted_louisiana_centroid <- st_geometry(louisiana_centroid) + c(-60000, 0)
adjusted_hawaii_centroid <- st_geometry(hawaii_centroid) + c(120000, -100000)


STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid



STATE_PUBLIC_CONSTRUCTION_MAP_GRAPH <- STATE_PUBLIC_CONSTRUCTION_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_PUBLIC_CONSTRUCTION_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_PUBLIC_CONSTRUCTION_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = `2019-2023`, size = `2023`/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_b(name = "2019-2023 Growth, %",breaks = c(0,.25,.5,.75,1), labels = c("0%","25%","50%","75%","100%")) +
  scale_size_area(name = "2023 Construction",
                  max_size = 17,
                  breaks = c(10,20,30,40),
                  labels = c("$10B","$20B","$30B","$40B"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("         State & Local Construction by State, 2023") +
  labs(caption = "Graph created by @JosephPolitano using Census data. NOTE: Dollars Not Adjusted for Inflation") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_PUBLIC_CONSTRUCTION_MAP_GRAPH, "States Public Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STATE_PUBLIC_PRIVATE_CONSTRUCTION <- STATE_PRIVATE_CONSTRUCTION %>%
  select(-`2019-2023`,-`2021-2023`,-`2019_minus_2023`) %>%
  rbind(.,STATE_PUBLIC_CONSTRUCTION %>% select(-`2006`,-`2007`,-`2019-2023`,-`2021-2023`)) %>%
  group_by(NAME) %>%
  summarize(across(everything(), sum, na.rm = TRUE)) %>%
  mutate(`2019-2023` = (`2023`-`2019`)/`2019`, `2021-2023` = (`2023`-`2021`)/`2021`)

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") #moves Alaska and Hawaii below the map

STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP <- merge(STATE_PUBLIC_PRIVATE_CONSTRUCTION,State_map, by = "NAME") %>%
  st_as_sf()

STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP %>%
  st_centroid()


michigan_index <- which(STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Michigan")
michigan_centroid <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[michigan_index]

florida_index <- which(STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Florida")
florida_centroid <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[florida_index]

california_index <- which(STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "California")
california_centroid <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[california_index]

louisiana_index <- which(STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Louisiana")
louisiana_centroid <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[louisiana_index]

hawaii_index <- which(STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$NAME == "Hawaii")
hawaii_centroid <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[hawaii_index]


adjusted_michigan_centroid <- st_geometry(michigan_centroid) + c(60000, -130000)
adjusted_florida_centroid <- st_geometry(florida_centroid) + c(70000, 0)
adjusted_california_centroid <- st_geometry(california_centroid) + c(-60000, 0)
adjusted_louisiana_centroid <- st_geometry(louisiana_centroid) + c(-60000, 0)
adjusted_hawaii_centroid <- st_geometry(hawaii_centroid) + c(120000, -100000)


STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid



STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_GRAPH <- STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = `2019-2023`, size = `2023`/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_b(name = "2019-2023 Growth, %",breaks = c(0,.25,.5,.75,1), labels = c("0%","25%","50%","75%","100%")) +
  scale_size_area(name = "2023 Construction",
                  max_size = 17,
                  breaks = c(40,80,120),
                  labels = c("$40B","$80B","$120B"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("    Total Nonresidential Construction by State, 2023") +
  labs(caption = "Graph created by @JosephPolitano using Census data.NOTE: Dollars Not Adjusted for Inflation\n Data Limitations Require Excluding Small Amounts of Federal Construction & Including Small Amounts of Public Housing") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_PUBLIC_PRIVATE_CONSTRUCTION_MAP_GRAPH, "States Public Private Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


STATE_POPULATION_DATA <- read.xlsx("https://www2.census.gov/programs-surveys/popest/tables/2020-2023/state/totals/NST-EST2023-POP.xlsx") %>%
  slice(-1:-3) %>%
  setNames(c("NAME","2020_BASE","2020","2021","2022","2023")) %>%
  drop_na() %>%
  select(NAME,`2023`) %>%
  mutate(NAME = gsub("\\.","",NAME))

STATE_PRIVATE_CONSTRUCTION_PER_CAPITA <- STATE_PRIVATE_CONSTRUCTION_MAP %>%
  as.data.frame() %>%
  select(NAME,`2023`) %>%
  merge(.,STATE_POPULATION_DATA, by = "NAME") %>%
  transmute(NAME,Construction_PC = (`2023.x`*1000000)/`2023.y`) %>%
  arrange(desc(Construction_PC)) %>%
  slice(1:25) %>%
  arrange(Construction_PC) %>%
  mutate(state_type = case_when(
    NAME %in% c("Michigan", "Nevada", "Pennsylvania", "Wisconsin", "Arizona", "Georgia", "North Carolina") ~ "Swing",
    NAME %in% c("Texas", "Maine's 2nd Congressional District", "Ohio", "Iowa", "Alaska", 
                 "South Carolina", "Kansas", "Nebraska's 1st Congressional District", 
                 "Missouri", "Indiana", "Montana", "Mississippi", "Louisiana", "Nebraska", 
                 "Utah", "Tennessee", "Alabama", "Kentucky", "South Dakota", "Arkansas", 
                 "Idaho", "Oklahoma", "North Dakota", "West Virginia", "Wyoming", "Florida") ~ "Republican",
    TRUE ~ "Democrat"
  ))

STATE_PRIVATE_CONSTRUCTION_PER_CAPITA <- STATE_PRIVATE_CONSTRUCTION_PER_CAPITA %>%
  mutate(NAME = factor(NAME, levels = STATE_PRIVATE_CONSTRUCTION_PER_CAPITA %>% pull(NAME)))

STATE_PRIVATE_CONSTRUCTION_PER_CAPITA_GRAPH <- ggplot(data = STATE_PRIVATE_CONSTRUCTION_PER_CAPITA, aes(x = NAME, y = Construction_PC/1000, fill = state_type)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"), limits = c(0,6), expand = c(0,0)) +
  ggtitle(paste("Private Nonresidential Construction\nPer Capita, Top 25 States, 2023")) +
  scale_fill_manual(name= "2020 Presidential Election Category",values = c("#9A348E","#EE6055","#3083DC"), breaks = c("Swing","Republican","Democrat")) +
  labs(caption = "Graph created by @JosephPolitano using Census Data\nNOTE: Swing Includes States With a <3% Margin of Victory") +
  theme_apricitas + theme(legend.position = c(.75,.75), axis.text.y = element_text(size = 14), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = STATE_PRIVATE_CONSTRUCTION_PER_CAPITA_GRAPH, "States Private Nonresidential Construction Per Capita Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


STATE_PRIVATE_CONSTRUCTION_LINEAR <- STATE_PRIVATE_CONSTRUCTION %>%
  select(-`2019-2023`,-`2021-2023`,-`2019_minus_2023`) %>%
  transpose() %>%
  row_to_names(1) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(date = seq.Date(from = as.Date("2008-01-01"), by = "1 year", length.out = nrow(.)))
  

STATE_PRIVATE_CONSTRUCTION_LINEAR_Graph <- ggplot() + 
  geom_line(data=filter(STATE_PRIVATE_CONSTRUCTION_LINEAR, date >= as.Date("2010-01-01")), aes(x=date,y= Ohio/1000,color= "Ohio"), size = 1.25) + 
  geom_line(data=filter(STATE_PRIVATE_CONSTRUCTION_LINEAR, date >= as.Date("2010-01-01")), aes(x=date,y= Arizona/1000,color= "Arizona"), size = 1.25) + 
  geom_line(data=filter(STATE_PRIVATE_CONSTRUCTION_LINEAR, date >= as.Date("2010-01-01")), aes(x=date,y= Georgia/1000,color= "Georgia"), size = 1.25) +
  geom_line(data=filter(STATE_PRIVATE_CONSTRUCTION_LINEAR, date >= as.Date("2010-01-01")), aes(x=date,y= Texas/1000,color= "Texas"), size = 1.25) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), breaks = c(0,25,50,75,100), limits = c(0,100), expand = c(0,0)) +
  ggtitle("State Private Nonresidential Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data. NOTE: Dollars Not Adjusted for Inflation", subtitle = "The CHIPS Act & IRA Have Driven a Construction Boom in Arizona, Georgia, Ohio, & Texas") +
  theme_apricitas + theme(legend.position = c(.40,.87), plot.title = element_text(size = 28)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Texas","Arizona","Ohio","Georgia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*100), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STATE_PRIVATE_CONSTRUCTION_LINEAR_Graph, "State Private Construction Linear Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Making USMAP
sf_use_s2(FALSE)

div_dat <- states(cb = FALSE, resolution = '20m') %>%
  st_drop_geometry() %>%
  select(NAME, DIVISION) %>%
  mutate(ID = tolower(NAME))

# get state data, convert to sf, join with division data
states <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  left_join(div_dat)

states <- st_make_valid(states)

# create division polygons
div <- states %>%
  group_by(DIVISION) %>% 
  summarize()

CENSUS_SUBNATIONAL_MFG_SPENDING <- read.xlsx("https://www.census.gov/construction/c30//xlsx/privmfgtime.xlsx") %>%
  .[-1,] %>%
  row_to_names(1) %>%
  select(`New England`,
         `Mid Atlantic`,
         `East North\nCentral`,
         `West North\nCentral`,
         `South Atlantic`,
         `East South\nCentral`,
         `West South\n Central`,
         `Mountain`,
         `Pacific`) %>%
  `colnames<-`(c("1","2","3","4","5","6","7","8","9")) %>%
  mutate_all(as.numeric) %>%
  slice(1:6) %>%
  summarize_all(sum) %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("YRSPENDING")) %>%
  mutate(DIVISION = c("1","2","3","4","5","6","7","8","9"))

states <- merge(states,CENSUS_SUBNATIONAL_MFG_SPENDING, by = "DIVISION")

# plot it
REGIONAL_MFG_SPENDING_GRAPH <- ggplot() + 
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  geom_sf(data = states, 
          aes(fill = YRSPENDING/1000), 
          color = 'grey25') +
  geom_sf(data = div, 
          color = 'black', 
          fill = NA,
          lwd = 1.25) +
  scale_fill_viridis_c(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,10,20,30,40,50,60,70), expand = c(0,0)) +
  # scale_fill_gradient(low = "#00A99D",
  #                      high = "#FFE98F",
  #                      space = "Lab",
  #                      na.value = "grey50",
  #                      guide = "colourbar",
  #                      aesthetics = "fill",
  #                     breaks = c(0,10,20,30,40,50), 
  #                     labels = c("$0B","$10B","$20B","$30B","$40B","$50B"),
  #                     limits = c(0,50)) +
  coord_sf(crs = 5070) +
  geom_text(
    data = filter(states, DIVISION == 1), 
    aes(x = 1950000, y = 2600000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    color = "white",
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 2), 
    aes(x = 1600000, y = 2250000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 3), 
    aes(x = 800000, y = 1950000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 4), 
    aes(x = 00000, y = 2000000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 5), 
    aes(x = 1300000, y = 1300000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 6), 
    aes(x = 725000, y = 1200000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 7), 
    aes(x = -250000, y = 950000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 8), 
    aes(x = -1350000, y = 1800000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "black",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(states, DIVISION == 9), 
    aes(x = -2000000, y = 2600000, label = paste0("$", sprintf("%.1f", round(YRSPENDING/1000, 1)), "B")), 
    size = 5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    color = "white",
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  ggtitle("  Manufacturing Construction Jan-Jun 2024 by Region") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using US Census data") +
  labs(fill = NULL) +
  theme_apricitas + theme(plot.title = element_text(size = 24), legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


ggsave(dpi = "retina",plot = REGIONAL_MFG_SPENDING_GRAPH, "Regional Manufacturing.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#PUBLIC INVESTMENT MONTHLY GRAPH 

STATE_AND_LOCAL_CONSTRUCTION_MONTHLY_BULK <- read.xlsx("https://www.census.gov/construction/c30/xlsx/slsatime.xlsx") 

STATE_AND_LOCAL_CONSTRUCTION_MONTHLY <- STATE_AND_LOCAL_CONSTRUCTION_MONTHLY_BULK %>%
  drop_na() %>%
  row_to_names(1) %>%
  select(-Date) %>%
  mutate(Total = `Total\n_x000D_State and Local Construction`) %>%
  select(-`Total\n_x000D_State and Local Construction`) %>%
  mutate_if(is.character,as.numeric) %>%
  select(-Total) %>%
  .[order(nrow(.):1),] %>%
  mutate(date = seq.Date(from = as.Date("1993-01-01"), by = "month", length.out = nrow(.))) %>%
  filter(date >= as.Date("2018-01-01"))

STATE_LOCAL_POWER_GRAPH <- ggplot() + 
  geom_line(data=STATE_AND_LOCAL_CONSTRUCTION_MONTHLY, aes(x=date,y= `Power`/1000,color= "State & Local Spending\nPower Infrastructure & Generation"), size = 1.25) + 
  annotate("vline", x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Inflation\nReduction\nAct", x = as.Date("2022-07-16"), y = 21, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ggtitle("State & Local Electric Power Construction") +
  labs(caption = "Graph created by @JosephPolitano using BLS data. NOTE: Nominal Dollars Not Adjusted for Inflation", subtitle = "State and Local Spending on Power Infrastructure and Generation has Boomed Since the IRA") +
  theme_apricitas + theme(legend.position = c(.25,.87), plot.title = element_text(size = 28)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STATE_LOCAL_POWER_GRAPH, "State Local Power Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


SELECTED_STATE_AND_LOCAL_CONSTRUCTION_MONTHLY <- STATE_AND_LOCAL_CONSTRUCTION_MONTHLY %>%
  select(date,`Transportation`,`Highway and street`,`Educational`,`Sewage and waste disposal`,`Power`,`Amusement and recreation`,`Public Safety`,`Water supply`) %>%
  setNames(c("date","Airports & Mass Transit","Highway & Street","Educational","Sewage & Waste","Power","Amusement & Recreation","Public Safety","Water Supply")) %>%
  pivot_longer(-date) %>%
  group_by(name) %>%
  arrange(name,date) %>%
  transmute(date,name,`Nominal Spending` = value, `Percent Growth Since 2019` = (value-value[13])/value[13]) %>%
  pivot_longer(cols = c(`Nominal Spending`,`Percent Growth Since 2019`), names_to = "type", values_to = "value") %>%
  mutate(name = factor(name, levels = c("Highway & Street","Educational", "Sewage & Waste","Airports & Mass Transit","Water Supply","Power","Amusement & Recreation","Public Safety")))

plot_nominal <- ggplot(subset(SELECTED_STATE_AND_LOCAL_CONSTRUCTION_MONTHLY, type == "Nominal Spending"), aes(x = date, y = value/1000, color = name)) +
  geom_line(size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"), limits = c(0,150), expand = c(0,0)) +
  ylab("Dollars") +
  labs(subtitle = "Nominal Spending") +
  scale_color_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, 0, 0, 0), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"))

plot_percent_growth <- ggplot(subset(SELECTED_STATE_AND_LOCAL_CONSTRUCTION_MONTHLY, type == "Percent Growth Since 2019"), aes(x = date, y = value, color = name)) +
  geom_line(size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-.40,2.5), expand = c(0,0)) +
  ylab("Percent Growth Since Jan 2019") +
  labs(subtitle = "Percent Growth Since 2019") +
  scale_color_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  theme_apricitas + theme(legend.position = "none", plot.margin= grid::unit(c(0, 0, 0, 0), "in"), plot.subtitle = element_text(size = 20, color = "white", face = "bold"))

# Create a text grob
tgrob <- text_grob(expression(bold("State & Local Construction Spending")),size = 30, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-0.15,-0.15,-0.15),"cm"))  

STATE_AND_LOCAL_CONSTRUCTION_ARRANGE <- ggarrange(plot_nominal,plot_percent_growth, ncol = 2, nrow = 1, heights = c(5,20), widths = 10, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32") 

FINAL_STATE_AND_LOCAL_CONSTRUCTION <- ggarrange(plot_0,STATE_AND_LOCAL_CONSTRUCTION_ARRANGE, nrow = 2, heights = c(2,20), widths = 10)

ggsave(dpi = "retina",plot = FINAL_STATE_AND_LOCAL_CONSTRUCTION, "Final State & Local Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#REGIONAL PUBLIC INVESTMENT GRAPH

REGION_PUBLIC_CONSTRUCTION_BULK <- read.xlsx("https://www.census.gov/construction/c30/xlsx/sldivision.xlsx")

REGION_PUBLIC_CONSTRUCTION <- REGION_PUBLIC_CONSTRUCTION_BULK %>%
  slice(-1:-3) %>%
  select(-1) %>%
  setNames(c("date","US Total","Northeast Total","New England","Middle Atlantic","Midwest Total","East North Central","West North Central","South Total","South Atlantic","East South Central","West South Central","West Total","Mountain","Pacific")) %>%
  mutate(category = date) %>%
  #mutate(date = as.Date(paste0(substr(date, 1, 4), "-01-01"), format = "%Y-%m-%d")) %>%
  mutate(date = as.numeric(substr(date, 1, 4))) %>%
  mutate(category = ifelse(grepl("^[0-9]", category), NA, category)) %>%
  fill(category, .direction = "down") %>%
  drop_na() %>%
  select(-`US Total`,-`Northeast Total`,-`Midwest Total`,-`South Total`,-`West Total`) %>%
  pivot_longer(cols = `New England`:`Pacific`) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(name) %>%
  group_by(category, name) %>%
  mutate(value_2019 = value[date == 2019]) %>%
  ungroup() %>%
  mutate(pct_growth_2019 = (value - value_2019)/value_2019) %>%
  select(-value_2019) %>%
  mutate(DIVISION = case_when(
    name == "New England" ~ 1,
    name == "Middle Atlantic" ~ 2,
    name == "East North Central" ~ 3,
    name == "West North Central" ~ 4,
    name == "South Atlantic" ~ 5,
    name == "East South Central" ~ 6,
    name == "West South Central" ~ 7,
    name == "Mountain" ~ 8,
    name == "Pacific" ~ 9,
    TRUE ~ NA_integer_  # Handles cases where name does not match any of the specified regions
  ))

#Making USMAP
sf_use_s2(FALSE)

div_dat <- states(cb = TRUE, resolution = '20m') %>%
  st_drop_geometry() %>%
  select(NAME, DIVISION) %>%
  mutate(ID = tolower(NAME))

# get state data, convert to sf, join with division data
states <- states(cb = TRUE, resolution = '20m') %>%
  st_as_sf() %>%
  shift_geometry(position = "below") %>%
  left_join(div_dat)

states <- st_make_valid(states)

# create division polygons
div <- states %>%
  group_by(DIVISION) %>% 
  summarize()

div <- merge(div,REGION_PUBLIC_CONSTRUCTION, by = "DIVISION")

div_centroids <- div %>%
  st_centroid()

states_outline <- states(cb = TRUE, resolution = '20m') %>%
  filter(STATEFP <= 56) %>%
  st_as_sf() %>%
  shift_geometry(position = "below") %>%
  left_join(div_dat)

pacific_index <- which(div_centroids$name == "Pacific")
pacific_centroid <- div_centroids$geometry[pacific_index]
adjusted_pacific_centroid <- st_geometry(pacific_centroid) + c(-90000, 0)
div_centroids$geometry[pacific_index] <- adjusted_pacific_centroid


REGION_HIGHWAY_CONSTRUCTION_MAP <- filter(div, date == max(div$date), category == "Highway and Street") %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = states_outline, fill = NA, color = "grey40", lwd = 0.5) +
  geom_sf(data = filter(div, date == max(div$date), category == "Highway and Street"), color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = filter(div_centroids, date == max(div$date), category == "Highway and Street"), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth_2019, size = value/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = paste0("2019-",max(div$date)," Growth, %"),breaks = c(-.25,0,.25,.5,.75),limits = c(-.25,.75), labels = c("-25%","0%","25%","50%","75%")) +
  scale_size_area(name = paste0(max(div$date)," Construction"),
                  max_size = 17,
                  breaks = c(5,10,15,20),
                  labels = c("$5B","$10B","$15B","$20B"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("     Highway & Street Construction by Region, 2023") +
  labs(caption = "Graph created by @JosephPolitano using Census data. NOTE: Dollars Not Adjusted for Inflation.") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = REGION_HIGHWAY_CONSTRUCTION_MAP, "Region Highway Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


REGION_EDUCATIONAL_CONSTRUCTION_MAP <- filter(div, date == max(div$date), category == "Educational") %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = states_outline, fill = NA, color = "grey40", lwd = 0.5) +
  geom_sf(data = filter(div, date == max(div$date), category == "Educational"), color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = filter(div_centroids, date == max(div$date), category == "Educational"), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth_2019, size = value/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = paste0("2019-",max(div$date)," Growth, %"),breaks = c(-.25,0,.25,.5,.75),limits = c(-.25,.75), labels = c("-25%","0%","25%","50%","75%")) +
  scale_size_area(name = paste0(max(div$date)," Construction"),
                  max_size = 17,
                  breaks = c(5,10,15,20),
                  labels = c("$5B","$10B","$15B","$20B"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA), order = 1)) +
  ggtitle(paste0("   Public Education Construction by Region, ", max(div$date))) +
  labs(caption = "Graph created by @JosephPolitano using Census data. NOTE: Dollars Not Adjusted for Inflation.") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) 

ggsave(dpi = "retina",plot = REGION_EDUCATIONAL_CONSTRUCTION_MAP, "Region Educational Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



GENERATOR_CAPACITY_MAP_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/GENERATOR_CAPACITY_ADDITIONS_MAP_DATA.csv") %>%
  transmute(capacity = as.numeric(gsub(",","",Nameplate.Capacity..MW.)), name = Energy.Source.Code, year = Planned.Operation.Year, state = Plant.State, month = Planned.Operation.Month, Latitude, Longitude) %>%
  filter(year == 2024 | (year == 2025 & month < 7)) %>% #NOTE: MUST BE MANUALLY EDITED TO SET DATE OF INTEREST
  filter(name %in% c("SUN","WND","MWH","NUC")) %>%
  mutate(name = case_when(
    name == "SUN" ~ "Solar",
    name == "WND" ~ "Wind",
    name == "NUC" ~ "Nuclear",
    name == "MWH" ~ "Batteries",
    TRUE ~ as.character(name)  # Keeps other values unchanged
  )) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326) %>%
  shift_geometry(position = "below")

CAPACITY_BREAKDOWN <- GENERATOR_CAPACITY_MAP_DATA %>%
  st_drop_geometry() %>%
  group_by(state, name) %>%
  summarise(total_capacity = sum(capacity, na.rm = TRUE))

TOTAL_CAPACITY <- GENERATOR_CAPACITY_MAP_DATA %>%
  st_drop_geometry() %>%
  group_by(name) %>%  # Group by the type, e.g., solar
  summarise(total_capacity_by_type = sum(capacity, na.rm = TRUE))

TOTAL_CAPACITY_PCT <- left_join(CAPACITY_BREAKDOWN,TOTAL_CAPACITY, by = "name") %>%
  mutate(percent_of_type_total = round(total_capacity/total_capacity_by_type*100,2))


states <- states(cb = TRUE, year = 2021) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS)) %>%
  shift_geometry(position = "below") %>% #THIS PUTS HAWAII AND ALASKA BELOW THE MAP
  filter(STATEFP < 60)

GENERATOR_CAPACITY_MAP <- states %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  geom_point(data = GENERATOR_CAPACITY_MAP_DATA, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = name, size = capacity), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_manual(name = "Type",
                    values = c("#FFE98F","#9A348E","#3083DC"),
                    breaks = c("Solar", "Wind","Batteries"), 
                    labels = c("Solar", "Wind","Batteries"),
                    guide = guide_legend(override.aes = list(color = c("#FFE98F","#9A348E","#3083DC"), size = 5))) +
  scale_size_area(name = "Capacity",
                  max_size = 7,
                  breaks = c(250,500,750,1000),
                  limits = c(0,1114.0),
                  labels = c("250MW","500MW","750MW","1GW"),
                  guide = guide_legend(override.aes = list(fill = c("#FFE98F")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle(" New Clean Power Capacity Planned for the Next 12M") +
  labs(caption = "Graph created by @JosephPolitano using EIA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 25.5),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = GENERATOR_CAPACITY_MAP, "New Generator Capacity Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TX <- counties(cb = TRUE, year = 2021) %>%
  filter(STUSPS == "TX") %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS))

TX_GENERATOR_CAPACITY_MAP <- TX %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = filter(states, STUSPS == "TX"), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  geom_point(data = filter(GENERATOR_CAPACITY_MAP_DATA, state == "TX"), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = name, size = capacity), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  coord_sf(xlim = c(-1500000, 500000)) +
  scale_fill_manual(name = "Type",
                    values = c("#FFE98F","#9A348E","#3083DC"),
                    breaks = c("Solar", "Wind","Batteries"), 
                    labels = c("Solar", "Wind","Batteries"),
                    guide = guide_legend(override.aes = list(color = c("#FFE98F","#9A348E","#3083DC"), size = 5))) +
  scale_size_area(name = "Capacity",
                  max_size = 10,
                  breaks = c(250,500,750,1000),
                  limits = c(0,1114.0),
                  labels = c("250MW","500MW","750MW","1GW"),
                  guide = guide_legend(override.aes = list(fill = c("#FFE98F")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Texas New Clean Power Capacity Planned for 2024") +
  labs(caption = "Graph created by @JosephPolitano using EIA data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = TX_GENERATOR_CAPACITY_MAP, "TX New Generator Capacity Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


OPERATING_CLEAN_POWER_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/GENERATOR_CAPACITY_OPERATING_MAP_DATA.csv") %>%
  select(Plant.State,Nameplate.Capacity..MW.,Energy.Source.Code,Operating.Month,Operating.Year) %>%
  setNames(c("State","Capacity","Source","Month","Year")) %>%
  mutate(Date = as.Date(paste0(Year,"-",Month,"-01"))) %>%
  filter(Date >= as.Date("2022-08-01") & Date <= as.Date("2024-06-01")) %>% #THIS DATE VALUE NEEDS TO BE UPDATED WHEN NEW DATA IS INCORPORATED
  mutate(Capacity =  as.numeric(gsub(",","",Capacity))) %>%
  filter(Source %in% c("WND","SUN","MWH")) %>%
  group_by(State,Source) %>%
  summarize(Capacity = sum(Capacity, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(State %in% c("TX","CA","FL","AZ","NV","OH","NM","CO","VA","NY")) %>%
  mutate(Source = case_when(
    Source == "MWH" ~ "Batteries",
    Source == "NUC" ~ "Nuclear",
    Source == "SUN" ~ "Solar",
    Source == "WND" ~ "Wind",
    TRUE ~ Source
  )) %>%
  mutate(Source = factor(Source, levels = c("Solar","Wind","Batteries","Nuclear"))) %>%
  mutate(State = case_when(
    State == "TX" ~ "Texas",
    State == "CA" ~ "California",
    State == "FL" ~ "Florida",
    State == "AZ" ~ "Arizona",
    State == "NV" ~ "Nevada",
    State == "OH" ~ "Ohio",
    State == "NM" ~ "New Mexico",
    State == "CO" ~ "Colorado",
    State == "VA" ~ "Virginia",
    State == "NY" ~ "New York",
    TRUE ~ State
  )) %>%
  mutate(State = factor(State, levels = rev(c("Texas","California","Florida","Arizona","Nevada","Ohio","New Mexico","Colorado","Virginia","New York"))))
  
CAPACITY_BREAKDOWN <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/GENERATOR_CAPACITY_OPERATING_MAP_DATA.csv") %>%
  select(Plant.State,Nameplate.Capacity..MW.,Energy.Source.Code,Operating.Month,Operating.Year) %>%
  setNames(c("State","Capacity","Source","Month","Year")) %>%
  mutate(Date = as.Date(paste0(Year,"-",Month,"-01"))) %>%
  filter(Date >= as.Date("2022-08-01") & Date <= as.Date("2024-06-01")) %>% #THIS DATE VALUE NEEDS TO BE UPDATED WHEN NEW DATA IS INCORPORATED
  mutate(Capacity =  as.numeric(gsub(",","",Capacity))) %>%
  filter(Source %in% c("WND","SUN","MWH")) %>%
  group_by(State,Source) %>%
  summarise(total_capacity = sum(Capacity, na.rm = TRUE))

TOTAL_CAPACITY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Solar%20Revolution/GENERATOR_CAPACITY_OPERATING_MAP_DATA.csv") %>%
  select(Plant.State,Nameplate.Capacity..MW.,Energy.Source.Code,Operating.Month,Operating.Year) %>%
  setNames(c("State","Capacity","Source","Month","Year")) %>%
  mutate(Date = as.Date(paste0(Year,"-",Month,"-01"))) %>%
  filter(Date >= as.Date("2022-08-01") & Date <= as.Date("2024-06-01")) %>% #THIS DATE VALUE NEEDS TO BE UPDATED WHEN NEW DATA IS INCORPORATED
  mutate(Capacity =  as.numeric(gsub(",","",Capacity))) %>%
  filter(Source %in% c("WND","SUN","MWH")) %>%
  group_by(Source) %>%  # Group by the type, e.g., solar
  summarise(total_capacity_by_type = sum(Capacity, na.rm = TRUE))

TOTAL_CAPACITY_PCT <- left_join(CAPACITY_BREAKDOWN,TOTAL_CAPACITY, by = "Source") %>%
  mutate(percent_of_type_total = round(total_capacity/total_capacity_by_type*100,2))

  

OPERATING_CLEAN_POWER_DATA_GRAPH <- ggplot(data = OPERATING_CLEAN_POWER_DATA, aes(x = State, y = Capacity/1000, fill = Source)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  facet_wrap(~ Source, ncol = 3) +
  coord_flip() +
  xlab(NULL) +
  ylab("Capacity") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "GW"), limits = c(0,10), breaks = c(2,6,10), expand = c(0,0)) +
  ggtitle(paste("New Renewables Since the IRA Passed\nTop 10 States by Total Additions")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#3083DC","#00A99D"), breaks = c("Solar","Wind","Batteries","Nuclear")) +
  labs(caption = "Graph created by @JosephPolitano using EIA Data") +
  theme_apricitas + theme(legend.position = "none", axis.text.y = element_text(size = 14, color = "grey80"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), strip.text.x = element_text(size = 20, face = "bold", color = "white")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = OPERATING_CLEAN_POWER_DATA_GRAPH, "Operating Clean Power Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

INDUS_CONS_EMP <- bls_api("CES2023621001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Industrial Buildings")

HWAY_CONS_EMP <- bls_api("CES2023730001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Highways & Streets")

UTIL_CONS_EMP <- bls_api("CES2023713001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("CES2023711001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(date) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Power, Water, & Communications Utilities")

OTHR_CONS_EMP <- bls_api("CES2023790001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Other Heavy & Civil Engineering")

#MANUFACTURING EMPLOYMENT

CARS_MANU_EMP <- bls_api("CES3133600101", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Motor Vehicle & Parts Manufacturing")

SEMI_MANU_EMP <- bls_api("CES3133441301", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Semiconductor & Related Device Manufacturing")

ELEC_MANU_EMP <- bls_api("CES3133530001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("CES3133590001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  group_by(date) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Electrical Equipment & Battery Manufacturing")

ELEC_GENR_EMP <- bls_api("CES4422110001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_change_2020 = (value-value[25])/value[25]) %>%
  mutate(raw_change_2020 = (value-value[25])) %>%
  mutate(name = "Electricity Generation, Transmission, & Distribution")

CONS_EMPLOYMENT_GRAPH_PCT <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=OTHR_CONS_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  geom_line(data=UTIL_CONS_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  geom_line(data=INDUS_CONS_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  geom_line(data=HWAY_CONS_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  annotate("vline", x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Inflation\nReduction\n& CHIPS Acts", x = as.Date("2022-09-16"), y = 0.17, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-11-15"), xintercept = as.Date("2021-11-15"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Bipartisan\nInfrastructure\nLaw", x = as.Date("2021-10-15"), y = 0.17, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Percent Growth Since Jan 2020") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.2,-.1,0,.1,.2), limits = c(-.2,.25), expand = c(0,0)) +
  ggtitle("Growth in Key Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Key Construction Subsectors Has Grown Since the Passage of BIF, IRA, & CHIPS") +
  theme_apricitas + theme(legend.position = c(.25,.82), plot.title = element_text(size = 28)) +
  scale_color_manual(name= "% Change Since Jan 2020",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72"), breaks = c("Highways & Streets", "Power, Water, & Communications Utilities", "Industrial Buildings","Other Heavy & Civil Engineering")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.2-(.3*.45), ymax = -.2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONS_EMPLOYMENT_GRAPH_PCT, "CONS EMPLOYMENT PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CONS_EMPLOYMENT_GRAPH_RAW <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=OTHR_CONS_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  geom_line(data=UTIL_CONS_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  geom_line(data=INDUS_CONS_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  geom_line(data=HWAY_CONS_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  annotate("vline", x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Inflation\nReduction\n& CHIPS Acts", x = as.Date("2022-09-16"), y = 43, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-11-15"), xintercept = as.Date("2021-11-15"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Bipartisan\nInfrastructure\nLaw", x = as.Date("2021-10-15"), y = 43, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Change Since Jan 2020") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-30,-20,-10,0,10,20,30,40,50), limits = c(-35,50), expand = c(0,0)) +
  ggtitle("Growth in Key Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Key Construction Subsectors Has Grown Since the Passage of BIF, IRA, & CHIPS") +
  theme_apricitas + theme(legend.position = c(.28,.79), plot.title = element_text(size = 28)) +
  scale_color_manual(name= "Construction Employment\nChange Since Jan 2020",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72"), breaks = c("Highways & Streets", "Power, Water, & Communications Utilities", "Industrial Buildings","Other Heavy & Civil Engineering")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -35-(.3*80), ymax = -35) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONS_EMPLOYMENT_GRAPH_RAW, "CONS EMPLOYMENT RAW Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



MANU_EMPLOYMENT_GRAPH_PCT <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CARS_MANU_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  geom_line(data=SEMI_MANU_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  geom_line(data=ELEC_MANU_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  geom_line(data=ELEC_GENR_EMP, aes(x=date,y= pct_change_2020,color= name), size = 1.25) +
  annotate("vline", x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Inflation\nReduction\n& CHIPS Acts", x = as.Date("2022-09-16"), y = 0.17, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-11-15"), xintercept = as.Date("2021-11-15"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Bipartisan\nInfrastructure\nLaw", x = as.Date("2021-10-15"), y = 0.17, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Percent Growth Since Jan 2020") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.2,-.1,0,.1,.2), limits = c(-.2,.25), expand = c(0,0)) +
  ggtitle("Growth in Key Industry Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Key Manufacturing Subsectors Has Grown Since the Passage of BIF, IRA, & CHIPS") +
  theme_apricitas + theme(legend.position = c(.25,.82), plot.title = element_text(size = 28)) +
  scale_color_manual(name= "% Change Since Jan 2020",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72"), breaks = c("Motor Vehicle & Parts Manufacturing", "Semiconductor & Related Device Manufacturing", "Electrical Equipment & Battery Manufacturing","Electricity Generation, Transmission, & Distribution")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -.2-(.3*.45), ymax = -.2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANU_EMPLOYMENT_GRAPH_PCT, "MANU EMPLOYMENT PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MANU_EMPLOYMENT_GRAPH_RAW <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CARS_MANU_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  geom_line(data=SEMI_MANU_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  geom_line(data=ELEC_MANU_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  geom_line(data=ELEC_GENR_EMP, aes(x=date,y= raw_change_2020,color= name), size = 1.25) +
  annotate("vline", x = as.Date("2022-08-16"), xintercept = as.Date("2022-08-16"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Inflation\nReduction\n& CHIPS Acts", x = as.Date("2022-09-16"), y = 110, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-11-15"), xintercept = as.Date("2021-11-15"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Bipartisan\nInfrastructure\nLaw", x = as.Date("2021-10-15"), y = 110, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("text",label = "NOTE: Early 2020 Vehicle Manufacturing\nJob Losses Cut Off", hjust = 0, x = as.Date("2018-02-01"), y =-25, color = "white", size = 4, alpha = 0.75, lineheight = 0.8) +
  xlab("Date") +
  ylab("Change Since Jan 2020") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-25,0,25,50,75,100,125), limits = c(-35,125), expand = c(0,0)) +
  ggtitle("Growth in Key Industry Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in Key Construction Subsectors Has Grown Since the Passage of BIF, IRA, & CHIPS") +
  theme_apricitas + theme(legend.position = c(.29,.79), plot.title = element_text(size = 28), legend.text = element_text(size = 12.5), legend.title = element_text(size = 14)) +
  scale_color_manual(name= "Employment Change Since Jan 2020",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72"), breaks = c("Motor Vehicle & Parts Manufacturing", "Semiconductor & Related Device Manufacturing", "Electrical Equipment & Battery Manufacturing","Electricity Generation, Transmission, & Distribution")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -35-(.3*160), ymax = -35) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANU_EMPLOYMENT_GRAPH_RAW, "MANU EMPLOYMENT RAW Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


STATE_FIPS_DATA <- tidycensus::fips_codes %>%
  filter(!(state_code %in% c("60","66","69","74","78"))) %>%  # Exclude territories if needed, except Puerto Rico
  distinct(state_code, state_name) %>%
  mutate(FIPS = as.numeric(state_code))

# Function to get BLS data
get_state_data <- function(FIPS, state_name) {
  seriesID <- paste0("SMS", sprintf("%02d", FIPS), "000003000000001")
  
  bls_api(seriesID, startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
    mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")),
           FIPS = FIPS,
           name = state_name) %>%
    arrange(date)
}

# Fetch data for all states
STATE_MANU_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, get_state_data)

# Combine all state data into one data frame
STATE_MANU_EMP_GROWTH <- bind_rows(STATE_MANU_EMP_GROWTH_DATA_LIST) 

STATE_MANU_EMP_GROWTH <- STATE_MANU_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2020 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))
  
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

STATES_MANU_EMP_GROWTH_MAP <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf() %>%
  mutate(name = state_name)

STATES_MANU_EMP_GROWTH_MAP <- left_join(STATES_MANU_EMP_GROWTH_MAP, STATE_MANU_EMP_GROWTH, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(pct_growth_2020, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66 & state_fips != 78) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(STATES_MANU_EMP_GROWTH_MAP, .) %>%
  st_centroid()

states_territories_labls <- get_urbn_labels(map = "territories") %>%
  left_join(STATES_MANU_EMP_GROWTH_MAP, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

STATES_MANU_EMP_GROWTH_MAP_GRAPH <- STATES_MANU_EMP_GROWTH_MAP  %>%
  ggplot(aes(fill = Growth_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = STATES_MANU_EMP_GROWTH_MAP, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1.5%", "1.5-3%", "3-4.5%", "4.5-6%","6%+")) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("PR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("VI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(pct_growth_2020 >= 0, " ", ""), sprintf("%.1f", round(pct_growth_2020 * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle("  Change in Manufacturing Jobs Since Jan 2020") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATES_MANU_EMP_GROWTH_MAP_GRAPH, "States Manu Emp Growth Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

# Function to get BLS QCEW data
get_state_QCEW_data <- function(FIPS, state_name, code) {
  seriesID <- paste0("ENU", sprintf("%02d", FIPS), code)
  data <- bls_api(seriesID, startyear = 2019, registrationKey = Sys.getenv("BLS_KEY"), annualaverage = TRUE)
  if (nrow(data) == 0) {
    return(tibble(date = as.Date(character()), FIPS = integer(), name = character(), value = numeric()))
  }
  
  data %>%
    mutate(date = as.Date(paste0(year,"-01-01")),
           FIPS = FIPS,
           name = state_name) %>%
    arrange(date)
}

#INDUSTRIAL BUILDING CONSTRUCTION

STATE_CONS_INDU_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "00010523621", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_CONS_INDU_EMP_GROWTH <- bind_rows(STATE_CONS_INDU_EMP_GROWTH_DATA_LIST) 

STATE_CONS_INDU_EMP_GROWTH <- STATE_CONS_INDU_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  mutate(name = NAME)

STATE_CONS_INDU_EMP_MAP <- merge(STATE_CONS_INDU_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_CONS_INDU_EMP_MAP_CENTROIDS <- STATE_CONS_INDU_EMP_MAP %>%
  st_centroid()


michigan_index <- which(STATE_CONS_INDU_EMP_MAP_CENTROIDS$NAME == "Michigan")
michigan_centroid <- STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[michigan_index]

florida_index <- which(STATE_CONS_INDU_EMP_MAP_CENTROIDS$NAME == "Florida")
florida_centroid <- STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[florida_index]

california_index <- which(STATE_CONS_INDU_EMP_MAP_CENTROIDS$NAME == "California")
california_centroid <- STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[california_index]

louisiana_index <- which(STATE_CONS_INDU_EMP_MAP_CENTROIDS$NAME == "Louisiana")
louisiana_centroid <- STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[louisiana_index]

hawaii_index <- which(STATE_CONS_INDU_EMP_MAP_CENTROIDS$NAME == "Hawaii")
hawaii_centroid <- STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[hawaii_index]


adjusted_michigan_centroid <- st_geometry(michigan_centroid) + c(60000, -130000)
adjusted_florida_centroid <- st_geometry(florida_centroid) + c(70000, 0)
adjusted_california_centroid <- st_geometry(california_centroid) + c(-60000, 0)
adjusted_louisiana_centroid <- st_geometry(louisiana_centroid) + c(-60000, 0)
adjusted_hawaii_centroid <- st_geometry(hawaii_centroid) + c(120000, -100000)


STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_CONS_INDU_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid



STATE_CONS_INDU_EMP_MAP_PCT_GRAPH <- STATE_CONS_INDU_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_INDU_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_INDU_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth_2019, size = value/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = "2019-2023 Growth, %",breaks = c(-.75,-.5,-.25,0,.25,.5,.75), labels = c("-75%","-50%","-25%","0%","25%","50%","75%")) +
  scale_size_area(name = "2023 Employment",
                  max_size = 17,
                  breaks = c(10,20,30,40),
                  labels = c("10k","20k","30k","40k"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("Industrial Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using Census data.") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_CONS_INDU_EMP_MAP_PCT_GRAPH, "States Industrial Construction Employment Percent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


STATE_CONS_INDU_EMP_MAP_RAW_GRAPH <- STATE_CONS_INDU_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_INDU_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_INDU_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-10,10),
                  breaks = c(5,10),
                  labels = c("5k","10k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("    Change in Industrial Building Construction \n                            Employment, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_CONS_INDU_EMP_MAP_RAW_GRAPH, "States Industrial Construction Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#Highway Construction

STATE_CONS_HWAY_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001052373", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_CONS_HWAY_EMP_GROWTH <- bind_rows(STATE_CONS_HWAY_EMP_GROWTH_DATA_LIST) 

STATE_CONS_HWAY_EMP_GROWTH <- STATE_CONS_HWAY_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  mutate(name = NAME)

STATE_CONS_HWAY_EMP_MAP <- merge(STATE_CONS_HWAY_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_CONS_HWAY_EMP_MAP_CENTROIDS <- STATE_CONS_HWAY_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_CONS_HWAY_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_CONS_HWAY_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_CONS_HWAY_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_CONS_HWAY_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_CONS_HWAY_EMP_MAP_CENTROIDS$NAME == "Hawaii")

STATE_CONS_HWAY_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_CONS_HWAY_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_CONS_HWAY_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_CONS_HWAY_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_CONS_HWAY_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid

STATE_CONS_HWAY_EMP_MAP_PCT_GRAPH <- STATE_CONS_HWAY_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_HWAY_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_HWAY_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth_2019, size = value/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = "2019-2023 Growth, %",breaks = c(-.75,-.5,-.25,0,.25,.5,.75), labels = c("-75%","-50%","-25%","0%","25%","50%","75%")) +
  scale_size_area(name = "2023 Employment",
                  max_size = 17,
                  breaks = c(10,20,30,40),
                  labels = c("10k","20k","30k","40k"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("Roadway Construction Employment by State, 2023") +
  labs(caption = "Graph created by @JosephPolitano using Census data.") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_CONS_HWAY_EMP_MAP_PCT_GRAPH, "States Highway Construction Employment Percent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STATE_CONS_HWAY_EMP_MAP_RAW_GRAPH <- STATE_CONS_HWAY_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_HWAY_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_HWAY_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-10,10),
                  breaks = c(5,10),
                  labels = c("5k","10k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Roadway Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_CONS_HWAY_EMP_MAP_RAW_GRAPH, "States Highway Construction Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



#Utility Construction
#STATE_CONS_UTIL_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001052371", get_state_QCEW_data)

STATE_CONS_UTIL_WATER_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "00010523711", get_state_QCEW_data)
STATE_CONS_UTIL_POWER_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "00010523713", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_CONS_UTIL_EMP_GROWTH <- bind_rows(STATE_CONS_UTIL_WATER_EMP_GROWTH_DATA_LIST) %>%
  rbind(.,bind_rows(STATE_CONS_UTIL_POWER_EMP_GROWTH_DATA_LIST))

STATE_CONS_UTIL_EMP_GROWTH <- STATE_CONS_UTIL_EMP_GROWTH %>%
  group_by(date,name) %>%
  filter(is.na(value) == FALSE) %>%
  filter(n() == 2) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  arrange(name,date) %>%
  ungroup() %>%
  group_by(name) %>%
  filter(min(date) == as.Date("2019-01-01")) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  mutate(name = NAME)

STATE_CONS_UTIL_EMP_MAP <- merge(STATE_CONS_UTIL_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_CONS_UTIL_EMP_MAP_CENTROIDS <- STATE_CONS_UTIL_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_CONS_UTIL_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_CONS_UTIL_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_CONS_UTIL_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_CONS_UTIL_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_CONS_UTIL_EMP_MAP_CENTROIDS$NAME == "Hawaii")


STATE_CONS_UTIL_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_CONS_UTIL_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_CONS_UTIL_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_CONS_UTIL_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_CONS_UTIL_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid

STATE_CONS_UTIL_EMP_MAP_PCT_GRAPH <- STATE_CONS_UTIL_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_UTIL_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_UTIL_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth_2019, size = value/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = "2019-2023 Growth, %",breaks = c(-.75,-.5,-.25,0,.25,.5,.75), labels = c("-75%","-50%","-25%","0%","25%","50%","75%")) +
  scale_size_area(name = "2023 Employment",
                  max_size = 17,
                  breaks = c(10,20,30,40),
                  labels = c("10k","20k","30k","40k"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("Power Water, & Utility Construction Employment by State, 2023") +
  labs(caption = "Graph created by @JosephPolitano using Census data.") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_CONS_UTIL_EMP_MAP_PCT_GRAPH, "States Utility Construction Employment Percent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STATE_CONS_UTIL_EMP_MAP_RAW_GRAPH <- STATE_CONS_UTIL_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_UTIL_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_UTIL_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-10,10),
                  breaks = c(5,10),
                  labels = c("5k","10k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Power, Water, & Communication Utility Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_CONS_UTIL_EMP_MAP_RAW_GRAPH, "States Utility Construction Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Other Construction
STATE_CONS_OTHR_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001052379", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_CONS_OTHR_EMP_GROWTH <- bind_rows(STATE_CONS_OTHR_EMP_GROWTH_DATA_LIST) 

STATE_CONS_OTHR_EMP_GROWTH <- STATE_CONS_OTHR_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  mutate(name = NAME)

STATE_CONS_OTHR_EMP_MAP <- merge(STATE_CONS_OTHR_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_CONS_OTHR_EMP_MAP_CENTROIDS <- STATE_CONS_OTHR_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_CONS_OTHR_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_CONS_OTHR_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_CONS_OTHR_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_CONS_OTHR_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_CONS_OTHR_EMP_MAP_CENTROIDS$NAME == "Hawaii")

STATE_CONS_OTHR_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_CONS_OTHR_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_CONS_OTHR_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_CONS_OTHR_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_CONS_OTHR_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid

STATE_CONS_OTHR_EMP_MAP_PCT_GRAPH <- STATE_CONS_OTHR_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_OTHR_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_OTHR_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = pct_growth_2019, size = value/1000), shape = 21, alpha = 0.6, stroke = NA, show.legend = TRUE) +
  scale_fill_viridis_c(name = "2019-2023 Growth, %",breaks = c(-.75,-.5,-.25,0,.25,.5,.75), labels = c("-75%","-50%","-25%","0%","25%","50%","75%")) +
  scale_size_area(name = "2023 Employment",
                  max_size = 17,
                  breaks = c(10,20,30,40),
                  labels = c("10k","20k","30k","40k"),
                  guide = guide_legend(override.aes = list(fill = c("#FDE725FF"), color = c("#FDE725FF"),stroke = NA))) +
  ggtitle("Other Heavy & Civil Construction Employment by State, 2023") +
  #labs(caption = "Graph created by @JosephPolitano using Census data.") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = STATE_CONS_OTHR_EMP_MAP_PCT_GRAPH, "States Other Construction Employment Percent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

STATE_CONS_OTHR_EMP_MAP_RAW_GRAPH <- STATE_CONS_OTHR_EMP_MAP %>%
  ggplot() +
  geom_sf(fill = "grey60") +
  geom_sf(data = STATE_CONS_OTHR_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_CONS_OTHR_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "2019-2023 Change",
                  max_size = 17,
                  limits = c(-10,10),
                  breaks = c(5,10),
                  labels = c("5k","10k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Other Heavy & Civil Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_CONS_OTHR_EMP_MAP_RAW_GRAPH, "States Other Construction Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ARRANGE_CONS_EMP_PCT <- ggarrange()

ARRANGE_CONS_EMP_RAW <- ggarrange(STATE_CONS_UTIL_EMP_MAP_RAW_GRAPH + ggtitle("           Power, Water, & Communications") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 16, hjust = 0.5)),
                                  STATE_CONS_HWAY_EMP_MAP_RAW_GRAPH + ggtitle("Highways & Streets") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 16, hjust = 0.5)),
                                  STATE_CONS_INDU_EMP_MAP_RAW_GRAPH + ggtitle("Industrial Buildings") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 16, hjust = 0.5)),
                                  STATE_CONS_OTHR_EMP_MAP_RAW_GRAPH + ggtitle("Other Civil Engineering") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 16, hjust = 0.5)), 
                                  common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

tgrob <- text_grob(expression(bold("Construction Employment Change\n  by State & Subsector, 2019-2023")),size = 27, color = "white", hjust = 0.5, lineheight = 0.8) 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0.3,0,-0.3,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(0.35,-0.15,-0.15,-0.15),"cm"))  

FINAL_ARRANGE_CONS_EMP_RAW <- ggarrange(plot_0,ARRANGE_CONS_EMP_RAW, nrow = 2, heights = c(4.5,20), widths = 10)

ggsave(dpi = "retina",plot = FINAL_ARRANGE_CONS_EMP_RAW, "Final Arrange Cons Emp Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Semiconductor Employment
STATE_SEMI_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "000105334413", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_MANU_SEMI_EMP_GROWTH <- bind_rows(STATE_SEMI_EMP_GROWTH_DATA_LIST) 

STATE_MANU_SEMI_EMP_GROWTH <- STATE_MANU_SEMI_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  filter(GEOID <= 56 | GEOID == 72) %>%
  mutate(name = NAME)

STATE_MANU_SEMI_EMP_MAP <- merge(STATE_MANU_SEMI_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_MANU_SEMI_EMP_MAP_CENTROIDS <- STATE_MANU_SEMI_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_MANU_SEMI_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_MANU_SEMI_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_MANU_SEMI_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_MANU_SEMI_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_MANU_SEMI_EMP_MAP_CENTROIDS$NAME == "Hawaii")

STATE_MANU_SEMI_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_MANU_SEMI_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_MANU_SEMI_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_MANU_SEMI_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_MANU_SEMI_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid


STATE_MANU_SEMI_EMP_GROWTH_GRAPH <- STATE_MANU_SEMI_EMP_MAP %>%
  ggplot() +
  geom_sf(data = State_map, fill = "grey60") +
  geom_sf(data = STATE_MANU_SEMI_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_MANU_SEMI_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-15,15),
                  breaks = c(5,10,15),
                  labels = c("5k","10k","15k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("Change in Semiconductor Manufacturing Employment\n                                2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 24.5),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_MANU_SEMI_EMP_GROWTH_GRAPH, "States Semiconductor Manufacturing Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#ELECTRICITY GENERATION 
STATE_ELEC_GENR_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001052211", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_ELEC_GENR_EMP_GROWTH <- bind_rows(STATE_ELEC_GENR_GROWTH_DATA_LIST) 

STATE_ELEC_GENR_EMP_GROWTH <- STATE_ELEC_GENR_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))


State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  filter(GEOID <= 56 | GEOID == 72) %>%
  mutate(name = NAME)

STATE_ELEC_GENR_EMP_MAP <- merge(STATE_ELEC_GENR_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_ELEC_GENR_EMP_MAP_CENTROIDS <- STATE_ELEC_GENR_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_ELEC_GENR_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_ELEC_GENR_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_ELEC_GENR_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_ELEC_GENR_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_ELEC_GENR_EMP_MAP_CENTROIDS$NAME == "Hawaii")

STATE_ELEC_GENR_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_ELEC_GENR_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_ELEC_GENR_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_ELEC_GENR_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_ELEC_GENR_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid


STATE_ELEC_GENR_EMP_GROWTH_GRAPH <- STATE_ELEC_GENR_EMP_MAP %>%
  ggplot() +
  geom_sf(data = State_map, fill = "grey60") +
  geom_sf(data = STATE_ELEC_GENR_EMP_MAP, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_ELEC_GENR_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-15,15),
                  breaks = c(5,10,15),
                  labels = c("5k","10k","15k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("     Change in Electricity Generation, Transmission\n           & Distribution Employment, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 24.5),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_ELEC_GENR_EMP_GROWTH_GRAPH, "States Electricity Generation Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")




STATE_MANU_ELEC_MAIN_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001053353", get_state_QCEW_data)
STATE_MANU_ELEC_OTHR_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001053359", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_MANU_ELEC_EMP_GROWTH <- bind_rows(STATE_MANU_ELEC_MAIN_EMP_GROWTH_DATA_LIST) %>%
  rbind(.,bind_rows(STATE_MANU_ELEC_OTHR_EMP_GROWTH_DATA_LIST))

STATE_MANU_ELEC_EMP_GROWTH <- STATE_MANU_ELEC_EMP_GROWTH %>%
  group_by(date,name) %>%
  filter(is.na(value) == FALSE) %>%
  filter(n() == 2) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  arrange(name,date) %>%
  ungroup() %>%
  group_by(name) %>%
  filter(min(date) == as.Date("2019-01-01")) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  filter(GEOID <= 56 | GEOID == 72) %>%
  mutate(name = NAME)

STATE_MANU_ELEC_EMP_MAP <- merge(STATE_MANU_ELEC_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_MANU_ELEC_EMP_MAP_CENTROIDS <- STATE_MANU_ELEC_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_MANU_ELEC_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_MANU_ELEC_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_MANU_ELEC_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_MANU_ELEC_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_MANU_ELEC_EMP_MAP_CENTROIDS$NAME == "Hawaii")

STATE_MANU_ELEC_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_MANU_ELEC_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_MANU_ELEC_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_MANU_ELEC_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_MANU_ELEC_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid


STATE_MANU_ELEC_EMP_GROWTH_GRAPH <- STATE_MANU_ELEC_EMP_MAP %>%
  ggplot() +
  geom_sf(data = State_map, fill = "grey60") +
  geom_sf(data = State_map, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_MANU_ELEC_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-15,15),
                  breaks = c(5,10,15),
                  labels = c("5k","10k","15k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("           Change in Electrical Equipment & Battery\n          Manufacturing Employment, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 24.5),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_MANU_ELEC_EMP_GROWTH_GRAPH, "States Electric Equipment Manufacturing Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



STATE_MANU_CARS_MAIN_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001053361", get_state_QCEW_data)
STATE_MANU_CARS_BODY_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001053362", get_state_QCEW_data)
STATE_MANU_CARS_PARTS_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001053363", get_state_QCEW_data)


# Combine all state data into one data frame
STATE_MANU_CARS_EMP_GROWTH <- bind_rows(STATE_MANU_CARS_MAIN_EMP_GROWTH_DATA_LIST) %>%
  rbind(.,bind_rows(STATE_MANU_CARS_BODY_EMP_GROWTH_DATA_LIST)) %>%
  rbind(.,bind_rows(STATE_MANU_CARS_PARTS_EMP_GROWTH_DATA_LIST))

STATE_MANU_CARS_EMP_GROWTH <- STATE_MANU_CARS_EMP_GROWTH %>%
  group_by(date,name) %>%
  filter(is.na(value) == FALSE) %>%
  filter(n() == 3) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  arrange(name,date) %>%
  ungroup() %>%
  group_by(name) %>%
  filter(min(date) == as.Date("2019-01-01")) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))

State_map <- states(cb = TRUE) %>%
  shift_geometry(position = "below") %>% #moves Alaska and Hawaii below the map
  filter(GEOID <= 56 | GEOID == 72) %>%
  mutate(name = NAME)

STATE_MANU_CARS_EMP_MAP <- merge(STATE_MANU_CARS_EMP_GROWTH,State_map, by = "name") %>%
  st_as_sf()

STATE_MANU_CARS_EMP_MAP_CENTROIDS <- STATE_MANU_CARS_EMP_MAP %>%
  st_centroid()

michigan_index <- which(STATE_MANU_CARS_EMP_MAP_CENTROIDS$NAME == "Michigan")
florida_index <- which(STATE_MANU_CARS_EMP_MAP_CENTROIDS$NAME == "Florida")
california_index <- which(STATE_MANU_CARS_EMP_MAP_CENTROIDS$NAME == "California")
louisiana_index <- which(STATE_MANU_CARS_EMP_MAP_CENTROIDS$NAME == "Louisiana")
hawaii_index <- which(STATE_MANU_CARS_EMP_MAP_CENTROIDS$NAME == "Hawaii")

STATE_MANU_CARS_EMP_MAP_CENTROIDS$geometry[michigan_index] <- adjusted_michigan_centroid
STATE_MANU_CARS_EMP_MAP_CENTROIDS$geometry[florida_index] <- adjusted_florida_centroid
STATE_MANU_CARS_EMP_MAP_CENTROIDS$geometry[california_index] <- adjusted_california_centroid
STATE_MANU_CARS_EMP_MAP_CENTROIDS$geometry[louisiana_index] <- adjusted_louisiana_centroid
STATE_MANU_CARS_EMP_MAP_CENTROIDS$geometry[hawaii_index] <- adjusted_hawaii_centroid


STATE_MANU_CARS_EMP_GROWTH_GRAPH <- STATE_MANU_CARS_EMP_MAP %>%
  ggplot() +
  geom_sf(data = State_map, fill = "grey60") +
  geom_sf(data = State_map, color = "black", fill = NA, lwd = 0.65) + 
  geom_point(data = STATE_MANU_CARS_EMP_MAP_CENTROIDS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2019 > 0, size = raw_growth_2019/1000), shape = 21, alpha = 0.6, show.legend = TRUE, stroke = NA) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of\nChange",
                  max_size = 17,
                  limits = c(-15,15),
                  breaks = c(5,10,15),
                  labels = c("5k","10k","15k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("              Change in Motor Vehicle & Parts\n          Manufacturing Employment, 2019-2023") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 24.5),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = STATE_MANU_CARS_EMP_GROWTH_GRAPH, "States Vehicle Manufacturing Employment Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ARRANGE_MANU_EMP_RAW <- ggarrange(STATE_MANU_CARS_EMP_GROWTH_GRAPH + ggtitle("Vehicle & Parts Manufacturing") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)),
                                  STATE_MANU_SEMI_EMP_GROWTH_GRAPH + ggtitle("Semiconductor & Related Manufacturing") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)),
                                  STATE_MANU_ELEC_EMP_GROWTH_GRAPH + ggtitle("Electrical & Battery Manufacturing") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)),
                                  STATE_ELEC_GENR_EMP_GROWTH_GRAPH + ggtitle("Electricity Generation & Distribution") + labs(caption = NULL) + theme(plot.margin = unit(c(0,0,0,0), units = "cm"), plot.title = element_text(size = 14, hjust = 0.5)), 
                                  common.legend = TRUE, legend = "right") + bgcolor("#252A32") + border("#252A32")

tgrob <- text_grob(expression(bold("Key Industrial Employment Change\n  by State & Subsector, 2019-2023")),size = 27, color = "white", hjust = 0.5, lineheight = 0.8) 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0.3,0,-0.3,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(0.35,-0.15,-0.15,-0.15),"cm"))  

FINAL_ARRANGE_MANU_EMP_RAW <- ggarrange(plot_0,ARRANGE_MANU_EMP_RAW, nrow = 2, heights = c(4.5,20), widths = 10)

ggsave(dpi = "retina",plot = FINAL_ARRANGE_MANU_EMP_RAW, "Final Arrange Manu Emp Raw Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#MANUFACTURING OPERATION
STATE_BATTERY_EMP_GROWTH_DATA_LIST <- purrr::map2(STATE_FIPS_DATA$FIPS, STATE_FIPS_DATA$state_name, code = "0001052211", get_state_QCEW_data)

# Combine all state data into one data frame
STATE_BATTERY_EMP_GROWTH <- bind_rows(STATE_BATTERY_EMP_GROWTH_DATA_LIST) 

STATE_BATTERY_EMP_GROWTH <- STATE_BATTERY_EMP_GROWTH %>%
  group_by(name) %>%
  mutate(pct_growth_2019 = (value-value[1])/value[1]) %>%
  mutate(raw_growth_2019 = value-value[1]) %>%
  ungroup() %>%
  filter(date == max(date))


#MAYBE DO VEHICLE AND OTHER ELECTRICAL COMPONENTS INSTEAD OF BATTERIES? OR SEPARATE BATTERY INTO ELECTRICAL COMPONENTS



#Utility Operation??

PA_MANU <- bls_api("SMS42000003000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("SMS42000003000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

IL_MANU <- bls_api("SMS17000003000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("SMS17000003000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

OH_MANU <- bls_api("SMS39000003000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("SMS39000003000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

IN_MANU <- bls_api("SMS18000003000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("SMS18000003000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

MI_MANU <- bls_api("SMS26000003000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("SMS26000003000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

WI_MANU <- bls_api("SMS55000003000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("SMS55000003000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

RUST_BELT_MANU <- rbind(PA_MANU,IL_MANU,OH_MANU,IN_MANU,MI_MANU,WI_MANU) %>%
  group_by(date) %>%
  summarize(value = sum(value, na.rm = TRUE))
  
RUST_BELT_MANU_JOBS <- ggplot() + 
  geom_line(data=RUST_BELT_MANU, aes(x=date,y= value/1000,color= "All Employees, Manufacturing, Rust Belt"), size = 1.25) + 
  xlab("Date") +
  ylab("Number of Employees") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3,4,5), limits = c(0,5), expand = c(0,0)) +
  ggtitle("Rust Belt Manufacturing Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data. NOTE: Rust Belt Defined as PA/OH/IN/IL/MI/WI", subtitle = "Rust Belt Manufacturing Employment Still Hasn't Recovered to Pre-COVID or Pre-08 Levels") +
  theme_apricitas + theme(legend.position = c(.70,.87), plot.title = element_text(size = 28)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUST_BELT_MANU_JOBS, "Rust Belt Manu Jobs.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

ALL_MANU <- bls_api("CES3000000001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("CES3000000001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

RUST_BELT_MANU_PCT <- merge(RUST_BELT_MANU,ALL_MANU, by = "date") %>%
  transmute(date, value = value.x/value.y)

RUST_BELT_MANU_JOBS_PCT <- ggplot() + 
  geom_line(data=RUST_BELT_MANU_PCT, aes(x=date,y= value,color= "Rust Belt Share of All US Manufacturing Employees"), size = 1.25) + 
  xlab("Date") +
  ylab("Number of Employees") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(.25,.26,.27,.28,.29,.30), limits = c(.25,.29), expand = c(0,0)) +
  ggtitle("Rust Belt Manufacturing Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data. NOTE: Rust Belt Defined as PA/OH/IN/IL/MI/WI", subtitle = "The Rust Belt is at its Lowest Share of Nationwide Manufacturing Employment Outside Recessions") +
  theme_apricitas + theme(legend.position = c(.60,.95), plot.title = element_text(size = 28)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = .25-(.3*0.04), ymax = .25) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RUST_BELT_MANU_JOBS_PCT, "Rust Belt Manu Jobs PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Arizona and Texas Industrial Construction and Employment

#STATE NONRES CONSTRUCTION AND MANUFACTURING GDP GROWTH

#HIGHWAY SPENDING BY REGION

#RUST BELT MANU JOBS

AZ_INDU_CONS <- bls_api()
TX_INDU_CONS <- bls_api()

AZ_ELEC_MANU <- bls_api()
TX_ELE_MANU <- bls_api()

#transmission and util employment

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
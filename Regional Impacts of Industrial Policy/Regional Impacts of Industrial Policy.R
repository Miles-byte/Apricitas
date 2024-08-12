pacman::p_load(prismatic,maps,tigris,sf,maps,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

STATE_PRIVATE_CONSTRUCTION_BULK <- read.xlsx("https://www.census.gov/construction/c30/xlsx/nrstate.xlsx") 

STATE_PRIVATE_CONSTRUCTION <- STATE_PRIVATE_CONSTRUCTION_BULK %>%
  slice(-1) %>%
  select(-`Annual.Value.of.Private.Nonresidential.Construction.Put.in.Place.by.State,.2008-2023`, -X19) %>%
  row_to_names(1) %>%
  rename(NAME = 1) %>%
  drop_na() %>%
  mutate(across(`2008`:`2023`, as.numeric)) %>%
  mutate(`2019-2023` = (`2023`-`2019`)/`2019`, `2021-2023` = (`2023`-`2021`)/`2021`)

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
  ggtitle(paste("2023 Nonresidential Construction\nPer Capita, Top 25 States")) +
  scale_fill_manual(name= "2020 Presidential Election Category",values = c("#9A348E","#EE6055","#3083DC"), breaks = c("Swing","Republican","Democrat")) +
  labs(caption = "Graph created by @JosephPolitano using Census Data\nNOTE: Swing Includes States With a <3% Margin of Victory") +
  theme_apricitas + theme(legend.position = c(.75,.75), axis.text.y = element_text(size = 14), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = STATE_PRIVATE_CONSTRUCTION_PER_CAPITA_GRAPH, "States Private Nonresidential Construction Per Capita Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


STATE_PRIVATE_CONSTRUCTION_LINEAR <- STATE_PRIVATE_CONSTRUCTION %>%
  select(-`2019-2023`,-`2021-2023`) %>%
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

#PUBLIC SECTOR CONSTRUCTION DATA
#HIGHWAY SPENDING BY REGION
#NONRES CONSTRUCTION PER CAPITA
#DON'T FORGET EIA MAPS
  
p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
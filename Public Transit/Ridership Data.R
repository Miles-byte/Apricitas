pacman::p_load(ntdr,pacman,ggrepel,dots,ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
pacman::p_load(purrr,curl,rlang)

p_unload(ntdr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#https://www.transit.dot.gov/ntd/data-product/2022-ntd-database-file-dictionary
#https://www.bts.gov/national-transit-map
#https://www.transit.dot.gov/ntd/ntd-data

#MANUAL OPERATIONS FOR WHEN THE API IS BROKEN
library(readxl)

UPT_manual <- read_excel(
  "C:/Users/Josep/Documents/GitHub/Apricitas/Public Transit/UPT_MANUAL.xlsx"
)

monthly_cols <- names(UPT_manual)[grepl("^[0-9]{1,2}/[0-9]{4}$", names(UPT_manual))]

UPT_manual_long <- UPT_manual |>
  pivot_longer(
    cols      = all_of(monthly_cols),
    names_to  = "month",
    values_to = "value"
  ) |>
  mutate(
    month        = as.Date(parse_date_time(month, orders = "m/Y")),
  )

NTD_BULK <- UPT_manual_long %>%
  mutate(modes_simplified = `3 Mode`) %>%
  mutate(modes = Mode) %>%
  mutate(agency = Agency) %>%
  mutate(uza_name = `UZA Name`)


NTD_BULK <- get_ntd(data_type = "adjusted",
                    ntd_variable = "UPT",
                    modes = "all",
                    cache = FALSE)

#BULK DOWNLOAD BROKEN 
#https://github.com/vgXhc/ntdr/issues/22
#https://dev.socrata.com/foundry/datahub.transportation.gov/8bui-9xvu


TOTAL_BULK <- NTD_BULK %>%
  group_by(month) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup()

TOTAL_TRANSIT_RIDERSHIP_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(TOTAL_BULK, month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000000,color="Rolling 12M Total"), size = 1.25) +
  geom_line(data=filter(TOTAL_BULK, month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000000*12,color="Monthly, Annualized"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "B"),limits = c(0,12), expand = c(0,0), breaks = c(0,2,4,6,8,10,12)) +
  ylab("Unlinked Passenger Trips") +
  ggtitle("Total Ridership, All US Public Transit") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Bus, Heavy Rail, Light Rail, Commuter Rail, Monorail, etc, But not Amtrak",subtitle = "US Public Transit Ridership Continues To Recover From the Effects of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.80,.85), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Ridership, Unlinked Passenger Trips",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*12), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TOTAL_TRANSIT_RIDERSHIP_graph, "Total Transit Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


RAIL_BULK <- NTD_BULK %>%
  filter(modes_simplified == "Rail" & modes != "CR") %>%
  mutate(agency = gsub("San Diego Trolley, Inc.","San Diego Metropolitan Transit System", agency)) %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup() #%>%
  #filter(month == max(month))

# Join the latest and previous rankings for Rail Networks
RAIL_RANKINGS_LATEST_PREVIOUS <- RAIL_BULK %>%
  filter(month == max(month)) %>%
  arrange(desc(year_roll)) %>%
  mutate(rank_latest = row_number()) %>%
  inner_join(RAIL_BULK %>%
               filter(month < max(month)) %>%
               filter(month == max(month)) %>%
               arrange(desc(year_roll)) %>%
               mutate(rank_previous = row_number()), by = "agency") %>%
  select(agency, rank_latest, rank_previous)

# Function to determine the agencies passed by other agencies
find_passed_agencies <- function(current_agency, comparison) {
  current_rank_previous <- comparison$rank_previous[comparison$agency == current_agency]
  current_rank_latest <- comparison$rank_latest[comparison$agency == current_agency]
  
  passed <- comparison %>%
    filter(rank_previous < current_rank_previous & rank_latest >= current_rank_latest) %>%
    pull(agency)
  
  return(passed)
}

# Determine which rail networks  passed others
RESULTS_RAIL <- RAIL_RANKINGS_LATEST_PREVIOUS %>%
  filter(rank_latest < rank_previous) %>%
  rowwise() %>%
  mutate(passed_agency = list(find_passed_agencies(agency, RAIL_RANKINGS_LATEST_PREVIOUS))) %>%
  filter(length(passed_agency) > 0) %>%
  mutate(passed_agency = paste(passed_agency, collapse = ", ")) %>%
  mutate(message = paste(agency, "passed", passed_agency,"to become rank",rank_latest)) %>%
  pull(message) %>%
  walk(print)

#Filtering to see the X largest networks
LARGE_RAIL <- ggplot() + 
  geom_line(data = RAIL_BULK %>%
              filter(agency %in% (RAIL_BULK %>%
                                    filter(month == as.Date("2019-12-01")) %>%
                                    arrange(desc(year_roll)) %>%
                                    slice(8) %>%
                                    pull(agency))), aes(x=month,y=year_roll,color = agency), size = 1.25)

BART_PLUS_MUNI <- RAIL_BULK %>%
  filter(agency %in% c("San Francisco Bay Area Rapid Transit District", "City and County of San Francisco")) %>%
  group_by(month) %>%
  summarise(across(value:three_month_roll, sum, na.rm = TRUE), .groups = 'drop') %>%
  mutate(agency = "BART + MUNI (SF)")

SECOND_FIFTH_NETWORKS_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(RAIL_BULK, agency == "Chicago Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="BART (Bay Area)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MBTA (Boston)"), size = 1.25) +
  #geom_line(data=filter(BART_PLUS_MUNI, agency == "BART + MUNI (SF)", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="BART + Muni (Bay Area)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="WMATA (DC)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="SEPTA (Philly)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="LA Metro (LA)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,300), expand = c(0,0), breaks = c(0,100,200,300)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Ridership, US 2nd-5th Largest Urban Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail. 2nd-5th Systems Selected Based on 2019 Ridership Rankings",subtitle = "DC Metro's Ridership Recovery has Vastly Exceeded Comparable US Transit Systems") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("WMATA (DC)","CTA (Chicago)","MBTA (Boston)","BART (Bay Area)","BART + Muni (Bay Area)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*300), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SECOND_FIFTH_NETWORKS_graph, "2nd to 5th Largest Rail Networks.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIRST_FIVE_RIDERSHIP_RECOVERY_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(RAIL_BULK, agency == "Chicago Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="BART (Bay Area)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MBTA (Boston)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="WMATA (DC)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "MTA New York City Transit", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MTA (NYC)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="LA Metro (LA)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="SEPTA (Philly)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Port Authority Trans-Hudson Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="PATH (NY/NJ)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="Muni (SF)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Atlanta Rapid Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MARTA (Atlanta)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1.25), expand = c(0,0), breaks = c(0,.25,.5,.75,1,1.25)) +
  ylab("Percent of 2019 Ridership") +
  ggtitle("Ridership Recovery, US 5 Largest Urban Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail. 1st-5th Systems Selected Based on 2019 Ridership Rankings",subtitle = "NYC's Subway Has Led the Post-COVID Ridership Recovery") +
  theme_apricitas + theme(legend.position = c(.4,.475), plot.title = element_text(size = 21)) +
  scale_color_manual(name= "Ridership, % of 2019\nRolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("MTA (NYC)","WMATA (DC)","CTA (Chicago)","MBTA (Boston)","BART (Bay Area)","SEPTA (Philly)","LA Metro (LA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIRST_FIVE_RIDERSHIP_RECOVERY_graph, "1st Five Ridership Recovery Percent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIRST_SEVEN_RIDERSHIP_RECOVERY_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(RAIL_BULK, agency == "Chicago Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="BART (Bay Area)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MBTA (Boston)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="WMATA (DC)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "MTA New York City Transit", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MTA (NYC)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="SEPTA (Philly)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Port Authority Trans-Hudson Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="PATH (NY/NJ)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="Muni (SF)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Atlanta Rapid Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MARTA (Atlanta)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1.25), expand = c(0,0), breaks = c(0,.25,.5,.75,1,1.25)) +
  ylab("Percent of 2019 Ridership") +
  ggtitle("Ridership Recovery, US 7 Largest Urban Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail. 1st-5th Systems Selected Based on 2019 Ridership Rankings",subtitle = "NYC's Subway Has Led the Post-COVID Ridership Recovery") +
  theme_apricitas + theme(legend.position = c(.4,.425), plot.title = element_text(size = 21)) +
  scale_color_manual(name= "Ridership, % of 2019\nRolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("MTA (NYC)","WMATA (DC)","CTA (Chicago)","MBTA (Boston)","BART (Bay Area)","SEPTA (Philly)","LA Metro (LA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*1.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIRST_SEVEN_RIDERSHIP_RECOVERY_graph, "1st Seven Ridership Recovery Percent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing




SIXTH_TENTH_NETWORKS_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(RAIL_BULK, agency == "Port Authority Trans-Hudson Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="PATH (NYC/NJ)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Atlanta Rapid Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MARTA (Atlanta)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="SEPTA (Philly)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Muni (SF)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,150), expand = c(0,0), breaks = c(50,100,150)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Ridership, US 6th-10th Largest Urban Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail. 6th-10th Systems Selected Based on 2019 Ridership Rankings") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("SEPTA (Philly)","LA Metro (LA)","PATH (NYC/NJ)","MARTA (Atlanta)","Muni (SF)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SIXTH_TENTH_NETWORKS_graph, "6th to 10th Largest Rail Networks.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ELEVENTH_FIFTEENTH_NETWORKS_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Sound Transit (Seattle)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "San Diego Metropolitan Transit System", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MTS (San Diego)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Tri-County Metropolitan Transportation District of Oregon", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Tri-Met (Portland)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Dallas Area Rapid Transit", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="DART (Dallas)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "County of Miami-Dade", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MDT (Miami)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,65), expand = c(0,0), breaks = c(0,20,40,60)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Ridership, US 11th-15th Largest Urban Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail. 11th-15th Systems Selected Based on 2019 Ridership Rankings") +
  theme_apricitas + theme(legend.position = c(.775,.75), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("MTS (San Diego)","Sound Transit (Seattle)","Tri-Met (Portland)","DART (Dallas)","MDT (Miami)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*65), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ELEVENTH_FIFTEENTH_NETWORKS_graph, "11th to 15th Largest Rail Networks.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

SIXTEENTH_TWENTIETH_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Denver Regional Transportation District", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="RTD (Denver)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Metro Transit", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="METRO (Minneapolis-St. Paul)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Transit Authority of Harris County, Texas", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="METRORail (Houston)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "New Jersey Transit Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="NJTransit (NJ, Light Rail Only)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Utah Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="UTA (Salt Lake City)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,37.5), expand = c(0,0), breaks = c(0,10,20,30)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Ridership, US 16th-20th Largest Urban Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail. 16th-20th Systems Selected Based on 2019 Ridership Rankings") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("NJTransit (NJ, Light Rail Only)","METRO (Minneapolis-St. Paul)","METRORail (Houston)","RTD (Denver)","UTA (Salt Lake City)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*37.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SIXTEENTH_TWENTIETH_graph, "16th to 20th Largest Rail Networks.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

MARTA_BAD_graph <- ggplot() +
  geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Seattle Link"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "San Diego Metropolitan Transit System", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MTS (San Diego)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Tri-County Metropolitan Transportation District of Oregon", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Tri-Met (Portland)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Muni (SF)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Atlanta Rapid Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MARTA Rail"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,80), expand = c(0,0), breaks = c(0,20,40,60,80)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("The Seattle Link has Passed MARTA in Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail",subtitle = "MARTA's Ridership has Fallen Below the San Diego Trolley and SF Muni") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("MARTA Rail","Seattle Link","MTS (San Diego)","Muni (SF)","Sound Transit (Seattle)","Tri-Met (Portland)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*80), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MARTA_BAD_graph, "MARTA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

LIGHT_RAIL_graph <- ggplot() +
  geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Sound Transit (Seattle)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "San Diego Metropolitan Transit System", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MTS (San Diego)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Tri-County Metropolitan Transportation District of Oregon", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Tri-Met (Portland)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Muni (SF)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Dallas Area Rapid Transit", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="DART (Dallas)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Valley Metro Rail, Inc.", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Valley Metro Rail (Phoenix)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,80), expand = c(0,0), breaks = c(0,20,40,60,80)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Ridership, Largest US Light-Rail-Only Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Heavy Rail, Light Rail, etc but Not Commuter Rail",subtitle = "The San Diego Trolley is America's Busiest Light-Rail-Only Transit Network") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 25)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("MTS (San Diego)","Muni (SF)","Sound Transit (Seattle)","Tri-Met (Portland)","DART (Dallas)","Valley Metro Rail (Phoenix)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*80), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LIGHT_RAIL_graph, "Light Rail Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


SKYLINE_graph <- ggplot() +
  geom_line(data=filter(RAIL_BULK, agency == "City and County of Honolulu", month >= as.Date("2023-01-01")), aes(x=month,y= value*12/1000000,color="Honolulu Skyline Ridership\n(Monthly, Annualized)"), size = 1.25) +
  annotate("text", label = "Skyline\nOpens", x = as.Date("2023-05-25"), y = 1.5, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2023-06-01"), xintercept = as.Date("2023-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Kahauiki\nExtension", x = as.Date("2025-09-25"), y = 1.5, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-10-01"), xintercept = as.Date("2025-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Ka‘ākaukukui\nExtension", x = as.Date("2031-05-25"), y = 1.5, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2031-06-01"), xintercept = as.Date("2031-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,2), expand = c(0,0), breaks = c(0,1,2)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Honolulu Skyline: America's Newest Metro") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "Honolulu's Skyline is America's Newest Heavy Rail Transit Network") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = 0-(.3*2), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SKYLINE_graph, "Skyline Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


SOUND_TRANSIT_graph <- ggplot() + 
  #geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2005-01-01")), aes(x=month,y= value/1000000*12,color="Sound Transit\nRail Ridership"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2006-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  #annotate("vline", x = as.Date("2003-08-01"), xintercept = as.Date("2003-08-01"), color = "white", size = 1, linetype = "dashed") +
  #annotate("text", label = "Tacoma\nT-Line\nOpens", x = as.Date("2003-05-01"), y = 30, color = "white", size = 5, hjust = 1, lineheight = 0.8) +
  annotate("vline", x = as.Date("2009-07-01"), xintercept = as.Date("2009-07-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Link\n1-Line\nOpens", x = as.Date("2009-09-01"), y = 50, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-03-01"), xintercept = as.Date("2016-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "University\n1-Line\nExtension", x = as.Date("2016-01-01"), y = 50, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-09-01"), xintercept = as.Date("2016-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Angle Lake\n1-Line\nExtension", x = as.Date("2016-11-01"), y = 50, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-10-01"), xintercept = as.Date("2021-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northgate\n1-Line\nExtension", x = as.Date("2021-08-01"), y = 50, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-04-01"), xintercept = as.Date("2024-04-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Link\n2-Line\nOpens", x = as.Date("2024-02-01"), y = 50, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-09-01"), xintercept = as.Date("2024-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Lynnwood\n1-Line\nExtension", x = as.Date("2024-11-01"), y = 50, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2025-05-01"), xend = as.Date("2025-05-01"), y = 0, yend = 45, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "Redmond\n2-Line\nExtension", x = as.Date("2025-07-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2025-09-01"), xintercept = as.Date("2025-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "1 & 2\nLines\nConnected", x = as.Date("2025-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-09-01"), xintercept = as.Date("2026-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Federal Way\n1-Line\nExtension", x = as.Date("2026-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,60), expand = c(0,0), breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Seattle Link Light Rail Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes T-Line and Link but not Sounder Commuter Rail",subtitle = "Seattle's Link is One of America's Fastest-Growing Rail Transit Systems") +
  theme_apricitas + theme(legend.position = c(.1,.94), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*60), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SOUND_TRANSIT_graph, "Sound Transit Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


SOUND_TRANSIT_MONTHLY_graph <- ggplot() + 
  #geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2005-01-01")), aes(x=month,y= value/1000000*12,color="Sound Transit\nRail Ridership"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2006-01-01")), aes(x=month,y= value*12/1000000,color="Link Ridership,\nAnnualized"), size = 1.25) +
  #annotate("vline", x = as.Date("2003-08-01"), xintercept = as.Date("2003-08-01"), color = "white", size = 1, linetype = "dashed") +
  #annotate("text", label = "Tacoma\nT-Line\nOpens", x = as.Date("2003-05-01"), y = 30, color = "white", size = 5, hjust = 1, lineheight = 0.8) +
  annotate("vline", x = as.Date("2009-07-01"), xintercept = as.Date("2009-07-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Link\n1-Line\nOpens", x = as.Date("2009-05-01"), y = 41, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-03-01"), xintercept = as.Date("2016-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "University\n1-Line\nExtension", x = as.Date("2016-01-01"), y = 41, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-09-01"), xintercept = as.Date("2016-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Angle Lake\n1-Line\nExtension", x = as.Date("2016-11-01"), y = 41, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-10-01"), xintercept = as.Date("2021-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northgate\n1-Line\nExtension", x = as.Date("2021-08-01"), y = 41, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-04-01"), xintercept = as.Date("2024-04-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Link\n2-Line\nOpens", x = as.Date("2024-02-01"), y = 41, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-09-01"), xintercept = as.Date("2024-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Lynnwood\n1-Line\nExtension", x = as.Date("2024-11-01"), y = 41, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2025-05-01"), xend = as.Date("2025-05-01"), y = 0, yend = 35, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "Redmond\n2-Line\nExtension", x = as.Date("2025-07-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  #annotate("text", label = "1 & 2\nLines\nConnected", x = as.Date("2025-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-09-01"), xintercept = as.Date("2026-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Federal Way\n1-Line\nExtension", x = as.Date("2026-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2025-12-01"), xend = as.Date("2025-12-01"), y = 35, yend = 45, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "Federal Way\n1-Line\nExtension", x = as.Date("2026-02-01"), y = 41, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,60), expand = c(0,0), breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
  scale_x_date(limits = c(as.Date("2006-01-01"), as.Date("2026-12-01")),breaks = seq(as.Date("2006-01-01"), as.Date("2026-01-01"), by = "2 years"),labels = scales::date_format("%Y")) +
  #annotate("vline", x = as.Date("2025-09-01"), xintercept = as.Date("2025-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "1 & 2\nLines\nConnected", x = as.Date("2025-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2025-09-01"), xintercept = as.Date("2025-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Redmond\n2-Line\nExtension", x = as.Date("2025-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-09-01"), xintercept = as.Date("2026-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Federal Way\n1-Line\nExtension", x = as.Date("2026-11-01"), y = 31, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Seattle Link Light Rail Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes T-Line and Link but not Sounder Commuter Rail",subtitle = "Seattle's Link is One of America's Fastest-Growing Rail Transit Systems") +
  theme_apricitas + theme(legend.position = c(.33,.95), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*50), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SOUND_TRANSIT_MONTHLY_graph, "Sound Transit Monthly Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


#GRAPH OF LA Metro Growth
LA_METRO_graph <- ggplot() + 
  #geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2005-01-01")), aes(x=month,y= value/1000000*12,color="Sound Transit\nRail Ridership"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(RAIL_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  #annotate("vline", x = as.Date("2003-08-01"), xintercept = as.Date("2003-08-01"), color = "white", size = 1, linetype = "dashed") +
  #annotate("text", label = "Tacoma\nT-Line\nOpens", x = as.Date("2003-05-01"), y = 30, color = "white", size = 5, hjust = 1, lineheight = 0.8) +
  annotate("vline", x = as.Date("2009-07-01"), xintercept = as.Date("2003-07-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Gold Line\n(Now A)\nOpens", x = as.Date("2003-05-01"), y = 130, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2009-12-01"), xintercept = as.Date("2009-12-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Gold (A/E)\nEastside\nExtension", x = as.Date("2009-10-01"), y = 130, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2012-05-01"), xintercept = as.Date("2012-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Expo Line\n(Now E)\nOpens", x = as.Date("2012-3-01"), y = 130, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-03-01"), xintercept = as.Date("2016-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Gold Line (A)\nFoothill\nExtension 2A", x = as.Date("2016-01-01"), y = 130, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-10-01"), xintercept = as.Date("2016-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Expo Line (E)\nPhase 2\nExtension", x = as.Date("2016-07-01"), y = 130, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2022-10-01"), xintercept = as.Date("2022-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "K\nLine\nOpens", x = as.Date("2022-08-01"), y = 130, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2023-06-01"), xintercept = as.Date("2023-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Regional\nConnector\nOpens (A/E)", x = as.Date("2023-08-01"), y = 130, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2025-06-01"), xend = as.Date("2025-06-01"), y = 0, yend = 115, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "K/C Line\nLAX\nExtension", x = as.Date("2025-08-01"), y = 105, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "segment", x = as.Date("2025-09-01"), xend = as.Date("2025-09-01"), y = 0, yend = 90, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "A Line\nPomona\nExtension", x = as.Date("2025-11-01"), y = 80, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  # annotate(geom = "segment", x = as.Date("2025-12-01"), xend = as.Date("2025-12-01"), y = 0, yend = 100, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate("text", label = "D Line\nPhase-1\nExtension", x = as.Date("2026-02-01"), y = 100, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  # annotate(geom = "segment", x = as.Date("2027-12-01"), xend = as.Date("2027-12-01"), y = 0, yend = 105, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate("text", label = "D Line\nPhase-2\nExtension", x = as.Date("2028-02-01"), y = 100, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  # annotate(geom = "segment", x = as.Date("2026-12-01"), xend = as.Date("2026-12-01"), y = 0, yend = 105, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate("text", label = "A Line\nMontclair\nExtension", x = as.Date("2027-02-01"), y = 100, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  # annotate(geom = "segment", x = as.Date("2027-12-01"), xend = as.Date("2027-12-01"), y = 0, yend = 105, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  # annotate("text", label = "D Line\nPhase-3\nExtension", x = as.Date("2028-02-01"), y = 100, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,165), expand = c(0,0), breaks = c(0,25,50,75,100,125,150)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("LA Metro Rail Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail and Heavy Rail Lines",subtitle = "LA Metro's Rail Network is Currently Undergoing Massive Expansion Projects") +
  theme_apricitas + theme(legend.position = c(.22,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*165), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LA_METRO_graph, "LA Metro Rail Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

SAN_DIEGO_TROLLEY <- RAIL_BULK %>%
  filter(agency %in% c("San Diego Metropolitan Transit System", "San Diego Trolley, Inc.")) %>%
  group_by(month) %>%
  summarise(across(value:three_month_roll, sum, na.rm = TRUE), .groups = 'drop') %>%
  mutate(agency = "San Diego Metropolitan Transit System")


SAN_DIEGO_TROLLEY_graph <- ggplot() + 
  #geom_line(data=filter(RAIL_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2005-01-01")), aes(x=month,y= value/1000000*12,color="Sound Transit\nRail Ridership"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(SAN_DIEGO_TROLLEY, agency == "San Diego Metropolitan Transit System", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  #annotate("vline", x = as.Date("2003-08-01"), xintercept = as.Date("2003-08-01"), color = "white", size = 1, linetype = "dashed") +
  #annotate("text", label = "Tacoma\nT-Line\nOpens", x = as.Date("2003-05-01"), y = 30, color = "white", size = 5, hjust = 1, lineheight = 0.8) +
  annotate("vline", x = as.Date("2005-07-01"), xintercept = as.Date("2005-07-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Green Line\nOpens", x = as.Date("2005-05-01"), y = 42, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("rect", xmin = as.Date("2010-01-01"), xmax = as.Date("2015-01-01"), ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.4) +
  annotate("text", label = "Trolley Renewal\nProject", x = as.Date("2009-10-01"), y = 42, color = "#EE6055", size = 4, hjust = 1, alpha = 0.75,lineheight = 0.8) +
  #annotate("vline", x = as.Date("2012-09-01"), xintercept = as.Date("2012-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "System-Wide\nService Reconfiguration", x = as.Date("2012-07-01"), y = 42, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2021-11-01"), xintercept = as.Date("2021-11-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Blue Line\nUCSD Extension", x = as.Date("2021-09-01"), y = 42, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,52.5), expand = c(0,0), breaks = c(0,15,30,45)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("San Diego Trolley Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "The San Diego Trolley is Now America's Busiest Light-Rail-Only Train Network") +
  theme_apricitas + theme(legend.position = c(.25,.95), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*52.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SAN_DIEGO_TROLLEY_graph, "San Diego Trolley Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


VALLEY_METRO_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Valley Metro Rail, Inc.", month >= as.Date("2008-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  annotate("vline", x = as.Date("2008-12-01"), xintercept = as.Date("2008-12-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Metro\nRail\nOpens", x = as.Date("2008-10-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2015-08-01"), xintercept = as.Date("2015-08-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Central\nMesa\nExtension", x = as.Date("2015-06-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-03-01"), xintercept = as.Date("2016-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northwest\nExtension\nPhase I", x = as.Date("2016-5-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2019-05-01"), xintercept = as.Date("2019-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Gilbert\nRoad\nExtension", x = as.Date("2019-07-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2022-05-01"), xintercept = as.Date("2022-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Tempe\nStreetcar\nOpens", x = as.Date("2022-03-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-01-01"), xintercept = as.Date("2024-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northwest\nExtension\nPhase II", x = as.Date("2023-12-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-06-01"), xintercept = as.Date("2025-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "B-Line\n(South PHX)\nOpens", x = as.Date("2025-08-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,25), expand = c(0,0), breaks = c(0,5,10,15,20,25)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Valley Metro Rail (Phoenix) Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail and Streetcar Lines",subtitle = "Phoenix Light Rail is Currently Undergoing Significant Expansion Projects") +
  theme_apricitas + theme(legend.position = c(.22,.95), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2008-01-01")-(.1861*(today()-as.Date("2008-01-01"))), xmax = as.Date("2008-01-01")-(0.049*(today()-as.Date("2008-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VALLEY_METRO_graph, "Valley Metro Rail Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


VALLEY_METRO_MONTH_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Valley Metro Rail, Inc.", month >= as.Date("2008-01-01")), aes(x=month,y= year_roll/1000000,color="Valley Metro Ridership"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Valley Metro Rail, Inc.", month >= as.Date("2008-01-01")), aes(x=month,y= value*12/1000000,color="Valley Metro Ridership"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  annotate(geom = "segment", x = as.Date("2008-12-01"), xend = as.Date("2008-12-01"), y = 0, yend = 15, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "Metro\nRail\nOpens", x = as.Date("2008-10-01"), y = 10, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2015-08-01"), xintercept = as.Date("2015-08-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Central\nMesa\nExtension", x = as.Date("2015-06-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-03-01"), xintercept = as.Date("2016-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northwest\nExtension\nPhase I", x = as.Date("2016-5-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2019-05-01"), xintercept = as.Date("2019-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Gilbert\nRoad\nExtension", x = as.Date("2019-07-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2022-05-01"), xintercept = as.Date("2022-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Tempe\nStreetcar\nOpens", x = as.Date("2022-03-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-01-01"), xintercept = as.Date("2024-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northwest\nExtension\nPhase II", x = as.Date("2023-12-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-06-01"), xintercept = as.Date("2025-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "B-Line\n(South PHX)\nOpens", x = as.Date("2025-08-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,25), expand = c(0,0), breaks = c(0,5,10,15,20,25)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Valley Metro Rail (Phoenix) Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail and Streetcar Lines",subtitle = "Phoenix Light Rail is Currently Undergoing Significant Expansion Projects") +
  theme_apricitas + theme(legend.position = c(.18,.875), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Solid = 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2008-01-01")-(.1861*(today()-as.Date("2008-01-01"))), xmax = as.Date("2008-01-01")-(0.049*(today()-as.Date("2008-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VALLEY_METRO_MONTH_graph, "Valley Metro Rail Monthly Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



METRO_TRANSIT_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Metro Transit", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  annotate("vline", x = as.Date("2004-06-01"), xintercept = as.Date("2004-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "METRO\nRail\nOpens", x = as.Date("2004-04-01"), y = 27.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2004-12-01"), xintercept = as.Date("2004-12-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Mall of\nAmerica\nExtension", x = as.Date("2005-02-01"), y = 27.5, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2009-11-01"), xintercept = as.Date("2009-11-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Target\nField\nExtension", x = as.Date("2009-09-01"), y = 27.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2014-06-01"), xintercept = as.Date("2014-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Green\nLine\nOpens", x = as.Date("2014-04-01"), y = 27.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2027-05-01"), xintercept = as.Date("2027-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "SouthWest\nGreen Line\nExtension", x = as.Date("2027-03-01"), y = 30, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,35), expand = c(0,0), breaks = c(0,5,10,15,20,25,30,35)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("METRO Rail (Minneapolis-St. Paul) Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail Lines",subtitle = "Minneapolis Light Rail Ridership Continues to Recover from the Pandemic") +
  theme_apricitas + theme(legend.position = c(.22,.95), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*35), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = METRO_TRANSIT_graph, "METRO Transit Minneapolis Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


KC_STREETCAR_RIDERSHIP_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Kansas City, City of Missouri", month >= as.Date("2015-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  annotate("vline", x = as.Date("2016-04-01"), xintercept = as.Date("2016-04-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Streetcar\nOpens", x = as.Date("2016-03-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-10-01"), xintercept = as.Date("2025-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Main Street\nExtension", x = as.Date("2025-09-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-10-01"), xintercept = as.Date("2026-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Riverfront\nExtension", x = as.Date("2026-09-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,2.5), expand = c(0,0), breaks = c(0,1,2,3,4,5,6,7)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Kansas City Streetcar Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail Lines",subtitle = "Kansas City is Extending its Streetcar Line to the UMKC Campus") +
  theme_apricitas + theme(legend.position = c(.27,.925), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*2.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = KC_STREETCAR_RIDERSHIP_graph, "KC Streetcar Ridership graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

KC_STREETCAR_RIDERSHIP_MONTHLY_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Kansas City, City of Missouri", month >= as.Date("2015-01-01")), aes(x=month,y= year_roll/1000000,color="Kansas City Streetcar Ridership"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Kansas City, City of Missouri", month >= as.Date("2015-01-01")), aes(x=month,y= value*12/1000000,color="Kansas City Streetcar Ridership"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  annotate("vline", x = as.Date("2016-04-01"), xintercept = as.Date("2016-04-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Streetcar\nOpens", x = as.Date("2016-03-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-10-01"), xintercept = as.Date("2025-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Main Street\nExtension", x = as.Date("2025-09-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-10-01"), xintercept = as.Date("2026-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Riverfront\nExtension", x = as.Date("2026-09-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,4), expand = c(0,0), breaks = c(0,1,2,3,4,5,6,7)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Kansas City Streetcar Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail Lines",subtitle = "Kansas City is Extending its Streetcar Line to the UMKC Campus") +
  theme_apricitas + theme(legend.position = c(.5,.825), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Solid = 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*3.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = KC_STREETCAR_RIDERSHIP_graph, "KC Streetcar Ridership graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


VALLEY_METRO_MONTH_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "Valley Metro Rail, Inc.", month >= as.Date("2008-01-01")), aes(x=month,y= year_roll/1000000,color="Valley Metro Ridership"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Valley Metro Rail, Inc.", month >= as.Date("2008-01-01")), aes(x=month,y= value*12/1000000,color="Valley Metro Ridership"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  annotate(geom = "segment", x = as.Date("2008-12-01"), xend = as.Date("2008-12-01"), y = 0, yend = 15, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "Metro\nRail\nOpens", x = as.Date("2008-10-01"), y = 10, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2015-08-01"), xintercept = as.Date("2015-08-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Central\nMesa\nExtension", x = as.Date("2015-06-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2016-03-01"), xintercept = as.Date("2016-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northwest\nExtension\nPhase I", x = as.Date("2016-5-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2019-05-01"), xintercept = as.Date("2019-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Gilbert\nRoad\nExtension", x = as.Date("2019-07-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2022-05-01"), xintercept = as.Date("2022-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Tempe\nStreetcar\nOpens", x = as.Date("2022-03-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-01-01"), xintercept = as.Date("2024-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Northwest\nExtension\nPhase II", x = as.Date("2023-12-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-06-01"), xintercept = as.Date("2025-06-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "B-Line\n(South PHX)\nOpens", x = as.Date("2025-08-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,25), expand = c(0,0), breaks = c(0,5,10,15,20,25)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Valley Metro Rail (Phoenix) Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail and Streetcar Lines",subtitle = "Phoenix Light Rail is Currently Undergoing Significant Expansion Projects") +
  theme_apricitas + theme(legend.position = c(.18,.875), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Solid = 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2008-01-01")-(.1861*(today()-as.Date("2008-01-01"))), xmax = as.Date("2008-01-01")-(0.049*(today()-as.Date("2008-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VALLEY_METRO_MONTH_graph, "Valley Metro Rail Monthly Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


RAIL_INC_CR_BULK <- NTD_BULK %>%
  filter(modes_simplified == "Rail") %>%
  mutate(agency = gsub("San Diego Trolley, Inc.","San Diego Metropolitan Transit System", agency)) %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup()


DART_RIDERSHIP_graph <- ggplot() + 
  geom_line(data=filter(RAIL_INC_CR_BULK, agency == "Dallas Area Rapid Transit", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="Ridership,\nRolling 12M"), size = 1.25) +
  annotate("vline", x = as.Date("2009-08-01"), xintercept = as.Date("2009-08-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Green\nLine\nOpens", x = as.Date("2009-07-01"), y = 35, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2010-11-01"), xintercept = as.Date("2010-11-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Orange Line &\nGreen Line\nExtension Open", x = as.Date("2010-12-01"), y = 35, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  
  annotate("vline", x = as.Date("2015-03-01"), xintercept = as.Date("2015-03-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Dallas\nStreetcar\nOpens", x = as.Date("2015-02-01"), y = 20, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  
  annotate("vline", x = as.Date("2016-07-01"), xintercept = as.Date("2016-07-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Dallas\nStreetcar\nExtension", x = as.Date("2016-08-01"), y = 20, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  
  annotate("vline", x = as.Date("2025-10-01"), xintercept = as.Date("2025-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Silver\nLine\nOpens", x = as.Date("2025-09-01"), y = 35, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  
  #annotate("vline", x = as.Date("2026-10-01"), xintercept = as.Date("2026-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Riverfront\nExtension", x = as.Date("2026-09-01"), y = 2.25, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,40), expand = c(0,0), breaks = c(0,10,20,30,40,50,60,70)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("DART (Dallas) Rail Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Light Rail Lines",subtitle = "DART Has Recently Opened the 26 mile Silver Line") +
  theme_apricitas + theme(legend.position = c(.12,.925), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DART_RIDERSHIP_graph, "DART Ridership graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



CALIFORNIA_TRANSIT_graph <- ggplot() + 
  geom_line(data=filter(RAIL_BULK, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="BART (Bay Area)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="Muni (SF)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(SAN_DIEGO_TROLLEY, agency == "San Diego Metropolitan Transit System", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="San Diego Trolley (SD)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Santa Clara Valley Transportation Authority", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="VTA Light Rail (SJ)"), size = 1.25) +
  geom_line(data=filter(RAIL_BULK, agency == "Sacramento Regional Transit District", month >= as.Date("2003-01-01")), aes(x=month,y= year_roll/1000000,color="SacRT (Sacramento)"), size = 1.25) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,190), expand = c(0,0), breaks = c(0,25,50,75,100,125,150,175,200)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("California Rail Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data") +
  theme_apricitas + theme(legend.position = c(.33,.87)) +
  scale_color_manual(name= "Ridership, Rolling 12M Total",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("BART (Bay Area)","LA Metro (LA)","San Diego Trolley (SD)","Muni (SF)","SacRT (Sacramento)","VTA Light Rail (SJ)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*190), ymax = 0) +
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(ncol =2))

ggsave(dpi = "retina",plot = CALIFORNIA_TRANSIT_graph, "California Transit Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


COMMUTER_RAIL_BULK <- NTD_BULK %>%
  filter(modes == "CR") %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup() #%>%
  #filter(month == as.Date("2019-12-01"))

# Join the latest and previous rankings for Rail Networks
COMMUTER_RAIL_RANKINGS_LATEST_PREVIOUS <- COMMUTER_RAIL_BULK %>%
  filter(month == max(month)) %>%
  arrange(desc(year_roll)) %>%
  mutate(rank_latest = row_number()) %>%
  inner_join(COMMUTER_RAIL_BULK %>%
               filter(month < max(month)) %>%
               filter(month == max(month)) %>%
               arrange(desc(year_roll)) %>%
               mutate(rank_previous = row_number()), by = "agency") %>%
  select(agency, rank_latest, rank_previous)

# Determine which commuter rail networks  passed others
RESULTS_COMMUTER_RAIL <- COMMUTER_RAIL_RANKINGS_LATEST_PREVIOUS %>%
  filter(rank_latest < rank_previous) %>%
  rowwise() %>%
  mutate(passed_agency = list(find_passed_agencies(agency, COMMUTER_RAIL_RANKINGS_LATEST_PREVIOUS))) %>%
  filter(length(passed_agency) > 0) %>%
  mutate(passed_agency = paste(passed_agency, collapse = ", ")) %>%
  mutate(message = paste(agency, "passed", passed_agency,"to become rank",rank_latest)) %>%
  pull(message) %>%
  walk(print)




LARGE_COMMUTER_AGENCIES <- ggplot() + 
  geom_line(data = COMMUTER_RAIL_BULK %>%
              filter(agency %in% (COMMUTER_RAIL_BULK %>%
                                    filter(month == as.Date("2019-12-01")) %>%
                                    arrange(desc(year_roll)) %>%
                                    slice(1:10) %>%
                                    pull(agency))), aes(x=month,y= year_roll/1000000,color = agency), size = 1.25)

LARGE_COMMUTER_AGENCIES <- ggplot() + 
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="SEPTA (PA)"), size = 1.25) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Northeast Illinois Regional Commuter Railroad Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Metra (IL)"), size = 1.25) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "New Jersey Transit Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="NJTransit (NJ/NY)"), size = 1.25) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Metro-North Commuter Railroad Company, dba: MTA Metro-North Railroad", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Metro-North (NY)"), size = 1.25) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "MTA Long Island Rail Road", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Long Island Rail Road (NY)"), size = 1.25) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MBTA (MA)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,150), expand = c(0,0), breaks = c(0,50,100,150)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Ridership, US 6 Largest Commuter Rail Networks") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Includes Commuter Rail but Not Light Rail, Heavy Rail, etc.",subtitle = "NY-Area Commuter Rail Networks Have the Highest Ridership and Have Seen Strong Recoveries") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 23)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("Long Island Rail Road (NY)","Metro-North (NY)","NJTransit (NJ/NY)","Metra (IL)","SEPTA (PA)","MBTA (MA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LARGE_COMMUTER_AGENCIES, "Large Commuter Agencies.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CALTRAIN_RIDERSHIP <- ggplot() + 
  annotate("vline", x = as.Date("2024-09-01"), xintercept = as.Date("2024-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Caltrain\nElectrification\nCompletes", x = as.Date("2024-07-01"), y = 20, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Peninsula Corridor Joint Powers Board", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Caltrain Ridership"), size = 1.25) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Peninsula Corridor Joint Powers Board", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="Caltrain Ridership"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,25), expand = c(0,0), breaks = c(0,5,10,15,20,25)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Caltrain Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "Caltrain Ridership Has Bounced Up in Wake of the Line's Electrification") +
  theme_apricitas + theme(legend.position = c(.25,.90)) +
  scale_color_manual(name= "Solid = 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CALTRAIN_RIDERSHIP, "Caltrain Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

CALTRAIN_RIDERSHIP_MONTHLY <- ggplot() + 
  annotate("vline", x = as.Date("2024-09-01"), xintercept = as.Date("2024-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Caltrain\nElectrification\nCompletes", x = as.Date("2024-07-01"), y = 20, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Peninsula Corridor Joint Powers Board", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Caltrain Ridership"), size = 1.25) +
  #geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Peninsula Corridor Joint Powers Board", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="Caltrain Ridership"), size = 0.75, linetype = "dashed", alpha = 0.5) +
  geom_line(data=filter(COMMUTER_RAIL_BULK, agency == "Peninsula Corridor Joint Powers Board", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="Caltrain Ridership"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,25), expand = c(0,0), breaks = c(0,5,10,15,20,25)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Caltrain Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "Caltrain Ridership Has Bounced Up in Wake of the Line's Electrification") +
  theme_apricitas + theme(legend.position = c(.25,.90)) +
  #scale_color_manual(name= "Solid = 12M Total\nDashed = Monthly, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  scale_color_manual(name= "Monthly Ridership, Annualized",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CALTRAIN_RIDERSHIP_MONTHLY, "Caltrain Ridership Monthly.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


BUS_BULK <- NTD_BULK %>%
  filter(modes_simplified == "Bus") %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup()

BUS_RANKINGS_LATEST_PREVIOUS <- BUS_BULK %>%
  filter(month == max(month)) %>%
  arrange(desc(year_roll)) %>%
  mutate(rank_latest = row_number()) %>%
  inner_join(BUS_BULK %>%
               filter(month < max(month)) %>%
               filter(month == max(month)) %>%
               arrange(desc(year_roll)) %>%
               mutate(rank_previous = row_number()), by = "agency") %>%
  select(agency, rank_latest, rank_previous)


# Determine which agencies passed others
RESULTS_BUS <- BUS_RANKINGS_LATEST_PREVIOUS %>%
  filter(rank_latest < rank_previous) %>%
  rowwise() %>%
  mutate(passed_agency = list(find_passed_agencies(agency, BUS_RANKINGS_LATEST_PREVIOUS))) %>%
  filter(length(passed_agency) > 0) %>%
  mutate(passed_agency = paste(passed_agency, collapse = ", ")) %>%
  mutate(message = paste(agency, "passed", passed_agency,"to become rank",rank_latest)) %>%
  pull(message) %>%
  walk(print)

LARGE_AGENCIES <- ggplot() + 
  geom_line(data = BUS_BULK %>%
              filter(agency %in% (BUS_BULK %>%
                                    filter(month == as.Date("2019-12-01")) %>%
                                    arrange(desc(year_roll)) %>%
                                    slice(1:5) %>%
                                    pull(agency))), aes(x=month,y= year_roll/1000000,color = agency), size = 1.25)
BRT_BULK <- NTD_BULK %>%
  filter(modes_simplified == "Bus" & modes == "RB") %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup()


BUS_Ridership <- ggplot() + 
  geom_line(data=filter(BUS_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="WMATA (DC)"), size = 1.25) +
  geom_line(data=filter(BUS_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(BUS_BULK, agency == "Chicago Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(BUS_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Muni (SF)"), size = 1.25)  +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,700), expand = c(0,0)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Bus Ridership by Agency") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "MetroLink Ridership Has Only Reached 5M Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MetroLink_Ridership, "MetroLink Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

INDYGO_BUS_graph <- ggplot() + 
  #geom_line(data=filter(BRT_BULK, agency == "Indianapolis and Marion County Public Transportation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="IndyGo Bus Rapid Transit Ridership"), size = 1.25) +
  #geom_line(data=filter(BRT_BULK, agency == "Indianapolis and Marion County Public Transportation", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="IndyGo Bus Rapid Transit Ridership"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(BRT_BULK, agency == "Indianapolis and Marion County Public Transportation", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="IndyGo Bus Rapid Transit Ridership\nMonthly, Annualized"), size = 1.25) +
  annotate("vline", x = as.Date("2019-09-01"), xintercept = as.Date("2019-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Red\nLine\nOpens", x = as.Date("2019-08-15"), y = 2.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2019-09-01"), xintercept = as.Date("2019-12-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Free\nFares\nEnd", x = as.Date("2019-12-15"), y = 2.5, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-10-01"), xintercept = as.Date("2024-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Purple\nLine\nOpens", x = as.Date("2024-09-15"), y = 2.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-05-01"), xintercept = as.Date("2027-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Blue\nLine\nOpens", x = as.Date("2027-03-01"), y = 30, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,3), expand = c(0,0), breaks = c(0,1,2,3)) +
  scale_x_date(limits = c(as.Date("2019-06-01"), max(BRT_BULK$month))) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("IndyGo (Indianapolis) BRT Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Only Includes BRT Ridership, Not All IndyGo Buses",subtitle = "Indianapolis' New BRT Network is Steadily Gaining Ridership") +
  theme_apricitas + theme(legend.position = c(.42,.95), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-06-01")-(.1861*(today()-as.Date("2019-06-01"))), xmax = as.Date("2019-06-01")-(0.049*(today()-as.Date("2019-06-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = INDYGO_BUS_graph, "INDYGO BRT Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


INDYGO_BUS_graph <- ggplot() + 
  #geom_line(data=filter(BRT_BULK, agency == "Indianapolis and Marion County Public Transportation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="IndyGo Bus Rapid Transit Ridership"), size = 1.25) +
  #geom_line(data=filter(BRT_BULK, agency == "Indianapolis and Marion County Public Transportation", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="IndyGo Bus Rapid Transit Ridership"), size = 0.75, linetype = "dashed") +
  geom_line(data=filter(BUS_BULK, agency == "Indianapolis and Marion County Public Transportation", month >= as.Date("2014-01-01")), aes(x=month,y= value/1000000*12,color="IndyGo Bus Rapid Transit Ridership\nMonthly, Annualized"), size = 1.25) +
  annotate("vline", x = as.Date("2019-09-01"), xintercept = as.Date("2019-09-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Red\nLine\nOpens", x = as.Date("2019-08-15"), y = 2.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2019-09-01"), xintercept = as.Date("2019-12-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Free\nFares\nEnd", x = as.Date("2019-12-15"), y = 2.5, color = "white", size = 3.5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2024-10-01"), xintercept = as.Date("2024-10-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "Purple\nLine\nOpens", x = as.Date("2024-09-15"), y = 2.5, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  #annotate("vline", x = as.Date("2026-05-01"), xintercept = as.Date("2027-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  #annotate("text", label = "Blue\nLine\nOpens", x = as.Date("2027-03-01"), y = 30, color = "white", size = 3.5, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  theme_apricitas + theme(legend.position = c(.775,.75)) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,3), expand = c(0,0), breaks = c(0,1,2,3)) +
  scale_x_date(limits = c(as.Date("2019-06-01"), max(BRT_BULK$month))) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("IndyGo (Indianapolis) BRT Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data\nNOTE: Only Includes BRT Ridership, Not All IndyGo Buses",subtitle = "Indianapolis' New BRT Network is Steadily Gaining Ridership") +
  theme_apricitas + theme(legend.position = c(.42,.95), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-06-01")-(.1861*(today()-as.Date("2019-06-01"))), xmax = as.Date("2019-06-01")-(0.049*(today()-as.Date("2019-06-01"))), ymin = 0-(.3*3), ymax = 0) +
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = INDYGO_BUS_graph, "INDYGO BRT Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



AGENCY_BULK <- NTD_BULK %>%
  mutate(agency = gsub("MTA Bus Company","MTA New York City Transit", agency)) %>% #Adding outerborough buses to MTA ridership data
  #filter(modes_simplified == "Rail" & modes != "CR") %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup() #%>%
  #filter(month == max(month))



AGENCY_RANKINGS_LATEST_PREVIOUS <- AGENCY_BULK %>%
  filter(month == max(month)) %>%
  arrange(desc(year_roll)) %>%
  mutate(rank_latest = row_number()) %>%
  inner_join(AGENCY_BULK %>%
               filter(month < max(month)) %>%
               filter(month == max(month)) %>%
               arrange(desc(year_roll)) %>%
               mutate(rank_previous = row_number()), by = "agency") %>%
  select(agency, rank_latest, rank_previous)

# Determine which agencies passed others
RESULTS_AGENCY <- AGENCY_RANKINGS_LATEST_PREVIOUS %>%
  filter(rank_latest < rank_previous) %>%
  rowwise() %>%
  mutate(passed_agency = list(find_passed_agencies(agency, AGENCY_RANKINGS_LATEST_PREVIOUS))) %>%
  filter(length(passed_agency) > 0) %>%
  mutate(passed_agency = paste(passed_agency, collapse = ", ")) %>%
  mutate(message = paste(agency, "passed", passed_agency,"to become rank",rank_latest)) %>%
  pull(message) %>%
  walk(print)

#
LARGE_AGENCIES <- ggplot() + 
  geom_line(data = AGENCY_BULK %>%
              filter(agency %in% (AGENCY_BULK %>%
                                    filter(month == as.Date("2019-12-01")) %>%
                                    arrange(desc(year_roll)) %>%
                                    slice(1:7) %>%
                                    pull(agency))), aes(x=month,y= year_roll/1000000,color = agency), size = 1.25)


AGENCIES_ALL_MODES <- ggplot() + 
  geom_line(data=filter(AGENCY_BULK, agency == "Chicago Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MBTA (Boston)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="WMATA (DC)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="SEPTA (Philly)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,550), expand = c(0,0), breaks = c(100,200,300,400,500)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Total Ridership (Rail, Bus, etc)\n2nd-5th Largest US Transit Agencies") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data, Rankings Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("CTA (Chicago)","LA Metro (LA)","MBTA (Boston)","WMATA (DC)","SEPTA (Philly)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*500), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AGENCIES_ALL_MODES, "Agencies Total Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

WMATA_CTA_LAMETRO_MONTHLY_RIDERSHIP <- ggplot() + 
  geom_line(data=filter(AGENCY_BULK, agency == "Chicago Transit Authority", month >= as.Date("2001-01-01")), aes(x=month,y= value/1000000,color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2001-01-01")), aes(x=month,y= value/1000000,color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2001-01-01")), aes(x=month,y= value/1000000,color="WMATA (DC)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,60), expand = c(0,0), breaks = c(0,10,20,30,40,50,60)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Total Monthly Ridership (Rail, Bus, etc)") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data, Rankings Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.15,.92), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("CTA (Chicago)","LA Metro (LA)","MBTA (Boston)","WMATA (DC)","SEPTA (Philly)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*60), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WMATA_CTA_LAMETRO_MONTHLY_RIDERSHIP, "WMATA_CTA_LAMETRO Monthly Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


MetroLink_Ridership <- ggplot() + 
  geom_line(data=filter(AGENCY_BULK, agency == "Southern California Regional Rail Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="MetroLink"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,15), expand = c(0,0), breaks = c(0,5,10,15)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("MetroLink Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "MetroLink Ridership Has Only Reached 5M Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.80,.75), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MetroLink_Ridership, "MetroLink Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Sound_Ridership <- ggplot() + 
  geom_line(data=filter(AGENCY_BULK, agency == "Central Puget Sound Regional Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Sound Transit\n(Incl. Link, Sounder, & Commuter Buses)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,65), expand = c(0,0), breaks = c(20,40,60)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Sound Transit Total Ridership") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data",subtitle = "Sound Transit Ridership Is Approaching 40M Trips Per Year") +
  theme_apricitas + theme(legend.position = c(.50,.90), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Ridership, Rolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*65), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Sound_Ridership, "Sound Total Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing




REGION_BULK <- NTD_BULK %>%
  #filter(modes_simplified == "Rail" & modes != "CR") %>%
  group_by(uza_name,month) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup()

REGION_RANKINGS_LATEST_PREVIOUS <- REGION_BULK %>%
  filter(month == max(month)) %>%
  arrange(desc(year_roll)) %>%
  mutate(rank_latest = row_number()) %>%
  inner_join(REGION_BULK %>%
               filter(month < max(month)) %>%
               filter(month == max(month)) %>%
               arrange(desc(year_roll)) %>%
               mutate(rank_previous = row_number()), by = "uza_name") %>%
  select(uza_name, rank_latest, rank_previous)

find_passed_regions <- function(current_agency, comparison) {
  current_rank_previous <- comparison$rank_previous[comparison$uza_name == current_agency]
  current_rank_latest <- comparison$rank_latest[comparison$uza_name == current_agency]
  
  passed <- comparison %>%
    filter(rank_previous < current_rank_previous & rank_latest >= current_rank_latest) %>%
    pull(uza_name)
  
  return(passed)
}


# Determine which REGIONS passed others
RESULTS_REGION <- REGION_RANKINGS_LATEST_PREVIOUS %>%
  filter(rank_latest < rank_previous) %>%
  rowwise() %>%
  mutate(passed_agency = list(find_passed_regions(uza_name, REGION_RANKINGS_LATEST_PREVIOUS))) %>%
  filter(length(passed_agency) > 0) %>%
  mutate(passed_agency = paste(passed_agency, collapse = ", ")) %>%
  mutate(message = paste(uza_name, "passed", passed_agency,"to become rank",rank_latest)) %>%
  pull(message) %>%
  walk(print)

LARGE_REGIONS <- ggplot() + 
  geom_line(data = REGION_BULK %>%
              filter(uza_name %in% (REGION_BULK %>%
                                    filter(month == as.Date("2019-12-01")) %>%
                                    arrange(desc(year_roll)) %>%
                                    slice(1:10) %>%
                                    pull(uza_name))), aes(x=month,y= year_roll/1000000,color = uza_name), size = 1.25)



REGION_Ridership <- ggplot() + 
  geom_line(data=filter(REGION_BULK, uza_name == "Washington--Arlington, DC--VA--MD", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Washington DC MSA"), size = 1.25) +
  geom_line(data=filter(REGION_BULK, uza_name == "San Francisco--Oakland, CA", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="San Francisco MSA"), size = 1.25) +
  geom_line(data=filter(REGION_BULK, uza_name == "Los Angeles--Long Beach--Anaheim, CA", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Los Angeles MSA"), size = 1.25) +
  geom_line(data=filter(REGION_BULK, uza_name == "Chicago, IL--IN", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Chicago MSA"), size = 1.25) +
  #geom_line(data=filter(REGION_BULK, uza_name == "Seattle--Tacoma, WA", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/1000000,color="Seattle MSA"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "M"),limits = c(0,700), expand = c(0,0)) +
  ylab("Millions of Unlinked Passenger Trips") +
  ggtitle("Public Transit Ridership by Region") +
  labs(caption = "Graph created by @JosephPolitano using FTA Data") +
  theme_apricitas + theme(legend.position = c(.73,.8), plot.title = element_text(size = 27)) +
  scale_color_manual(name= "Ridership, All Modes\nRolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*700), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REGION_Ridership, "Region Ridership.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FIRST_SEVEN_RIDERSHIP_RECOVERY_AGENCY_LONG_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(AGENCY_BULK, agency == "Chicago Transit Authority", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="BART (Bay Area)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="MBTA (Boston)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="WMATA (DC)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "MTA New York City Transit", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="MTA (NYC)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2002-01-01")), aes(x=month,y= year_roll/year_roll[216],color="SEPTA (Philly)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Port Authority Trans-Hudson Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="PATH (NY/NJ)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="Muni (SF)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Atlanta Rapid Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MARTA (Atlanta)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1.4), expand = c(0,0), breaks = c(0,.25,.5,.75,1,1.25)) +
  ylab("Percent of 2019 Ridership") +
  ggtitle("Ridership Recovery, US 7 Largest Transit Agencies") +
  labs(caption = "1st-7th Systems Selected Based on 2019 Ridership Rankings",subtitle = "LA Metro Has Led the Post-COVID Ridership Recovery") +
  theme_apricitas + theme(legend.position = c(.6,.325), plot.title = element_text(size = 21)) +
  scale_color_manual(name= "Ridership, % of 2019\nRolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("MTA (NYC)","WMATA (DC)","CTA (Chicago)","MBTA (Boston)","BART (Bay Area)","SEPTA (Philly)","LA Metro (LA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2002-01-01")-(.1861*(today()-as.Date("2002-01-01"))), xmax = as.Date("2002-01-01")-(0.049*(today()-as.Date("2002-01-01"))), ymin = 0-(.3*1.4), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIRST_SEVEN_RIDERSHIP_RECOVERY_AGENCY_LONG_graph, "1st Seven Ridership Recovery Agency Long Percent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FIRST_SEVEN_RIDERSHIP_RECOVERY_AGENCY_SHORT_graph <- ggplot() + #plotting regular vs non-regular employment
  geom_line(data=filter(AGENCY_BULK, agency == "Chicago Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="CTA (Chicago)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="BART (Bay Area)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MBTA (Boston)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="WMATA (DC)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "MTA New York City Transit", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MTA (NYC)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="LA Metro (LA)"), size = 1.25) +
  geom_line(data=filter(AGENCY_BULK, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="SEPTA (Philly)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Port Authority Trans-Hudson Corporation", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="PATH (NY/NJ)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "City and County of San Francisco", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="Muni (SF)"), size = 1.25) +
  #geom_line(data=filter(RAIL_BULK, agency == "Metropolitan Atlanta Rapid Transit Authority", month >= as.Date("2014-01-01")), aes(x=month,y= year_roll/year_roll[72],color="MARTA (Atlanta)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1.4), expand = c(0,0), breaks = c(0,.25,.5,.75,1,1.25)) +
  ylab("Percent of 2019 Ridership") +
  ggtitle("Ridership Recovery, US 7 Largest Transit Agencies") +
  labs(caption = "1st-7th Systems Selected Based on 2019 Ridership Rankings",subtitle = "LA Metro Has Led the Post-COVID Ridership Recovery") +
  theme_apricitas + theme(legend.position = c(.4,.325), plot.title = element_text(size = 21)) +
  scale_color_manual(name= "Ridership, % of 2019\nRolling 12M Totals",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("MTA (NYC)","WMATA (DC)","CTA (Chicago)","MBTA (Boston)","BART (Bay Area)","SEPTA (Philly)","LA Metro (LA)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*1.4), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIRST_SEVEN_RIDERSHIP_RECOVERY_AGENCY_SHORT_graph, "1st Seven Ridership Recovery Agency Short Percent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


AGENCY_YOY_LINE_GROWTH <- AGENCY_BULK %>%
  group_by(agency) %>%
  mutate(yoy = value/lag(value,12)-1)

LA_METRO_YOY_RIDERSHIP_GROWTH <- ggplot() + #plotting regular vs non-regular employment
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Chicago Transit Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="CTA (Chicago)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="BART (Bay Area)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="MBTA (Boston)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="WMATA (DC)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "MTA New York City Transit", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="MTA (NYC)"), size = 1.25) +
  annotate("text", label = "CA National Guard\nFederalized", x = as.Date("2025-04-15"), y = .10, color = "white", size = 4, hjust = 1, lineheight = 0.8, alpha = 0.75) +
  annotate("vline", x = as.Date("2025-05-01"), xintercept = as.Date("2025-05-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2022-06-01")), aes(x=month,y= yoy,color="Year-on-Year Ridership Growth,\nLA Metro (Bus & Rail)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="SEPTA (Philly)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.10,.20), expand = c(0,0), breaks = c(-.1,0,.1,.2)) +
  ylab("Year-on-Year Ridership Growth") +
  ggtitle("LA Metro Ridership Growth") +
  labs(caption = "Graph Created by @Josephpolitano Using FTA NTD Data",subtitle = "LA Metro Ridership Shrunk in June, July, & August") +
  theme_apricitas + theme(legend.position = c(.6,.825)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-06-01")-(.1861*(today()-as.Date("2022-06-01"))), xmax = as.Date("2022-06-01")-(0.049*(today()-as.Date("2022-06-01"))), ymin = -.10-(.3*.3), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LA_METRO_YOY_RIDERSHIP_GROWTH, "LA METRO YOY RIDERSHIP GROWTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


SEPTA_YOY_RIDERSHIP_GROWTH <- ggplot() + #plotting regular vs non-regular employment
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Chicago Transit Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="CTA (Chicago)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "San Francisco Bay Area Rapid Transit District", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="BART (Bay Area)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Massachusetts Bay Transportation Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="MBTA (Boston)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Washington Metropolitan Area Transit Authority", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="WMATA (DC)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "MTA New York City Transit", month >= as.Date("2022-01-01")), aes(x=month,y= yoy,color="MTA (NYC)"), size = 1.25) +
  #geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Los Angeles County Metropolitan Transportation Authority", month >= as.Date("2022-06-01")), aes(x=month,y= yoy,color="Year-on-Year Ridership Growth,\nLA Metro (Bus & Rail)"), size = 1.25) +
  geom_line(data=filter(AGENCY_YOY_LINE_GROWTH, agency == "Southeastern Pennsylvania Transportation Authority", month >= as.Date("2023-02-01")), aes(x=month,y= yoy,color="Year-on-Year Ridership Growth,\nSEPTA (Bus, Metro, Trolley, & Regional Rail)"), size = 1.25) +
  annotate(geom = "hline",y = 0,yintercept = 0, size = 0.5,color = "white") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.15,.275), expand = c(0,0), breaks = c(-.1,0,.1,.2)) +
  ylab("Year-on-Year Ridership Growth") +
  ggtitle("SEPTA Ridership Growth") +
  labs(caption = "Graph Created by @Josephpolitano Using FTA NTD Data",subtitle = "SEPTA Ridership Shrunk in August") +
  theme_apricitas + theme(legend.position = c(.3,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-06-01")-(.1861*(today()-as.Date("2022-06-01"))), xmax = as.Date("2022-06-01")-(0.049*(today()-as.Date("2022-06-01"))), ymin = -.10-(.3*.3), ymax = -.10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SEPTA_YOY_RIDERSHIP_GROWTH, "SEPTA YOY RIDERSHIP GROWTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



AGENCY_EX_CR_BULK <- NTD_BULK %>%
  filter(modes != "CR") %>%
  group_by(agency,month) %>%
  #drop_na() %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(year_roll = rollsum(value, 12, fill = NA, align = "right")) %>%
  mutate(six_month_roll = rollsum(value, 6, fill = NA, align = "right")) %>%
  mutate(three_month_roll = rollsum(value, 3, fill = NA, align = "right")) %>%
  ungroup() #%>%
#filter(month == max(month))


RAIL_RECOVERY <- RAIL_BULK %>%
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == 2019 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_recovery = ((get(paste0("value_", recent_year))) / value_2019)) %>%
  ungroup() %>%
  arrange(desc(value_2019)) %>%
  slice(1:10) %>%
  arrange(percentage_recovery) %>%
  mutate(agency = case_when(
    agency == "San Francisco Bay Area Rapid Transit District" ~ "BART (Bay Area)",
    agency == "Los Angeles County Metropolitan Transportation Authority" ~ "LA Metro (LA)",
    agency == "Chicago Transit Authority" ~ "CTA (Chicago)",
    agency == "Metropolitan Atlanta Rapid Transit Authority" ~ "MARTA (Atlanta)",
    agency == "Washington Metropolitan Area Transit Authority" ~ "WMATA (DC)",
    agency == "MTA New York City Transit" ~ "MTA (NYC)",
    agency == "Port Authority Trans-Hudson Corporation" ~ "PATH (NYC/NJ)",
    agency == "City and County of San Francisco" ~ "Muni (SF)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    TRUE ~ agency
  ))
  


RAIL_RECOVERY_TOP_10 <- RAIL_RECOVERY %>%
  mutate(agency = factor(agency, levels = RAIL_RECOVERY %>% 
                           pull(agency)))
  
  

RAIL_RECOVERY_TOP_10_graph <- ggplot(data = RAIL_RECOVERY_TOP_10, aes(x = agency, y = percentage_recovery)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% of 2019 Ridership") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ggtitle(paste("Ridership Recovery, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs 2019\n10 Largest US Urban Rail Networks", sep = "")) +
  labs(caption = "Graph created by @JosephPolitano using FTA Data. NOTE: Includes Heavy & Light Rail But Not Commuter Rail. Top 10 Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = RAIL_RECOVERY_TOP_10_graph, "Rail Recovery Top 10 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RAIL_RECOVERY_YOY <- RAIL_BULK %>%
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == recent_year-1 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(AGENCY_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_growth = ((get(paste0("value_", recent_year))) / get(paste0("value_", recent_year-1)))-1) %>%
  ungroup() %>%
  arrange(desc(get(paste0("value_", recent_year[1]-1))))


RAIL_RECOVERY_YOY <- RAIL_RECOVERY_YOY %>%
  slice(1:10) %>%
  arrange(percentage_growth) %>%
  mutate(agency = case_when(
    agency == "San Francisco Bay Area Rapid Transit District" ~ "BART (Bay Area)",
    agency == "Los Angeles County Metropolitan Transportation Authority" ~ "LA Metro (LA)",
    agency == "Chicago Transit Authority" ~ "CTA (Chicago)",
    agency == "Metropolitan Atlanta Rapid Transit Authority" ~ "MARTA (Atlanta)",
    agency == "Washington Metropolitan Area Transit Authority" ~ "WMATA (DC)",
    agency == "MTA New York City Transit" ~ "MTA (NYC)",
    agency == "Port Authority Trans-Hudson Corporation" ~ "PATH (NYC/NJ)",
    agency == "City and County of San Francisco" ~ "Muni (SF)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    agency == "San Diego Metropolitan Transit System" ~ "MTS (San Diego)",
    TRUE ~ agency
  ))

RAIL_RECOVERY_YOY_TOP_10 <- RAIL_RECOVERY_YOY %>%
  mutate(agency = factor(agency, levels = RAIL_RECOVERY_YOY %>% 
                           pull(agency)))



RAIL_RECOVERY_TOP_10_YOY_graph <- ggplot(data = RAIL_RECOVERY_YOY_TOP_10, aes(x = agency, y = percentage_growth)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle(paste("Ridership Growth, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs ",year(max(RAIL_BULK$month))-1,"\n10 Largest US Urban Rail Networks", sep = "")) +
  labs(caption = paste("Graph created by @JosephPolitano using FTA Data. NOTE: Includes Heavy & Light Rail But Not Commuter Rail. Top 10 Selected Based on",year(max(RAIL_BULK$month))-1 ,"Ridership")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = RAIL_RECOVERY_TOP_10_YOY_graph, "Rail Recovery Top 10 YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


AGENCY_RECOVERY <- AGENCY_BULK %>%
  mutate(agency = gsub("MTA Bus Company","MTA New York City Transit", agency)) %>% #Adding outerborough buses to MTA ridership data
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == 2019 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(AGENCY_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_recovery = ((get(paste0("value_", recent_year))) / value_2019)) %>%
  ungroup() %>%
  arrange(desc(value_2019))


AGENCY_RECOVERY <- AGENCY_RECOVERY %>%
  slice(1:10) %>%
  #filter(agency %in% c("San Francisco Bay Area Rapid Transit District","Los Angeles County Metropolitan Transportation Authority","Chicago Transit Authority","King County","Washington Metropolitan Area Transit Authority","MTA New York City Transit","New Jersey Transit Corporation","City and County of San Francisco","Massachusetts Bay Transportation Authority","Southeastern Pennsylvania Transportation Authority")) %>%
  arrange(percentage_recovery) %>%
  mutate(agency = case_when(
    agency == "San Francisco Bay Area Rapid Transit District" ~ "BART (Bay Area)",
    agency == "Los Angeles County Metropolitan Transportation Authority" ~ "LA Metro (LA)",
    agency == "Chicago Transit Authority" ~ "CTA (Chicago)",
    agency == "King County" ~ "Metro (Seattle)",
    agency == "Washington Metropolitan Area Transit Authority" ~ "WMATA (DC)",
    agency == "MTA New York City Transit" ~ "MTA (NYC)",
    agency == "New Jersey Transit Corporation" ~ "NJTransit (NJ)",
    agency == "City and County of San Francisco" ~ "Muni (SF)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    TRUE ~ agency
  ))

AGENCY_RECOVERY_TOP_10 <- AGENCY_RECOVERY %>%
  mutate(agency = factor(agency, levels = AGENCY_RECOVERY %>% 
                           pull(agency)))



AGENCY_RECOVERY_TOP_10_graph <- ggplot(data = AGENCY_RECOVERY_TOP_10, aes(x = agency, y = percentage_recovery)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% of 2019 Ridership") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ggtitle(paste("Ridership Recovery, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs 2019\n10 Largest US Transit Agencies", sep = "")) +
  labs(caption = "Graph created by @JosephPolitano using FTA Data. NOTE: MTA Does Not Include LIRR or Metro-North. Top 10 Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = AGENCY_RECOVERY_TOP_10_graph, "Agency Recovery Top 10 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

AGENCY_RECOVERY_YOY <- AGENCY_BULK %>%
  mutate(agency = gsub("MTA Bus Company","MTA New York City Transit", agency)) %>% #Adding outerborough buses to MTA ridership data
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == recent_year-1 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(AGENCY_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_growth = ((get(paste0("value_", recent_year))) / get(paste0("value_", recent_year-1)))-1) %>%
  ungroup() %>%
  arrange(desc(get(paste0("value_", recent_year[1]-1))))


AGENCY_RECOVERY_YOY <- AGENCY_RECOVERY_YOY %>%
  slice(1:10) %>%
  #filter(agency %in% c("San Francisco Bay Area Rapid Transit District","Los Angeles County Metropolitan Transportation Authority","Chicago Transit Authority","King County","Washington Metropolitan Area Transit Authority","MTA New York City Transit","New Jersey Transit Corporation","City and County of San Francisco","Massachusetts Bay Transportation Authority","Southeastern Pennsylvania Transportation Authority")) %>%
  arrange(percentage_growth) %>%
  mutate(agency = case_when(
    agency == "San Francisco Bay Area Rapid Transit District" ~ "BART (Bay Area)",
    agency == "Los Angeles County Metropolitan Transportation Authority" ~ "LA Metro (LA)",
    agency == "Chicago Transit Authority" ~ "CTA (Chicago)",
    agency == "King County" ~ "Metro (Seattle)",
    agency == "Washington Metropolitan Area Transit Authority" ~ "WMATA (DC)",
    agency == "MTA New York City Transit" ~ "MTA (NYC)",
    agency == "New Jersey Transit Corporation" ~ "NJTransit (NJ)",
    agency == "City and County of San Francisco" ~ "Muni (SF)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    agency == "County of Miami-Dade" ~ "MDT (Miami)",
    agency == "MTA Long Island Rail Road" ~ "LIRR (NY)",
    TRUE ~ agency
  ))

AGENCY_RECOVERY_YOY_TOP_10 <- AGENCY_RECOVERY_YOY %>%
  mutate(agency = factor(agency, levels = AGENCY_RECOVERY_YOY %>% 
                           pull(agency)))



AGENCY_RECOVERY_TOP_10_YOY_graph <- ggplot(data = AGENCY_RECOVERY_YOY_TOP_10, aes(x = agency, y = percentage_growth)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle(paste("Ridership Growth, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs ",year(max(RAIL_BULK$month))-1,"\n10 Largest US Transit Agencies", sep = "")) +
  labs(caption = paste("Graph created by @JosephPolitano using FTA Data. NOTE: MTA Does Not Include LIRR or Metro-North. Top 10 Selected Based on",year(max(RAIL_BULK$month))-1 ,"Ridership")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = AGENCY_RECOVERY_TOP_10_YOY_graph, "Agency Recovery Top 10 YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BUS_RECOVERY <- BUS_BULK %>%
  mutate(agency = gsub("MTA Bus Company","MTA New York City Transit", agency)) %>% #Adding outerborough buses to MTA ridership data
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == 2019 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(AGENCY_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_recovery = ((get(paste0("value_", recent_year))) / value_2019)) %>%
  ungroup() %>%
  arrange(desc(value_2019))


BUS_RECOVERY <- BUS_RECOVERY %>%
  slice(1:10) %>%
  arrange(percentage_recovery) %>%
  mutate(agency = case_when(
    agency == "San Francisco Bay Area Rapid Transit District" ~ "BART (Bay Area)",
    agency == "Los Angeles County Metropolitan Transportation Authority" ~ "LA Metro (LA)",
    agency == "Chicago Transit Authority" ~ "CTA (Chicago)",
    agency == "King County" ~ "Metro (Seattle)",
    agency == "Washington Metropolitan Area Transit Authority" ~ "WMATA (DC)",
    agency == "MTA New York City Transit" ~ "MTA (NYC)",
    agency == "New Jersey Transit Corporation" ~ "NJTransit (NJ)",
    agency == "City and County of San Francisco" ~ "Muni (SF)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    agency == "County of Miami-Dade" ~ "MDT (Miami)",
    TRUE ~ agency
  ))

BUS_RECOVERY_TOP_10 <- BUS_RECOVERY %>%
  mutate(agency = factor(agency, levels = BUS_RECOVERY %>% 
                           pull(agency)))



BUS_RECOVERY_TOP_10_graph <- ggplot(data = BUS_RECOVERY_TOP_10, aes(x = agency, y = percentage_recovery)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  annotate("hline", y = 1, yintercept = 1, color = "white", size = .5) +
  
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% of 2019 Ridership") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1.1), expand = c(0,0)) +
  ggtitle(paste("Ridership Recovery, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs 2019\n10 Largest US Bus Networks", sep = "")) +
  labs(caption = "Graph created by @JosephPolitano using FTA Data. Top 10 Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = BUS_RECOVERY_TOP_10_graph, "Bus Recovery Top 10 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


BUS_RECOVERY_YOY <- BUS_BULK %>%
  mutate(agency = gsub("MTA Bus Company","MTA New York City Transit", agency)) %>% #Adding outerborough buses to MTA ridership data
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == recent_year-1 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(AGENCY_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_growth = ((get(paste0("value_", recent_year))) / get(paste0("value_", recent_year-1)))-1) %>%
  ungroup() %>%
  arrange(desc(get(paste0("value_", recent_year[1]-1))))

BUS_RECOVERY_YOY <- BUS_RECOVERY_YOY %>%
  slice(1:10) %>%
  arrange(percentage_growth) %>%
  mutate(agency = case_when(
    agency == "San Francisco Bay Area Rapid Transit District" ~ "BART (Bay Area)",
    agency == "Los Angeles County Metropolitan Transportation Authority" ~ "LA Metro (LA)",
    agency == "Chicago Transit Authority" ~ "CTA (Chicago)",
    agency == "King County" ~ "Metro (Seattle)",
    agency == "Washington Metropolitan Area Transit Authority" ~ "WMATA (DC)",
    agency == "MTA New York City Transit" ~ "MTA (NYC)",
    agency == "New Jersey Transit Corporation" ~ "NJTransit (NJ)",
    agency == "City and County of San Francisco" ~ "Muni (SF)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    agency == "County of Miami-Dade" ~ "MDT (Miami)",
    TRUE ~ agency
  ))

BUS_RECOVERY_YOY_TOP_10 <- BUS_RECOVERY_YOY %>%
  mutate(agency = factor(agency, levels = BUS_RECOVERY_YOY %>% 
                           pull(agency)))

BUS_RECOVERY_TOP_10_YOY_graph <- ggplot(data = BUS_RECOVERY_YOY_TOP_10, aes(x = agency, y = percentage_growth)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle(paste("Ridership Growth, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs ",year(max(RAIL_BULK$month))-1,"\n10 Largest US Bus Networks", sep = "")) +
  labs(caption = paste("Graph created by @JosephPolitano using FTA Data. Top 10 Selected Based on",year(max(RAIL_BULK$month))-1 ,"Ridership")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = BUS_RECOVERY_TOP_10_YOY_graph, "Bus Recovery Top 10 YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

REGION_RECOVERY <- REGION_BULK %>%
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(uza_name) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == 2019 & month <= recent_month)) %>%
  group_by(uza_name, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(REGION_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_recovery = ((get(paste0("value_", recent_year))) / value_2019)) %>%
  ungroup() %>%
  arrange(desc(value_2019))


REGION_RECOVERY <- REGION_RECOVERY %>%
  slice(1:10) %>%
  arrange(percentage_recovery) %>%
  mutate(uza_name = case_when(
    uza_name == "Atlanta, GA" ~ "Atlanta",
    uza_name == "Chicago, IL--IN" ~ "Chicago",
    uza_name == "San Francisco--Oakland, CA" ~ "San Francisco",
    uza_name == "Boston, MA--NH" ~ "Boston",
    uza_name == "Philadelphia, PA--NJ--DE--MD" ~ "Philadelphia",
    uza_name == "Seattle--Tacoma, WA" ~ "Seattle",
    uza_name == "Los Angeles--Long Beach--Anaheim, CA" ~ "Los Angeles",
    uza_name == "Miami--Fort Lauderdale, FL" ~ "Miami",
    uza_name == "Washington--Arlington, DC--VA--MD" ~ "Washington DC",
    uza_name == "New York--Jersey City--Newark, NY--NJ" ~ "New York",
    TRUE ~ uza_name
  ))

REGION_RECOVERY_TOP_10 <- REGION_RECOVERY %>%
  mutate(uza_name = factor(uza_name, levels = REGION_RECOVERY %>% 
                             pull(uza_name)))



REGION_RECOVERY_TOP_10_graph <- ggplot(data = REGION_RECOVERY_TOP_10, aes(x = uza_name, y = percentage_recovery)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% of 2019 Ridership") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ggtitle(paste("Ridership Recovery, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs 2019\n10 Largest US Metro Areas by Transit Use", sep = "")) +
  labs(caption = "Graph created by @JosephPolitano using FTA Data. Top 10 Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = REGION_RECOVERY_TOP_10_graph, "Region Recovery Top 10 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


REGION_RECOVERY_YOY <- REGION_BULK %>%
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(uza_name) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == recent_year-1 & month <= recent_month)) %>%
  group_by(uza_name, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(REGION_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_growth = ((get(paste0("value_", recent_year))) / get(paste0("value_", recent_year-1)))-1) %>%
  ungroup() %>%
  arrange(desc(get(paste0("value_", recent_year[1]-1))))

REGION_RECOVERY_YOY <- REGION_RECOVERY_YOY %>%
  slice(1:10) %>%
  arrange(percentage_growth) %>%
  mutate(uza_name = case_when(
    uza_name == "Atlanta, GA" ~ "Atlanta",
    uza_name == "Chicago, IL--IN" ~ "Chicago",
    uza_name == "San Francisco--Oakland, CA" ~ "San Francisco",
    uza_name == "Boston, MA--NH" ~ "Boston",
    uza_name == "Philadelphia, PA--NJ--DE--MD" ~ "Philadelphia",
    uza_name == "Seattle--Tacoma, WA" ~ "Seattle",
    uza_name == "Los Angeles--Long Beach--Anaheim, CA" ~ "Los Angeles",
    uza_name == "Miami--Fort Lauderdale, FL" ~ "Miami",
    uza_name == "Washington--Arlington, DC--VA--MD" ~ "Washington DC",
    uza_name == "New York--Jersey City--Newark, NY--NJ" ~ "New York",
    uza_name == "San Diego, CA" ~ "San Diego",
    TRUE ~ uza_name
  ))

REGION_RECOVERY_YOY_TOP_10 <- REGION_RECOVERY_YOY %>%
  mutate(uza_name = factor(uza_name, levels = REGION_RECOVERY_YOY %>% 
                             pull(uza_name)))

REGION_RECOVERY_TOP_10_YOY_graph <- ggplot(data = REGION_RECOVERY_YOY_TOP_10, aes(x = uza_name, y = percentage_growth)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle(paste("Ridership Growth, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs ",year(max(RAIL_BULK$month))-1,"\n10 Largest US Metro Areas by Transit Use", sep = "")) +
  labs(caption = paste("Graph created by @JosephPolitano using FTA Data. Top 10 Selected Based on",year(max(RAIL_BULK$month))-1 ,"Ridership")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = REGION_RECOVERY_TOP_10_YOY_graph, "Region Recovery Top 10 YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


COMMUTER_RAIL_RECOVERY <- COMMUTER_RAIL_BULK %>%
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == 2019 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(REGION_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_recovery = ((get(paste0("value_", recent_year))) / value_2019)) %>%
  ungroup() %>%
  arrange(desc(value_2019))


COMMUTER_RAIL_RECOVERY <- COMMUTER_RAIL_RECOVERY %>%
  slice(1:10) %>%
  arrange(percentage_recovery) %>%
  mutate(agency = case_when(
    agency == "Southern California Regional Rail Authority" ~ "Metrolink (LA)",
    agency == "Maryland Transit Administration" ~ "MTA (Maryland)",
    agency == "Northeast Illinois Regional Commuter Railroad Corporation" ~ "Metra (Chicago)",
    agency == "Peninsula Corridor Joint Powers Board" ~ "Caltrain (Bay Area)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    agency == "New Jersey Transit Corporation" ~ "NJTransit (NJ)",
    agency == "Metro-North Commuter Railroad Company, dba: MTA Metro-North Railroad" ~ "Metro-North (NY)",
    agency == "MTA Long Island Rail Road" ~ "LIRR (NY)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Denver Regional Transportation District" ~ "RTD (Denver)",
    TRUE ~ agency
  ))

COMMUTER_RAIL_RECOVERY_TOP_10 <- COMMUTER_RAIL_RECOVERY %>%
  mutate(agency = factor(agency, levels = COMMUTER_RAIL_RECOVERY %>% 
                           pull(agency)))



COMMUTER_RAIL_RECOVERY_TOP_10_graph <- ggplot(data = COMMUTER_RAIL_RECOVERY_TOP_10, aes(x = agency, y = percentage_recovery)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% of 2019 Ridership") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1), expand = c(0,0)) +
  ggtitle(paste("Ridership Recovery, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs 2019\n10 Largest US Commuter Rail Networks", sep = "")) +
  labs(caption = "Graph created by @JosephPolitano using FTA Data. Top 10 Selected Based on 2019 Ridership") +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = COMMUTER_RAIL_RECOVERY_TOP_10_graph, "Commuter Rail Recovery Top 10 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


COMMUTER_RAIL_RECOVERY_YOY <- COMMUTER_RAIL_BULK %>%
  mutate(year = year(month), month = month(month)) %>%
  group_by(year, month) %>%
  ungroup() %>%
  group_by(agency) %>%
  mutate(recent_year = max(year),
         recent_month = max(month[year == recent_year])) %>%
  filter((year == recent_year & month <= recent_month) | (year == recent_year-1 & month <= recent_month)) %>%
  group_by(agency, year, recent_year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  filter(year >= 2019 & recent_year == year(max(REGION_BULK$month))) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(percentage_growth = ((get(paste0("value_", recent_year))) / get(paste0("value_", recent_year-1)))-1) %>%
  ungroup() %>%
  arrange(desc(get(paste0("value_", recent_year[1]-1))))

COMMUTER_RAIL_RECOVERY_YOY <- COMMUTER_RAIL_RECOVERY_YOY %>%
  slice(1:10) %>%
  arrange(percentage_growth) %>%
  mutate(agency = case_when(
    agency == "Southern California Regional Rail Authority" ~ "Metrolink (LA)",
    agency == "Maryland Transit Administration" ~ "MTA (Maryland)",
    agency == "Northeast Illinois Regional Commuter Railroad Corporation" ~ "Metra (Chicago)",
    agency == "Peninsula Corridor Joint Powers Board" ~ "Caltrain (Bay Area)",
    agency == "Southeastern Pennsylvania Transportation Authority" ~ "SEPTA (Philly)",
    agency == "New Jersey Transit Corporation" ~ "NJTransit (NJ)",
    agency == "Metro-North Commuter Railroad Company, dba: MTA Metro-North Railroad" ~ "Metro-North (NY)",
    agency == "MTA Long Island Rail Road" ~ "LIRR (NY)",
    agency == "Massachusetts Bay Transportation Authority" ~ "MBTA (Boston)",
    agency == "Denver Regional Transportation District" ~ "RTD (Denver)",
    agency == "South Florida Regional Transportation Authority" ~ "Tri-Rail (Miami)",
    TRUE ~ agency
  ))

COMMUTER_RAIL_RECOVERY_YOY_TOP_10 <- COMMUTER_RAIL_RECOVERY_YOY %>%
  mutate(agency = factor(agency, levels = COMMUTER_RAIL_RECOVERY_YOY %>% 
                           pull(agency)))

COMMUTER_RAIL_RECOVERY_TOP_10_YOY_graph <- ggplot(data = COMMUTER_RAIL_RECOVERY_YOY_TOP_10, aes(x = agency, y = percentage_growth)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ylab("% Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  ggtitle(paste("Ridership Growth, Jan-", month.abb[month(max(RAIL_BULK$month))]," ", year(max(RAIL_BULK$month)), " vs ",year(max(RAIL_BULK$month))-1,"\n10 Largest US Commuter Rail Networks", sep = "")) +
  labs(caption = paste("Graph created by @JosephPolitano using FTA Data. Top 10 Selected Based on",year(max(RAIL_BULK$month))-1 ,"Ridership")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 16, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = COMMUTER_RAIL_RECOVERY_TOP_10_YOY_graph, "Commuter Rail Recovery Top 10 YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()


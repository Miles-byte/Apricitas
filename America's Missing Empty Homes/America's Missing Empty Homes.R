pacman::p_load(purrr,zctaCrosswalk,zipcodeR,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,openxlsx,sf,tidycensus,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

counties <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(GEOID = county_fips) %>%
  select(geometry, GEOID)

states <- get_urbn_map("states", sf = TRUE) %>%
  st_as_sf()


VACANCY_AGG_2019 <- get_acs(
  geography = "county",
  #variables = DP04,
  table = "B25002",
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE
) %>%
  transmute(GEOID,NAME,AGG_VACANCY_RATE = B25002_003E/B25002_001E, TOTAL = B25002_001E) %>%
  merge(.,counties)

AGG_VACANCY_2019_Counties <- ggplot() +
  geom_sf(data = VACANCY_AGG_2019 %>% mutate(AGG_VACANCY_RATE = case_when(
    AGG_VACANCY_RATE > 0.20 ~ 0.20,
    TRUE ~ AGG_VACANCY_RATE
  )), aes(fill = AGG_VACANCY_RATE, geometry = geometry)) +
  geom_sf(data = counties, color = "black", fill = NA, lwd = 0.1) + # Black borders for counties
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(0,0.05,0.1,0.15,0.20),
                      labels = c("0%","5%","10%","15%","20%+"),
                      limits = c(0,0.20)) +
  ggtitle("            Total Vacancy Rate 2014-2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Census data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = AGG_VACANCY_2019_Counties, "Aggregate Vacancy Rate 2014-2019.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


IN_DEPTH_VACANCY <- get_acs(
  geography = "county",
  #variables = DP04,
  table = "B25004",
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE
) %>%
  transmute(GEOID,NAME,FOR_RENT = B25004_002E,UNOCCUPIED_RENTED = B25004_003E, FOR_SALE = B25004_004E, SOLD = B25004_005E, SEASONAL = B25004_006E, MIGRANT = B25004_007E, OTHER = B25004_008E)

CURRENT_RES_ELSEWHERE <- get_acs(
  geography = "county",
  table = "B25005",
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE
) %>%
  transmute(GEOID,NAME,CURRENT_RES_ELSEWHERE = B25005_002E)

RENTER_OWNER_OCCUPIED <- get_acs(
  geography = "county",
  table = "B25008",
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE
) %>%
  transmute(GEOID,NAME, TOTAL_OCCUPIED = B25008_001E, OWNER_OCCUPIED = B25008_002E, RENTER_OCCUPIED = B25008_003E)

RENTAL_VACANCY_RATES_2019 <- merge(IN_DEPTH_VACANCY,CURRENT_RES_ELSEWHERE) %>%
  merge(RENTER_OWNER_OCCUPIED) %>%
  mutate(OTHER = OTHER-CURRENT_RES_ELSEWHERE) %>%
  mutate(RENTAL_VACANCY_RATE = FOR_RENT/(RENTER_OCCUPIED + FOR_RENT)) %>%
  merge(.,counties)

RENTAL_VACANCY_2019_Counties <- ggplot() +
  geom_sf(data = RENTAL_VACANCY_RATES_2019 %>% mutate(RENTAL_VACANCY_RATE = case_when(
    RENTAL_VACANCY_RATE > 0.05 ~ 0.05,
    TRUE ~ RENTAL_VACANCY_RATE
  )), aes(fill = RENTAL_VACANCY_RATE, geometry = geometry)) +
  geom_sf(data = counties, color = "black", fill = NA, lwd = 0.1) + # Black borders for counties
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_gradient(high = "#00A99D",
                      low = "#EE6055",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill",
                      breaks = c(0,0.05),
                      labels = c("0%","5%+"),
                      limits = c(0,0.05)) +
  ggtitle("            Total Vacancy Rate 2014-2019") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Census data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = AGG_VACANCY_2019_Counties, "Aggregate Vacancy Rate 2014-2019.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#2019 Rental Vacancy Rates

years <- c(2005:2019,2021,2022)
final_df_IN_DEPTH_VACANCY <- NULL
final_df_CURRENT_RES_ELSEWHERE <- NULL
final_df_RENTER_OWNER_OCCUPIED <- NULL

for(year in years){
  IN_DEPTH_VACANCY <- get_acs(
    geography = "county",
    #variables = DP04,
    table = "B25004",
    cache_table = TRUE,
    year = year,
    output = "wide",
    key = Sys.getenv("CENSUS_KEY"),
    moe_level = 90,
    survey = "acs1",
    show_call = TRUE
  ) %>%
    transmute(GEOID,NAME,FOR_RENT = B25004_002E,UNOCCUPIED_RENTED = B25004_003E, FOR_SALE = B25004_004E, SOLD = B25004_005E, SEASONAL = B25004_006E, MIGRANT = B25004_007E, OTHER = B25004_008E) %>%
    mutate(YEAR = as.Date(paste0(year,"-01-01")))
  
  CURRENT_RES_ELSEWHERE <- get_acs(
    geography = "county",
    table = "B25005",
    cache_table = TRUE,
    year = year,
    output = "wide",
    key = Sys.getenv("CENSUS_KEY"),
    moe_level = 90,
    survey = "acs1",
    show_call = TRUE
  ) %>%
    transmute(GEOID,NAME,CURRENT_RES_ELSEWHERE = B25005_002E) %>%
    mutate(YEAR = as.Date(paste0(year,"-01-01")))
  
  RENTER_OWNER_OCCUPIED <- RENTER_OWNER_OCCUPIED <- get_acs(
    geography = "county",
    table = "B25008",
    cache_table = TRUE,
    year = year,
    output = "wide",
    key = Sys.getenv("CENSUS_KEY"),
    moe_level = 90,
    survey = "acs1",
    show_call = TRUE
  ) %>%
    transmute(GEOID,NAME, TOTAL_OCCUPIED = B25008_001E, OWNER_OCCUPIED = B25008_002E, RENTER_OCCUPIED = B25008_003E) %>%
    mutate(YEAR = as.Date(paste0(year,"-01-01")))
  
  
  final_df_IN_DEPTH_VACANCY <- rbind(final_df_IN_DEPTH_VACANCY, IN_DEPTH_VACANCY)
  final_df_CURRENT_RES_ELSEWHERE <- rbind(final_df_CURRENT_RES_ELSEWHERE, CURRENT_RES_ELSEWHERE)
  final_df_RENTER_OWNER_OCCUPIED <- rbind(final_df_RENTER_OWNER_OCCUPIED, RENTER_OWNER_OCCUPIED)
}

RENTAL_VACANCY_RATES_2005_2022 <- merge(final_df_IN_DEPTH_VACANCY,final_df_CURRENT_RES_ELSEWHERE) %>%
  merge(final_df_RENTER_OWNER_OCCUPIED) %>%
  mutate(OTHER = OTHER-CURRENT_RES_ELSEWHERE) %>%
  mutate(RENTAL_VACANCY_RATE = FOR_RENT/(RENTER_OCCUPIED + FOR_RENT)) %>%
  full_join(.,counties)

RENTAL_VACANCY_RATES_2005_2022<- ggplot() + #plotting rent by A/B/C City Size
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Los Angeles County, California"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Los Angeles"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "New York County, New York"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Manhattan"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Queens County, New York"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Queens"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Bronx County, New York"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Bronx"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Richmond County, New York"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Staten Island"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Kings County, New York"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Brooklyn"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Cook County, Illinois"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Chicago"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "Harris County, Texas"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Houston"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "San Francisco County, California"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "San Francisco"), size = 1.25) +
  geom_line(data=filter(RENTAL_VACANCY_RATES_2005_2022, NAME == "King County, Washington"), aes(x=YEAR,y= RENTAL_VACANCY_RATE, color= "Seattle"), size = 1.25) +
  geom_line(data=CPIRENTBC, aes(x=date,y= calculations/100, color= "CPI Rent of Primary Residence: Medium/Small Metro Areas"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.08), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("Superstar Upset") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices Growth in Smaller Metros Has Caught Up to Larger Metros") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*.08), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


states_list <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

states_data_list <- map(states_list, function(st) {
  df <- fredr(series_id = paste0(st, "RVAC"), observation_start = as.Date("1986-01-01")) %>%
    mutate(name = st)
  
  change_since_2019 <- df %>% 
    filter(date >= as.Date("2019-01-01")) %>%
    arrange(desc(date)) %>%
    slice_head(n = 1) %>%
    pull(value) - 
    df %>% 
    filter(date == as.Date("2019-01-01")) %>%
    pull(value)
  
  df %>%
    mutate(change_since_2019 = ifelse(date == max(date), change_since_2019, NA))
})

states_data <- bind_rows(states_data_list)

states <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf() %>%
  mutate(name = state_abbv)

states_data <- full_join(states_data, states, by = "name")

VACANCY_CHANGE_2019_STATE <- states_data %>%
  drop_na() %>%
  mutate(change_since_2019_bucket = cut(change_since_2019, breaks = c(-4.51, -3, -1.5, 0, 1.5, 3.01), labels = c("-4.5% to -3%", "-1.5% to -3%", "0 to -1.5%", "0 to 1.5%", "1.5% to 3%")))

VACANCY_CHANGE_2019_STATE_graph <- ggplot() +
  geom_sf(data = VACANCY_CHANGE_2019_STATE, aes( fill = change_since_2019_bucket, geometry = geometry), color = NA) +
  geom_sf(data = VACANCY_CHANGE_2019_STATE, aes(geometry = geometry), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("-4.5% to -3%", "-1.5% to -3%", "0 to -1.5%", "0 to 1.5%", "1.5% to 3%")) +
  ggtitle("     Growth in Rental Vacancy Rate: 2019-2022") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank())




ggsave(dpi = "retina",plot = VACANCY_CHANGE_2019_STATE_graph, "Vacancy Change 2019 2022.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RENT_BURDEN_RATE <- get_acs(
  geography = "us",
  table = "B25070",
  cache_table = TRUE,
  year = 2022,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs1",
  show_call = TRUE
) %>%
  mutate(GEOID,NAME, SEVERE_RENT_BURDEN = (B25070_010E)/(B25070_001E-B25070_011E), RENT_BURDEN = (B25070_010E+B25070_009E+B25070_008E+B25070_007E)/(B25070_001E-B25070_011E))

OWNER_BURDEN_RATE <- get_acs(
  geography = "us",
  table = "B25091",
  cache_table = TRUE,
  year = 2022,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs1",
  show_call = TRUE
) %>%
  mutate(GEOID,NAME, SEVERE_OWNER_BURDEN = (B25091_022E + B25091_011E)/(B25091_001E-B25091_012E-B25091_023E), OWNER_BURDEN = (B25091_021E+B25091_020E+B25091_019E+B25091_010E+B25091_009E+B25091_008E)/(B25091_001E-B25091_012E-B25091_023E))


years <- c(2005:2019,2021,2022)
final_df_RENT_BURDEN_RATE <- NULL
final_df_OWNER_BURDEN_RATE <- NULL


for(year in years){
  RENT_BURDEN_RATE <- get_acs(
    geography = "us",
    table = "B25070",
    cache_table = TRUE,
    year = year,
    output = "wide",
    key = Sys.getenv("CENSUS_KEY"),
    moe_level = 90,
    survey = "acs1",
    show_call = TRUE
  ) %>%
    mutate(GEOID,NAME, SEVERE_RENT_BURDEN = (B25070_010E)/(B25070_001E-B25070_011E), RENT_BURDEN = (B25070_009E+B25070_008E+B25070_007E)/(B25070_001E-B25070_011E)) %>%
    mutate(YEAR = as.Date(paste0(year,"-01-01")))
  
  OWNER_BURDEN_RATE <- get_acs(
    geography = "us",
    table = "B25091",
    cache_table = TRUE,
    year = year,
    output = "wide",
    key = Sys.getenv("CENSUS_KEY"),
    moe_level = 90,
    survey = "acs1",
    show_call = TRUE
  ) %>%
    mutate(GEOID,NAME, SEVERE_OWNER_BURDEN = (B25091_022E + B25091_011E)/(B25091_001E-B25091_012E-B25091_023E), OWNER_BURDEN = (B25091_021E+B25091_020E+B25091_019E+B25091_010E+B25091_009E+B25091_008E)/(B25091_001E-B25091_012E-B25091_023E)) %>%
    mutate(YEAR = as.Date(paste0(year,"-01-01")))
  
  final_df_RENT_BURDEN_RATE <- rbind(final_df_RENT_BURDEN_RATE, RENT_BURDEN_RATE)
  final_df_OWNER_BURDEN_RATE <- rbind(final_df_OWNER_BURDEN_RATE, OWNER_BURDEN_RATE)
}

RENT_OWNER_MERGE <- merge(final_df_RENT_BURDEN_RATE,final_df_OWNER_BURDEN_RATE, by = "YEAR") %>%
  transmute(`Moderate (50%+ of Income) Ownership Burden` = B25091_022E + B25091_011E, `Moderate (30-49.9% of Income) Ownership Burden` = B25091_021E+B25091_020E+B25091_019E+B25091_010E+B25091_009E+B25091_008E, `Severely (50%+ of Income) Rent-Burdened` = B25070_010E, `Moderately (30-49.9% of Income) Rent-Burdened` = B25070_009E+B25070_008E+B25070_007E, TOTAL = B25091_001E-B25091_012E-B25091_023E+B25070_001E-B25070_011E, YEAR) %>%
  mutate(across(where(is.numeric), ~ . / TOTAL)) %>%
  select(-TOTAL) %>%
  pivot_longer(cols = -YEAR)

RENT_BURDEN_RATE_BAR_graph<- ggplot(data = RENT_OWNER_MERGE %>% filter(name %in% c("Severely (50%+ of Income) Rent-Burdened","Moderately (30-49.9% of Income) Rent-Burdened")), aes(x = YEAR, y = value, fill = name)) + #plotting permanent and temporary job losers
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Percent of Total Households") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(0,0.05,0.1,0.15,.20), limits = c(0,0.215), expand = c(0,0)) +
  ggtitle("US Rent Burdens are Rising") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "The Share of Households that are Rent-Burdened Has Risen Post-Pandemic") +
  theme_apricitas + theme(legend.position = c(.52,.95)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2004-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0-(.3*.215), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RENT_BURDEN_RATE_BAR_graph, "Rent Burden Bar.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RENT_BURDEN_RATE_LINE_graph <- ggplot(data = RENT_OWNER_MERGE %>% filter(name %in% c("Severely (50%+ of Income) Rent-Burdened","Moderately (30-49.9% of Income) Rent-Burdened")), aes(x = YEAR, y = value, color = name)) + #plotting permanent and temporary job losers
  geom_line(size = 1.25) +
  xlab("Date") +
  ylab("Percent of Total Households") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), breaks = c(0.075,0.08,0.085,0.09,0.095,0.10), limits = c(0.075,0.095), expand = c(0,0)) +
  ggtitle("US Rent Burdens are Rising") +
  labs(caption = "Graph created by @JosephPolitano using Census data", subtitle = "The Share of Households that are Rent-Burdened Has Risen Post-Pandemic") +
  theme_apricitas + theme(legend.position = c(.62,.15)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-01-01")-(.1861*(today()-as.Date("2005-01-01"))), xmax = as.Date("2005-01-01")-(0.049*(today()-as.Date("2005-01-01"))), ymin = 0.075-(.3*.02), ymax = 0.075) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RENT_BURDEN_RATE_LINE_graph, "Rent Burden Line.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


B25136
B25131
b25130

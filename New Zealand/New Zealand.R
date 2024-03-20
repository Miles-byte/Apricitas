pacman::p_load(tigris,sf,readabs,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

devtools::install_github("StatisticsNZ/open-data-api")

source("get-opendata-catalogue-fun.R")

library("httr")
library("jsonlite")


# function to call the stats nz open data api

get_odata <-  function(service, endpoint, entity, query_option, service_api_key) {
  
  config_proxy <- use_proxy(
    url = curl::ie_get_proxy_for_url(service),
    auth = "any",
    username = ""
  )
  
  odata_url <- URLencode(paste0(service, "/", endpoint, "/", entity, "?", query_option))
  top_query <- grepl("$top",query_option,fixed=TRUE)
  
  # continue getting results while there are additional pages
  
  while (!is.null(odata_url)) {
    
    result <- GET(odata_url,
                  config_proxy,
                  add_headers(.headers = c("Content-Type" = "application/json;charset=UTF-8",
                                           "Ocp-Apim-Subscription-Key" = service_api_key)),
                  timeout(60)
    )
    
    
    # catch errors
    
    if (http_type(result) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
    
    
    if (http_error(result)) {
      stop(
        sprintf(
          "The request failed - %s \n%s \n%s ",
          http_status(result)$message,
          fromJSON(content(result, "text"))$value,
          odata_url
        ),
        call. = FALSE
      )
    }
    
    
    # parse and concatenate result while retaining UTF-8 encoded characters
    
    parsed <- jsonlite::fromJSON(content(result, "text", encoding = "UTF-8"), flatten = TRUE)
    response  <- rbind(parsed$value, if(exists("response")) response)
    odata_url <- parsed$'@odata.nextLink'
    
    
    cat("\r", nrow(response), "obs retrieved")
    
    # break when top(n) obs are specified
    
    if (top_query) {
      break
    }
    
  }
  
  structure(response,
            comment = "Odata response")
  
}

get_odata_catalogue <-  function(service, endpoint, service_api_key) {
  
  catalogue_url <- URLencode(paste0(service, "/", endpoint))
  
  # Add the proxy authentication
  config_proxy <- use_proxy(
    url = curl::ie_get_proxy_for_url(service), 
    auth = "any", 
    username = ""
  )
  
  # Look at the available tables
  opendata_catalogue <- 
    GET(
      url = catalogue_url,
      config_proxy,
      add_headers(.headers = c('Cache-Control' = 'no-cache',
                               'Ocp-Apim-Subscription-Key' = service_api_key)),
      timeout(60)
    ) %>%
    content(as = "text") %>%
    fromJSON()
  
  opendata_catalogue <- as.data.frame(opendata_catalogue$dataset) %>%
    unnest_longer(distribution)
  
  
  structure(opendata_catalogue,
            comment = "Odata Catalogue")
  
}

Catalogue <- get_odata_catalogue(
  service="https://api.stats.govt.nz/odata/v1",
  endpoint="data.json",
  service_api_key = "05a63a33eec048ce86da17602ab887b0"
)

Observations <-  Filter(function(x)!all(is.na(x)),
                        get_odata(
                          service = "https://api.stats.govt.nz/odata/v1",
                          endpoint = "EmploymentIndicators",
                          entity = "Observations",
                          query_option = "$select=ResourceID,Period,Duration,Label1,Label2,Value,Unit,Measure,Multiplier&$top=10",
                          service_api_key = "05a63a33eec048ce86da17602ab887b0"))

#Infoshare > Industry Sectors

AUCKLAND_REGION_CONSENTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/AUCKLAND_REGION_CONSENTS.csv") %>%
  mutate(Year = as.Date(paste0(Year,"-01-01"))) %>%
  setNames(c("date", "total", "Detached Single-Family Homes", "Multi-Family Homes", "Multi-Story Apartments", "Retirement Villages", "Townhouses, Terraced Housing, Duplexes, and Other Units")) %>%
  select(-total,-`Multi-Family Homes`) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = c("Detached Single-Family Homes","Retirement Villages","Multi-Story Apartments","Townhouses, Terraced Housing, Duplexes, and Other Units")))
  
AUCKLAND_REGION_CONSENTS_GRAPH <- ggplot(AUCKLAND_REGION_CONSENTS, aes(x = date, y = value/1000, fill = name)) + #plotting auckland dwelling consents
  annotate(geom = "text", label = "2005\nDownzone", x = as.Date("2004-04-01"), y = 14.8, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2004-07-01"), xend = as.Date("2004-07-01"), y = 0, yend = 15, color = "white", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2013\nSpHA\nPartial\nUpzone", x = as.Date("2013-04-01"), y = 14, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2013-07-01"), xend = as.Date("2013-07-01"), y = 0, yend = 15, color = "white", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2016\nAUP\nFull\nUpzone", x = as.Date("2016-04-01"), y = 14, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2016-07-01"), xend = as.Date("2016-07-01"), y = 0, yend = 15, color = "white", lwd = 0.75, linetype = "dashed") +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Dwelling Consents") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ggtitle("Auckland's Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "Auckland's Dwelling Consents Hit a Record High In 2022, Driven by Rising Townhouse Construction") +
  theme_apricitas + theme(legend.position = c(.38,.839), legend.spacing.y = unit(0,"cm")) +
  scale_fill_manual(name= "Dwelling Consents by Year, Auckland",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1991-01-01")-(.1861*(today()-as.Date("1991-03-01"))), xmax = as.Date("1991-01-01")-(0.049*(today()-as.Date("1991-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AUCKLAND_REGION_CONSENTS_GRAPH, "Auckland Region Consents.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

POPULATION_US <- fredr("POPTHM", frequency = "a", aggregation_method = "avg")

PERMITS_US <- fredr("PERMIT", frequency = "a", aggregation_method = "avg") %>%
  mutate(date = as.Date(paste0(date,"-01-01")))

PERMITS_PER_US <- merge(POPULATION_US,PERMITS_US, by = "date") %>%
  drop_na() %>%
  transmute(date, value = value.y/value.x*1000) %>%
  filter(date >= as.Date("1984-01-01"))

POPULATION_AU <- read_abs(series_id = "A2133251W") %>%
  filter(month(date) == 6) %>%
  mutate(date = update(date, month = 1)) %>%
  select(date, value)

PERMITS_AU <- read_abs(series_id = "A418427K") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(n() == 12) %>%
  summarise(
    date = as.Date(paste0(year, "-01-01")),
    value = sum(value)
  ) %>%
  ungroup() %>%
  unique() %>%
  select(-year)

PERMITS_PER_AU <- merge(POPULATION_AU,PERMITS_AU, by = "date") %>%
  drop_na() %>%
  transmute(date, value = value.y/value.x)

PERMITS_PER_NZ_REGIONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/NZ_PERMITS_PER_CAPITA.csv") %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  mutate(date = as.Date(paste0(date,"-01-01")))
  
PERMITS_PER_CAPITA_NZ_US_AU <- ggplot() + #plotting power generation
  geom_line(data=PERMITS_PER_US, aes(x=date,y= value,color= "United States"), size = 1.25) + 
  geom_line(data=PERMITS_PER_AU, aes(x=date,y= value,color= "Australia"), size = 1.25) + 
  geom_line(data=filter(PERMITS_PER_NZ_REGIONS, date>= as.Date("1984-01-01")), aes(x=date,y= New_Zealand,color= "New Zealand"), size = 1.25) + 
  xlab("Date") +
  ylab("Permits Per 1000 Residents") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,2,4,6,8,10), limits = c(0,10), expand = c(0,0)) +
  ggtitle("New Zealand's Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ, ABS, and US Census data", subtitle = "New Zealand's Housing Permits Per-Capita Hit a 45-Year High in 2022—in Stark Contrast to the US") +
  theme_apricitas + theme(legend.position = c(.5,.85)) +
  scale_color_manual(name= "Housing Permits Per 1000 Residents",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("New Zealand","United States","Australia")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1984-09-01")-(.1861*(today()-as.Date("1984-01-01"))), xmax = as.Date("1984-01-01")-(0.049*(today()-as.Date("1984-01-01"))), ymin = 0-(.3*10), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PERMITS_PER_CAPITA_NZ_US_AU, "Permits Per Capita NZ US AU.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PERMITS_PER_NZ_TERRITORIAL_AUTHORITIES <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/PERMITS_PER_TERRITORIAL_AUTHORITY.csv") %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  mutate(date = as.Date(paste0(date,"-01-01")))

PERMITS_PER_CAPITA_AUCK_WELL_Graph <- ggplot() + #plotting power generation
  geom_line(data=filter(PERMITS_PER_NZ_TERRITORIAL_AUTHORITIES, date >= as.Date("1996-01-01")), aes(x=date,y= Auckland,color= "Auckland"), size = 1.25) + 
  geom_line(data=filter(PERMITS_PER_NZ_TERRITORIAL_AUTHORITIES, date >= as.Date("1996-01-01")), aes(x=date,y= Wellington_City,color= "Wellington"), size = 1.25) + 
  annotate(geom = "text", label = "2005\nDownzone", x = as.Date("2004-04-01"), y = 10.4, color ="#FFE98F",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2004-07-01"), xend = as.Date("2004-07-01"), y = 0, yend = 11, color = "#FFE98F", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2013\nSpHA\nPartial\nUpzone", x = as.Date("2013-04-01"), y = 10, color ="#FFE98F",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2013-07-01"), xend = as.Date("2013-07-01"), y = 0, yend = 11, color = "#FFE98F", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2016\nAUP\nFull\nUpzone", x = as.Date("2016-04-01"), y = 10, color ="#FFE98F",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2016-07-01"), xend = as.Date("2016-07-01"), y = 0, yend = 11, color = "#FFE98F", lwd = 0.75, linetype = "dashed") +
  xlab("Date") +
  ylab("Permits Per 1000 Residents") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,4,8,12), limits = c(0,14), expand = c(0,0)) +
  ggtitle("Auckland's Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "Auckland's Permitting Rate Has Surged Well Above Wellington's in the Wake of Upzonings") +
  theme_apricitas + theme(legend.position = c(.35,.9)) +
  scale_color_manual(name= "Housing Permits Per 1000 Residents (Territorial Authority)",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Auckland","Wellington")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1996-06-01")-(.1861*(today()-as.Date("1996-01-01"))), xmax = as.Date("1996-01-01")-(0.049*(today()-as.Date("1996-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PERMITS_PER_CAPITA_AUCK_WELL_Graph, "Permits Per Capita Auck Well Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#LOWER HUTT PERMITS BY TYPE

WELLINGTON_REGIONAL_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/WELLINGTON_REGION_PERMIT_DATA.csv") %>%
  setNames(c("date", "Total", "Detached Single-Family Homes", "Multi-Story Apartments", "Retirement Villages", "Townhouses, Terraced Housing, Duplexes, etc", "Authority")) %>%
  mutate(date = as.Date(paste0(date,"-01-01"))) %>%
  pivot_longer(cols = `Total`:`Townhouses, Terraced Housing, Duplexes, etc`) %>%
  filter(name == "Total" | Authority == "Lower Hutt City") %>%
  mutate(Authority_group = case_when(
    Authority == "Wellington City" ~ "Wellington City",
    Authority == "Lower Hutt City" ~ "Lower Hutt City",
    Authority %in% c("Wellington City", "Lower Hutt City") ~ Authority,
    TRUE ~ "Other Areas in Wellington Region"
  )) %>%
  group_by(Authority_group, date, name) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(is_adjustment_needed = Authority_group == "Lower Hutt City" & (name == "Total" | name == "Townhouses, Terraced Housing, Duplexes, and Other Units" | name == "Multi-Story Apartments")) %>%
  mutate(Townhouses_apt_value = if_else(Authority_group == "Lower Hutt City" & name %in% c("Townhouses, Terraced Housing, Duplexes, etc","Multi-Story Apartments"), value, 0),
         Adjust_value = sum(Townhouses_apt_value)) %>%
  # Apply conditional adjustment
  mutate(value = if_else(Authority_group == "Lower Hutt City" & name == "Total", value - Adjust_value, value),
         # Rename "Total" to "All Other Units" for "Lower Hutt City"
         name = if_else(Authority_group == "Lower Hutt City" & name == "Total", "Detached Single-Family Homes and Retirement Villages", name)) %>%
  select(Authority_group, date, name, value) %>%
  filter(name %in% c('Total',"Detached Single-Family Homes and Retirement Villages","Multi-Story Apartments","Townhouses, Terraced Housing, Duplexes, etc")) %>%
  mutate(Authority_group = if_else(Authority_group == "Lower Hutt City",
                                   paste0("Lower Hutt: ", name),
                                   Authority_group)) %>%
  select(-name) %>%
  setNames(c("name", "date", "value")) %>%
  mutate(name = factor(name, levels = rev(c("Lower Hutt: Townhouses, Terraced Housing, Duplexes, etc","Lower Hutt: Multi-Story Apartments", "Lower Hutt: Detached Single-Family Homes and Retirement Villages","Wellington City","Other Areas in Wellington Region"))))

WELLINGTON_REGIONAL_DATA_GRAPH <- ggplot(WELLINGTON_REGIONAL_DATA, aes(x = date, y = value/1000, fill = name)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA) +
  annotate(geom = "text", label = "2017\nPlan Change 43\nPartial Lower Hutt Upzone", x = as.Date("2017-04-01"), y = 2.7, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2017-07-01"), xend = as.Date("2017-07-01"), y = 0, yend = 2.7, color = "white", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2020\nPlan Change 43\nFull Upzone\n& NPS-UD\nParking Reforms", x = as.Date("2020-04-01"), y = 3.80, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2020-07-01"), xend = as.Date("2020-07-01"), y = 0, yend = 3.80, color = "white", lwd = 0.75, linetype = "dashed") +
  xlab("Date") +
  ylab("Units Permitted by Year") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(1,2,3,4,5), limits = c(0,5.5), expand = c(0,0)) +
  ggtitle("Lower Hutt City's Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "Permits in the Broader Wellington Region Have Been Driven by Upzonings in Lower Hutt City") +
  theme_apricitas + theme(legend.position = c(.37,.82), legend.spacing.y = unit(0,"cm"), legend.text = element_text(size = 12.5), legend.title = element_text(size = 13.5)) +
  scale_fill_manual(name= "Units Permitted by Year, Wellington Region",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#3083DC","#A7ACD9"), breaks = c("Lower Hutt: Townhouses, Terraced Housing, Duplexes, etc","Lower Hutt: Multi-Story Apartments","Lower Hutt: Detached Single-Family Homes and Retirement Villages","Wellington City","Other Areas in Wellington Region")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1991-01-01")-(.1861*(today()-as.Date("1991-01-01"))), xmax = as.Date("1991-01-01")-(0.049*(today()-as.Date("1991-01-01"))), ymin = 0-(.3*5.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = WELLINGTON_REGIONAL_DATA_GRAPH, "Wellington Regional Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


  

PERMITS_PER_CAPITA_NZ_REGIONS <- ggplot() + #plotting power generation
  geom_line(data=filter(PERMITS_PER_NZ_REGIONS, date >= as.Date("1996-01-01")), aes(x=date,y= Wellington_Region,color= "City of Wellington"), size = 1.25) + 
  geom_line(data=filter(PERMITS_PER_NZ_REGIONS, date >= as.Date("1996-01-01")), aes(x=date,y= Auckland_Region,color= "Auckland"), size = 1.25) + 
  xlab("Date") +
  ylab("Permits Per 1000 Residents") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,4,8,12), limits = c(0,14), expand = c(0,0)) +
  ggtitle("New Zealand Region Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ and US Census data", subtitle = "New Zealand's Housing Permits Per-Capita Hit a 45-Year High in 2022—in Stark Contrast to the US") +
  theme_apricitas + theme(legend.position = c(.42,.81)) +
  scale_color_manual(name= "Housing Permits Per 1000 Residents",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Auckland","Canterbury","Wellington")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*(today()-as.Date("1967-01-01"))), xmax = as.Date("1996-01-01")-(0.049*(today()-as.Date("1996-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

#ggsave(dpi = "retina",plot = PERMITS_PER_CAPITA_NZ_REGIONS, "Permits Per Capita NZ Regions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


LA_PERMIT_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/LA_PERMITS_BY_TYPE.csv") %>%
  mutate(date = as.Date(date)) %>%
  setNames(c("date","Single-Family Homes", "Accessory Dwelling Units (ADUs)", "2-4 Unit Multifamily Buildings", "5+ Unit Multifamily Buildings")) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Accessory Dwelling Units (ADUs)","Single-Family Homes","2-4 Unit Multifamily Buildings","5+ Unit Multifamily Buildings"))))

LA_PERMIT_GRAPH <- ggplot(LA_PERMIT_DATA, aes(x = date, y = value/1000, fill = name)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  annotate(geom = "text", label = "CA ADU Reforms\n(SB 229 & AB 494)", x = as.Date("2015-09-01"), y = 19, color ="white",size = 4, lineheight = unit(0.75, "cm")) + 
  annotate(geom = "segment", x = as.Date("2016-07-01"), xend = as.Date("2016-07-01"), y = 0, yend = 21, color = "white", lwd = 0.75, linetype = "dashed") +
  ylab("Units Permitted by Year") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,27.5), expand = c(0,0)) +
  ggtitle("Los Angeles' ADU Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using LA City Planning data", subtitle = "LA Home Permitting Rose to 23k in 2022, Driven by a Long Boom in ADU Construction") +
  theme_apricitas + theme(legend.position = c(.42,.89)) +
  scale_fill_manual(name= "Units Permitted by Year, City of LA",values = c("#00A99D","#FFE98F","#9A348E","#EE6055","#A7ACD9","#3083DC"), breaks = c("Accessory Dwelling Units (ADUs)","Single-Family Homes","2-4 Unit Multifamily Buildings","5+ Unit Multifamily Buildings")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*27.5), ymax = 0) +
  guides(fill = guide_legend(ncol = 2)) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LA_PERMIT_GRAPH, "LA Permits Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CONSTRUCTION_EMPLOYMENT_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/ALL_EMPLOYEES_CONSTRUCTION_NZ.csv") %>%
  filter(date > 0) %>%
  transmute(date = as.Date(paste0(date, "-01-01")), Auckland, NZ)

CONSTRUCTION_EMPLOYMENT_Graph <- ggplot() + #plotting power generation
  geom_line(data=filter(CONSTRUCTION_EMPLOYMENT_DATA, date >= as.Date("1996-01-01")), aes(x=date,y= Auckland,color= "Auckland, New Zealand"), size = 1.25) + 
  geom_line(data=filter(CONSTRUCTION_EMPLOYMENT_DATA, date >= as.Date("1996-01-01")), aes(x=date,y= NZ,color= "New Zealand Total"), size = 1.25) + 
  xlab("Date") +
  ylab("Employment Level") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200,300), limits = c(0,350), expand = c(0,0)) +
  ggtitle("New Zealand Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "New Zealand & Auckland Construction Employment Hit a Record High in 2023") +
  theme_apricitas + theme(legend.position = c(.42,.81)) +
  scale_color_manual(name= "Employment Level, Construction",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("New Zealand Total","Auckland, New Zealand")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*350), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_EMPLOYMENT_Graph, "Construction Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

RPI_NZ_DATA <- read.csv("https://www.stats.govt.nz/assets/Uploads/Selected-price-indexes/Selected-price-indexes-February-2024/Download-data/selected-price-indexes-february-2024.csv") %>%
  filter(Series_title_2 == "Actual rentals for housing" & Series_title_3 == "Flow") %>%
  filter(str_detect(Period, "\\.1$")) %>%
  transmute(date = ymd(paste0(Period, ".01")),value = Data_value, name = Series_title_1) %>%
  pivot_wider(values_from = value, names_from = name)

RIP_Graph <- ggplot() + #RPI NZ
  geom_line(data=RPI_NZ_DATA, aes(x=date,y= Auckland,color= "Auckland, New Zealand"), size = 1.25) + 
  geom_line(data=RPI_NZ_DATA, aes(x=date,y= Wellington,color= "Wellington"), size = 1.25) + 
  geom_line(data=RPI_NZ_DATA, aes(x=date,y= National,color= "National"), size = 1.25) + 
  geom_line(data=RPI_NZ_DATA, aes(x=date,y= `Rest of North Island`,color= "Canterbury"), size = 1.25) + 
  geom_line(data=RPI_NZ_DATA, aes(x=date,y= Canterbury,color= "Rest of North Island"), size = 1.25) + 
  geom_line(data=RPI_NZ_DATA, aes(x=date,y= `Rest of South Island`,color= "Rest of South Island"), size = 1.25) + 
  
  xlab("Date") +
  ylab("Employment Level") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200,300), limits = c(0,350), expand = c(0,0)) +
  ggtitle("New Zealand Construction Employment") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "New Zealand & Auckland Construction Employment Hit a Record High in 2023") +
  theme_apricitas + theme(legend.position = c(.42,.81)) +
  scale_color_manual(name= "Employment Level, Construction",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("New Zealand Total","Auckland, New Zealand")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*350), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_EMPLOYMENT_Graph, "Construction Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PUBLIC_HOUSING_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/PUBLIC_PERMITS_DATA.csv") %>%
  mutate(date = as.Date(date)) %>%
  setNames(c("date", "Central Government","Local Government")) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Central Government","Local Government"))))
  
PUBLIC_HOUSING_GRAPH <- ggplot(PUBLIC_HOUSING_DATA, aes(x = date, y = value/1000, fill = name)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  annotate(geom = "text", label = "2013\nSpHA\nPartial\nUpzone", x = as.Date("2013-04-01"), y = 2.75, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2013-07-01"), xend = as.Date("2013-07-01"), y = 0, yend = 3, color = "white", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2016\nAUP\nFull\nUpzone", x = as.Date("2016-04-01"), y = 2.75, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2016-07-01"), xend = as.Date("2016-07-01"), y = 0, yend = 3, color = "white", lwd = 0.75, linetype = "dashed") +
  ylab("Units Permitted by Year") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,1,2,3,4), limits = c(0,4), expand = c(0,0)) +
  ggtitle("New Zealand's Public Housing Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "NZ Public Housing Construction Intensified After the Auckland Upzonings") +
  theme_apricitas + theme(legend.position = c(.42,.88)) +
  scale_fill_manual(name= "Units Permitted by Year, New Zealand",values = c("#00A99D","#FFE98F","#9A348E","#EE6055","#A7ACD9","#3083DC"), breaks = c("Central Government","Local Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*4), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PUBLIC_HOUSING_GRAPH, "Public Housing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PERMITS_NYC_MSA <- fredr("NEWY636BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_LA_MSA <- fredr("LOSA106BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_CHI_MSA <- fredr("CHIC917BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_DAL_MSA <- fredr("DALL148BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_HOU_MSA <- fredr("HOUS448BP1FH", frequency = "a", aggregation_method = "avg")


#LOOK UP COST DATA


#FLOOR AREA CONSENTED DATA


#GDP REGIONALLY AND GDP PER CAPITA DATA

#RENTAL PRICE INDEX

#Download
shapefile_path <- "path_to_your_shapefile"
NZ_REGIONS_SF <- st_read("C:/Users/Joseph/Downloads/regional-council-2023-clipped-generalised.shp")

graph_dates <- seq(as.Date("2000-01-01"), as.Date("2023-01-01"), by="year")

for (i in 1:length(graph_dates)) {
  single_date <- graph_dates[i]

PERMITS_PER_NZ_REGIONS_EDIT <- PERMITS_PER_NZ_REGIONS %>%
  #select(-New_Zealand) %>%
  #filter(date == max(date)) %>%
  pivot_longer(-date) %>%
  drop_na() %>%
  #select(-date) %>%
  mutate(name = gsub("\\_"," ",name)) %>%
  mutate(name = gsub("e s","e's",name)) %>% #editing hawks bay
  mutate(name = gsub("tu Wh","tu-Wh",name)) %>% #editing Manawatu_Whanganui_Region
  left_join(NZ_REGIONS_SF, by = c("name" = "REGC2023_2")) %>%
  filter(date == single_date) %>%
  mutate(date = year(date))

PERMITS_PER_NZ_REGIONS_MAP <- ggplot() +
  geom_sf(data = PERMITS_PER_NZ_REGIONS_EDIT, aes(fill = value, geometry = geometry)) +
  geom_sf(data = PERMITS_PER_NZ_REGIONS_EDIT, color = "black", fill = NA, lwd = 0.35, aes(geometry = geometry)) + # Black borders for states
  geom_text(data = subset(PERMITS_PER_NZ_REGIONS_EDIT, name == "New Zealand"),
            aes(label = paste0("Total: ", value)),
            x = 930000, y = 6000000, color ="white",size = 9, fontface = "bold", lineheight = unit(0.85,"cm"), hjust = 0) +
  annotate(geom = "segment", x = 1770000, xend = 1990000, y = 5420000, yend = 5200000, color = "white", lwd = 1.25) +
  #annotate(geom = "text", label = "Wellington", x = 1990000, y = 5180000, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm")) +
  geom_text(data = subset(PERMITS_PER_NZ_REGIONS_EDIT, name == "Wellington Region"),
            aes(label = paste0("Wellington: ", value)),
            x = 1820000, y = 5180000, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm"), hjust = 0) +
  annotate(geom = "segment", x = 1570000, xend = 1810000, y = 5180000, yend = 5010000, color = "white", lwd = 1.25) +
  #annotate(geom = "text", label = "Christchurch\n(Canterbury Region)", x = 1810000, y = 4960000, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm")) +
  geom_text(data = subset(PERMITS_PER_NZ_REGIONS_EDIT, name == "Canterbury Region"),
            aes(label = paste0("Christchurch: ", value,"\n(Canterbury Region)")),
            x = 1620000, y = 4960000, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm"), hjust = 0) +
  annotate(geom = "segment", x = 1500000, xend = 1760000, y = 5800000, yend = 5920000, color = "white", lwd = 1.25) +
  #annotate(geom = "text", label = "Auckland", x = 1500000, y = 5770000, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm")) +
  geom_text(data = subset(PERMITS_PER_NZ_REGIONS_EDIT, name == "Auckland Region"),
            aes(label = paste0("Auckland: ", value)),
            x = 1310000, y = 5770000, color ="white",size = 5, fontface = "bold", lineheight = unit(0.85,"cm"), hjust = 0) +
  scale_fill_viridis_c(limits = c(1.2,13.6), breaks = c(2,4,6,8,10,12)) +
  coord_sf(xlim = c(500000, 2300000), expand = FALSE) +
  ggtitle(paste0("NZ Housing Permits Per 1000 Residents: ",PERMITS_PER_NZ_REGIONS_EDIT$date[1])) +
  theme(plot.title = element_text(size = 18)) +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right",axis.title.y = element_blank() ,axis.title.x = element_blank(), panel.grid.major=element_blank(),panel.grid.minor = element_blank(),  axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(plot.margin= grid::unit(c(0.2, -0.4, 0, -2), "in"), legend.key = element_blank()) 
  #theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, -2.5), "in")) +

ggsave(dpi = "retina",plot = PERMITS_PER_NZ_REGIONS_MAP, paste0("Permits Per NZ Region Map ",PERMITS_PER_NZ_REGIONS_EDIT$date[1],".png"), type = "cairo-png", width = 9.02, height = 5.76, units = "in")

}

AUCKLAND_BOARD_SF <- st_read("C:/Users/Joseph/Downloads/territorial-authority-local-board-2023-clipped-generalised.shp") %>%
  slice(-1:-3)

AUCKLAND_BOARD_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/AUCKLAND_LOCAL_BOARD_CONSENTS.csv")


for (i in 1:length(graph_dates)) {
  single_date <- graph_dates[i]

PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT <- AUCKLAND_BOARD_DATA %>%
  rename_with(~ gsub("\\.", " ", .x)) %>%
  #select(-New_Zealand) %>%
  #filter(date == 2022) %>%
  pivot_longer(-date) %>%
  mutate(date = as.Date(paste0(date, "-01-01"))) %>%
  #select(-date) %>%
  mutate(name = gsub("\\_"," ",name)) %>%
  mutate(name = gsub("son Mas","son-Mas",name)) %>% #editing hawks bay
  mutate(name = gsub("port Taka","port-Taka",name)) %>% #
  mutate(name = gsub("Albert Eden","Albert-Eden",name)) %>% #
  mutate(name = gsub("Mangere Otahuhu","Mangere-Otahuhu",name)) %>% #
  mutate(name = gsub("Otara Papatoetoe","Otara-Papatoetoe",name)) %>% #
  mutate(name = gsub("Maungakiekie Tamak","Maungakiekie-Tamak",name)) %>% #
  left_join(AUCKLAND_BOARD_SF, by = c("name" = "TALB2023_2")) %>%
  drop_na() %>%
  mutate(across(is.numeric, ~ifelse(. > 25, 25, .))) %>%
  filter(date == single_date) %>%
  mutate(date = year(date))
  
PERMITS_PER_NZ_REGIONS_EDIT <- PERMITS_PER_NZ_REGIONS %>%
  #select(-New_Zealand) %>%
  #filter(date == max(date)) %>%
  pivot_longer(-date) %>%
  drop_na() %>%
  #select(-date) %>%
  mutate(name = gsub("\\_"," ",name)) %>%
  mutate(name = gsub("e s","e's",name)) %>% #editing hawks bay
  mutate(name = gsub("tu Wh","tu-Wh",name)) %>% #editing Manawatu_Whanganui_Region
  left_join(NZ_REGIONS_SF, by = c("name" = "REGC2023_2")) %>%
  filter(date == single_date) %>%
  mutate(date = year(date))

PERMITS_PER_AUCKLAND_BOARD_MAP <- ggplot() +
  geom_sf(data = PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT, aes(fill = value, geometry = geometry)) +
  geom_sf(data = PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT, color = "black", fill = NA, lwd = 0.35, aes(geometry = geometry)) +
  scale_fill_viridis_c(limits = c(0,25), breaks = c(0,5,10,15,20,25), labels = c("0","5","10","15","20","25+")) +
  coord_sf(xlim = c(1600000, 1810000), expand = FALSE) +
  ggtitle(paste0("                   Auckland Housing Permits Per 1000 Residents: ",PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT$date[1])) +
  geom_text(data = subset(PERMITS_PER_NZ_REGIONS_EDIT, name == "Auckland Region"),
            aes(label = paste0("Total: ", value)),
            x = 1665000, y = 5980000, color ="white",size = 9, fontface = "bold", lineheight = unit(0.85,"cm"), hjust = 0) +
  geom_point(aes(x = 1758000, y = 5920000), color = "#EE6055", size = 3) +
  annotate(geom = "segment", x = 1758000, xend = 1780000, y = 5920000, yend = 5940000, color = "#EE6055", lwd = 1.25) +
  annotate(geom = "text", label = "Central\nBusiness\nDistrict", x = 1780000, y = 5946000, color ="#EE6055",size = 3.5, fontface = "bold", lineheight = unit(0.85,"cm")) +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data") +
  geom_text(data = if(PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT$date[1] == 2005) 
    data.frame(x = 1690000, y = 5990000, label = "2005 Downzoning") 
    else 
      data.frame(x = numeric(0), y = numeric(0), label = character(0)), 
    aes(x = x, y = y, label = label), color = "#FFE98F", size = 7) +
  geom_text(data = if(PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT$date[1] == 2013) 
    data.frame(x = 1690000, y = 5990000, label = "2013 Partial Upzoning") 
    else 
      data.frame(x = numeric(0), y = numeric(0), label = character(0)), 
    aes(x = x, y = y, label = label), color = "#FFE98F", size = 7) +
  geom_text(data = if(PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT$date[1] == 2016) 
    data.frame(x = 1690000, y = 5990000, label = "2016 Full Upzoning") 
    else 
      data.frame(x = numeric(0), y = numeric(0), label = character(0)), 
    aes(x = x, y = y, label = label), color = "#FFE98F", size = 7) +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right",axis.title.y = element_blank() ,axis.title.x = element_blank(), panel.grid.major=element_blank(),panel.grid.minor = element_blank(),  axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.margin= grid::unit(c(0.2, -0.4, 0, -3.5), "in"), legend.key = element_blank()) 

ggsave(dpi = "retina",plot = PERMITS_PER_AUCKLAND_BOARD_MAP, paste0("Permits Per Auckland Board Map ",PERMITS_PER_AUCKLAND_BOARD_DATA_EDIT$date[1],".png"), type = "cairo-png", width = 9.02, height = 5.76, units = "in")

}

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(readabs,openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  annotate(geom = "text", label = "2013\nSpHA\nPartial\nUpzone", x = as.Date("2012-04-01"), y = 14, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2012-07-01"), xend = as.Date("2012-07-01"), y = 0, yend = 15, color = "white", lwd = 0.75, linetype = "dashed") +
  annotate(geom = "text", label = "2016\nAUP\nFull\nUpzone", x = as.Date("2015-04-01"), y = 14, color ="white",size = 4, lineheight = unit(0.75, "cm"), hjust = 1) + 
  annotate(geom = "segment", x = as.Date("2015-07-01"), xend = as.Date("2015-07-01"), y = 0, yend = 15, color = "white", lwd = 0.75, linetype = "dashed") +
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

PERMITS_PER_NZ <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/New%20Zealand/NZ_PERMITS_PER_CAPITA.csv") %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  mutate(date = as.Date(paste0(date,"-01-01")))
  
PERMITS_PER_CAPITA_NZ_US_AU <- ggplot() + #plotting power generation
  geom_line(data=PERMITS_PER_US, aes(x=date,y= value,color= "United States"), size = 1.25) + 
  geom_line(data=PERMITS_PER_AU, aes(x=date,y= value,color= "Australia"), size = 1.25) + 
  geom_line(data=filter(PERMITS_PER_NZ, date>= as.Date("1984-01-01")), aes(x=date,y= New_Zealand,color= "New Zealand"), size = 1.25) + 
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

PERMITS_PER_CAPITA_US_NZ <- ggplot() + #plotting power generation
  geom_line(data=filter(PERMITS_PER_NZ, date >= as.Date("1996-01-01")), aes(x=date,y= Wellington_Region,color= "Wellington"), size = 1.25) + 
  geom_line(data=filter(PERMITS_PER_NZ, date >= as.Date("1996-01-01")), aes(x=date,y= Auckland_Region,color= "Auckland"), size = 1.25) + 
  geom_line(data=filter(PERMITS_PER_NZ, date >= as.Date("1996-01-01")), aes(x=date,y= Canterbury_Region,color= "Canterbury"), size = 1.25) + 
  xlab("Date") +
  ylab("Permits Per 1000 Residents") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,4,8,12), limits = c(0,14), expand = c(0,0)) +
  ggtitle("New Zealand's Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ and US Census data", subtitle = "New Zealand's Housing Permits Per-Capita Hit a 45-Year High in 2022—in Stark Contrast to the US") +
  theme_apricitas + theme(legend.position = c(.42,.81)) +
  scale_color_manual(name= "Housing Permits Per 1000 Residents",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Auckland","Canterbury","Wellington")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*(today()-as.Date("1967-01-01"))), xmax = as.Date("1996-01-01")-(0.049*(today()-as.Date("1996-01-01"))), ymin = 0-(.3*14), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PERMITS_PER_CAPITA_US_NZ, "Permits Per Capita US NZ.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

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
  scale_color_manual(name= "Housing Permits Per 1000 Residents",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("New Zealand Total","Auckland, New Zealand")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2003-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2003-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*350), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_EMPLOYMENT_Graph, "Construction_Activity_Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ggsave(dpi = "retina",plot = LA_PERMIT_GRAPH, "LA Permits Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



PERMITS_NYC_MSA <- fredr("NEWY636BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_LA_MSA <- fredr("LOSA106BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_CHI_MSA <- fredr("CHIC917BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_DAL_MSA <- fredr("DALL148BPPRIV", frequency = "a", aggregation_method = "avg")
PERMITS_HOU_MSA <- fredr("HOUS448BP1FH", frequency = "a", aggregation_method = "avg")


#LOOK UP COST DATA


#FLOOR AREA CONSENTED DATA


#GDP REGIONALLY AND GDP PER CAPITA DATA

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(openxlsx,tidyverse,janitor,bea.R,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

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
  
AUCKLAND_REGION_CONSENTS <- ggplot(AUCKLAND_REGION_CONSENTS, aes(x = date, y = value/1000, fill = name)) + #plotting power generation
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Dwelling Consents") +
  scale_y_continuous(labels = scales::number_format(suffix = "k", accuracy = 1), breaks = c(0,5,10,15,20,25), limits = c(0,25), expand = c(0,0)) +
  ggtitle("Auckland's Building Boom") +
  labs(caption = "Graph created by @JosephPolitano using Stats NZ data", subtitle = "Auckland's Dwelling Consents Hit a Record High In 2022, Driven by Rising Multifamily Construction") +
  theme_apricitas + theme(legend.position = c(.375,.81)) +
  scale_fill_manual(name= "Dwelling Consents by Year, Auckland Region",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1991-01-01")-(.1861*(today()-as.Date("1991-03-01"))), xmax = as.Date("1991-01-01")-(0.049*(today()-as.Date("1991-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AUCKLAND_REGION_CONSENTS, "Auckland Regiong Consents.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#LOOK UP COST DATA


#FLOOR AREA CONSENTED DATA


#GDP REGIONALLY AND GDP PER CAPITA DATA
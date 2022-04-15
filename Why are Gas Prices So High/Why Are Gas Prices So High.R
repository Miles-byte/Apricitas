pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)
pacman::p_load(eia)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

GASREGW <- fredr(series_id = "GASREGW") #downloading regular all formulations gas price
RETAIL_GAS <- fredr(series_id = "RSGASS") #downloading advanced retail sales of gas
RETAIL_ALL <- fredr(series_id = "RSXFS") #downloading all retail sales

VMT <- fredr(series_id = "VMT")

#downloading expenditure data for consumer units 
GAS_EXPEND_BOTTOM <- bls_api("CXUGASOILLB0102M", startyear = 1984, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
TOTAL_EXPEND_BOTTOM <- bls_api("CXUTOTALEXPLB0102M", startyear = 1984, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
GAS_EXPEND_MEDIAN<- bls_api("CXUGASOILLB0104M", startyear = 1984, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
TOTAL_EXPEND_MEDIAN <- bls_api("CXUTOTALEXPLB0104M", startyear = 1984, endyear = 2020, Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

XOP <- tq_get("XOP", from = "2006-07-01") #downloading XOP returns
SPY <- tq_get("SPY", from = "2006-07-01") #downloading SPY returns

WTI <- tq_get("CL=F", from = "2006-07-01")
Baker_Hughes

Petrol_Profits

Capital_Discipline <- data.frame(answer = c("Investor Pressure to Maintain Capital Discipline","Other","Environmental, Social, and Governance Issues","Lack of Access to Financing","Government Regulations"), percent = c(.59,.15,.11,.08,.06))

eia_series("PET.MCRFPUS1.M", start = "2006")

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

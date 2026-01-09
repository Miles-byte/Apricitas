pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


# AL <- fredr(series_id = "ALNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Alabama")
# AK <- fredr(series_id = "AKNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Alaska")
# AZ <- fredr(series_id = "AZNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Arizona")
# AR <- fredr(series_id = "ARNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Arkansas")
# CA <- fredr(series_id = "CANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "California")
# CO <- fredr(series_id = "CONA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Colorado")
# CT <- fredr(series_id = "CTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Connecticut")
# DE <- fredr(series_id = "DENA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Delaware")
# DC <- fredr(series_id = "DCNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "District of Columbia")
# FL <- fredr(series_id = "FLNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Florida")
# GA <- fredr(series_id = "GANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Georgia")
# HI <- fredr(series_id = "HINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Hawaii")
# ID <- fredr(series_id = "IDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Idaho")
# IL <- fredr(series_id = "ILNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Illinois")
# IN <- fredr(series_id = "INNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Indiana")
# IA <- fredr(series_id = "IANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Iowa")
# KS <- fredr(series_id = "KSNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Kansas")
# KY <- fredr(series_id = "KYNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Kentucky")
# LA <- fredr(series_id = "LANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Louisiana")
# ME <- fredr(series_id = "MENA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Maine")
# MD <- fredr(series_id = "MDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Maryland")
# MA <- fredr(series_id = "MANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Massachusetts")
# MI <- fredr(series_id = "MINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Michigan")
# MN <- fredr(series_id = "MNNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Minnesota")
# MS <- fredr(series_id = "MSNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Mississippi")
# MO <- fredr(series_id = "MONA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Missouri")
# MT <- fredr(series_id = "MTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Montana")
# NE <- fredr(series_id = "NENA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Nebraska")
# NV <- fredr(series_id = "NVNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Nevada")
# NH <- fredr(series_id = "NHNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New Hampshire")
# NJ <- fredr(series_id = "NJNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New Jersey")
# NM <- fredr(series_id = "NMNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New Mexico")
# NY <- fredr(series_id = "NYNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New York")
# NC <- fredr(series_id = "NCNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "North Carolina")
# ND <- fredr(series_id = "NDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "North Dakota")
# OH <- fredr(series_id = "OHNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Ohio")
# OK <- fredr(series_id = "OKNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Oklahoma")
# OR <- fredr(series_id = "ORNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Oregon")
# PA <- fredr(series_id = "PANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Pennsylvania")
# RI <- fredr(series_id = "RINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Rhode Island")
# SC <- fredr(series_id = "SCNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "South Carolina")
# SD <- fredr(series_id = "SDNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "South Dakota")
# TN <- fredr(series_id = "TNNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Tennessee")
# TX <- fredr(series_id = "TXNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Texas")
# UT <- fredr(series_id = "UTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Utah")
# VT <- fredr(series_id = "VTNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Vermont")
# VA <- fredr(series_id = "VANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Virginia")
# WA <- fredr(series_id = "WANA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Washington")
# WV <- fredr(series_id = "WVNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "West Virginia")
# WI <- fredr(series_id = "WINA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Wisconsin")
# WY <- fredr(series_id = "WYNA",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Wyoming")
# PR <- fredr(series_id = "SMS72000000000000001",observation_start = as.Date("2020-01-01")) %>%
#   mutate(series_id = "PR") %>%
#   mutate(name = "Puerto Rico")
# VI <- fredr(series_id = "SMS78000000000000001",observation_start = as.Date("2020-01-01")) %>%
#   mutate(series_id = "VI") %>%
#   mutate(name = "Virgin Islands")

AL <- bls_api("SMS01000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alabama")
AK <- bls_api("SMS02000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alaska")
AZ <- bls_api("SMS04000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arizona")
AR <- bls_api("SMS05000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arkansas")
CA <- bls_api("SMS06000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "California")
CO <- bls_api("SMS08000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Colorado")
CT <- bls_api("SMS09000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Connecticut")
DE <- bls_api("SMS10000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Delaware")
DC <- bls_api("SMS11000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "District of Columbia")
FL <- bls_api("SMS12000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Florida")
GA <- bls_api("SMS13000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Georgia")
HI <- bls_api("SMS15000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Hawaii")
ID <- bls_api("SMS16000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Idaho")
IL <- bls_api("SMS17000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Illinois")
IN <- bls_api("SMS18000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Indiana")
IA <- bls_api("SMS19000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Iowa")
KS <- bls_api("SMS20000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kansas")
KY <- bls_api("SMS21000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kentucky")
LA <- bls_api("SMS22000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Louisiana")
ME <- bls_api("SMS23000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maine")
MD <- bls_api("SMS24000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maryland")
MA <- bls_api("SMS25000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Massachusetts")
MI <- bls_api("SMS26000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Michigan")
MN <- bls_api("SMS27000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Minnesota")
MS <- bls_api("SMS28000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Mississippi")
MO <- bls_api("SMS29000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Missouri")
MT <- bls_api("SMS30000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Montana")
NE <- bls_api("SMS31000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nebraska")
NV <- bls_api("SMS32000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nevada")
NH <- bls_api("SMS33000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Hampshire")
NJ <- bls_api("SMS34000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Jersey")
NM <- bls_api("SMS35000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Mexico")
NY <- bls_api("SMS36000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New York")
NC <- bls_api("SMS37000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Carolina")
ND <- bls_api("SMS38000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Dakota")
OH <- bls_api("SMS39000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Ohio")
OK <- bls_api("SMS40000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oklahoma")
OR <- bls_api("SMS41000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oregon")
PA <- bls_api("SMS42000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Pennsylvania")
RI <- bls_api("SMS44000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Rhode Island")
SC <- bls_api("SMS45000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Carolina")
SD <- bls_api("SMS46000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Dakota")
TN <- bls_api("SMS47000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Tennessee")
TX <- bls_api("SMS48000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Texas")
UT <- bls_api("SMS49000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Utah")
VT <- bls_api("SMS50000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Vermont")
VA <- bls_api("SMS51000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virginia")
WA <- bls_api("SMS53000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Washington")
WV <- bls_api("SMS54000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "West Virginia")
WI <- bls_api("SMS55000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wisconsin")
WY <- bls_api("SMS56000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wyoming")


PR <- bls_api("SMS72000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Puerto Rico")
VI <- bls_api("SMS78000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virgin Islands")



JOB_GROWTH <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, PR, VI) %>%
  #select(date, value, name, series_id) %>%
  select(date, value, name, seriesID) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(CAGR = (value / first(value)) ^ (1 / ((row_number() - 1) / 12)) - 1) %>%
  mutate(Growth = ((value - first(value)) / first(value))) %>%
  mutate(Yoy_Growth = (value-lag(value,12))/lag(value,12)) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states_job_growth <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_job_growth <- states_job_growth %>%
  mutate(name = state_name)

states_job_growth <- left_join(states_job_growth, JOB_GROWTH, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_job_growth, .) %>%
  st_centroid()

states_territories_labls <- get_urbn_labels(map = "territories") %>%
  left_join(states_job_growth, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

JOB_GROWTH_STATE <- states_job_growth  %>%
  ggplot(aes(fill = Growth_bucket)) +
  geom_sf(color = NA) +
  geom_sf(data = states_job_growth, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AED581", "#00A99D","#3083DC"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<0%", "0-1.5%", "1.5-3%", "3-4.5%", "4.5-6%","6%+")) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle("    Change in Nonfarm Payrolls Since Jan 2020") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE, "Job Growth By State Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

states_territories_centroids <- states_territories_centroids %>%
  mutate(Growth_bucket = case_when(
    Growth < 0 ~ 0,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  ))

states_territories_labls <- states_territories_labls %>%
  mutate(Growth_bucket = case_when(
    Growth < 0 ~ 0,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  ))

JOB_GROWTH_STATE_GRADIENT_RAINBOW <- states_job_growth  %>%
  mutate(Growth_bucket = case_when(
    Growth < 0 ~ 0,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth_bucket)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),limits = c(0,.15), label = c("<0%","5%","10%","15%+"),breaks = c(0,0.05,0.1,0.15), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle("    Change in Nonfarm Payrolls Since Jan 2020") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE_GRADIENT_RAINBOW, "Job Growth By State Map Gradient Rainbow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

# AL_NSA <- fredr(series_id = "ALNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Alabama")
# AK_NSA <- fredr(series_id = "AKNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Alaska")
# AZ_NSA <- fredr(series_id = "AZNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Arizona")
# AR_NSA <- fredr(series_id = "ARNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Arkansas")
# CA_NSA <- fredr(series_id = "CANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "California")
# CO_NSA <- fredr(series_id = "CONAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Colorado")
# CT_NSA <- fredr(series_id = "CTNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Connecticut")
# DE_NSA <- fredr(series_id = "DENAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Delaware")
# DC_NSA <- fredr(series_id = "DCNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "District of Columbia")
# FL_NSA <- fredr(series_id = "FLNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Florida")
# GA_NSA <- fredr(series_id = "GANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Georgia")
# HI_NSA <- fredr(series_id = "HINAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Hawaii")
# ID_NSA <- fredr(series_id = "IDNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Idaho")
# IL_NSA <- fredr(series_id = "ILNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Illinois")
# IN_NSA <- fredr(series_id = "INNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Indiana")
# IA_NSA <- fredr(series_id = "IANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Iowa")
# KS_NSA <- fredr(series_id = "KSNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Kansas")
# KY_NSA <- fredr(series_id = "KYNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Kentucky")
# LA_NSA <- fredr(series_id = "LANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Louisiana")
# ME_NSA <- fredr(series_id = "MENAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Maine")
# MD_NSA <- fredr(series_id = "MDNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Maryland")
# MA_NSA <- fredr(series_id = "MANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Massachusetts")
# MI_NSA <- fredr(series_id = "MINAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Michigan")
# MN_NSA <- fredr(series_id = "MNNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Minnesota")
# MS_NSA <- fredr(series_id = "MSNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Mississippi")
# MO_NSA <- fredr(series_id = "MONAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Missouri")
# MT_NSA <- fredr(series_id = "MTNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Montana")
# NE_NSA <- fredr(series_id = "NENAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Nebraska")
# NV_NSA <- fredr(series_id = "NVNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Nevada")
# NH_NSA <- fredr(series_id = "NHNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New Hampshire")
# NJ_NSA <- fredr(series_id = "NJNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New Jersey")
# NM_NSA <- fredr(series_id = "NMNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New Mexico")
# NY_NSA <- fredr(series_id = "NYNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "New York")
# NC_NSA <- fredr(series_id = "NCNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "North Carolina")
# ND_NSA <- fredr(series_id = "NDNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "North Dakota")
# OH_NSA <- fredr(series_id = "OHNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Ohio")
# OK_NSA <- fredr(series_id = "OKNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Oklahoma")
# OR_NSA <- fredr(series_id = "ORNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Oregon")
# PA_NSA <- fredr(series_id = "PANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Pennsylvania")
# RI_NSA <- fredr(series_id = "RINAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Rhode Island")
# SC_NSA <- fredr(series_id = "SCNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "South Carolina")
# SD_NSA <- fredr(series_id = "SDNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "South Dakota")
# TN_NSA <- fredr(series_id = "TNNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Tennessee")
# TX_NSA <- fredr(series_id = "TXNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Texas")
# UT_NSA <- fredr(series_id = "UTNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Utah")
# VT_NSA <- fredr(series_id = "VTNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Vermont")
# VA_NSA <- fredr(series_id = "VANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Virginia")
# WA_NSA <- fredr(series_id = "WANAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Washington")
# WV_NSA <- fredr(series_id = "WVNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "West Virginia")
# WI_NSA <- fredr(series_id = "WINAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Wisconsin")
# WY_NSA <- fredr(series_id = "WYNAN",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
#   mutate(name = "Wyoming")
# PR_NSA <- fredr(series_id = "SMU72000000000000001",observation_start = as.Date("2020-01-01")) %>%
#   mutate(series_id = "PR") %>%
#   mutate(name = "Puerto Rico")
# VI_NSA <- fredr(series_id = "SMU78000000000000001",observation_start = as.Date("2020-01-01")) %>%
#   mutate(series_id = "VI") %>%
#   mutate(name = "Virgin Islands")

AL_NSA <- bls_api("SMU01000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alabama")
AK_NSA <- bls_api("SMU02000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alaska")
AZ_NSA <- bls_api("SMU04000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arizona")
AR_NSA <- bls_api("SMU05000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arkansas")
CA_NSA <- bls_api("SMU06000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "California")
CO_NSA <- bls_api("SMU08000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Colorado")
CT_NSA <- bls_api("SMU09000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Connecticut")
DE_NSA <- bls_api("SMU10000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Delaware")
DC_NSA <- bls_api("SMU11000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "District of Columbia")
FL_NSA <- bls_api("SMU12000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Florida")
GA_NSA <- bls_api("SMU13000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Georgia")
HI_NSA <- bls_api("SMU15000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Hawaii")
ID_NSA <- bls_api("SMU16000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Idaho")
IL_NSA <- bls_api("SMU17000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Illinois")
IN_NSA <- bls_api("SMU18000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Indiana")
IA_NSA <- bls_api("SMU19000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Iowa")
KS_NSA <- bls_api("SMU20000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kansas")
KY_NSA <- bls_api("SMU21000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kentucky")
LA_NSA <- bls_api("SMU22000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Louisiana")
ME_NSA <- bls_api("SMU23000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maine")
MD_NSA <- bls_api("SMU24000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maryland")
MA_NSA <- bls_api("SMU25000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Massachusetts")
MI_NSA <- bls_api("SMU26000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Michigan")
MN_NSA <- bls_api("SMU27000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Minnesota")
MS_NSA <- bls_api("SMU28000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Mississippi")
MO_NSA <- bls_api("SMU29000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Missouri")
MT_NSA <- bls_api("SMU30000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Montana")
NE_NSA <- bls_api("SMU31000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nebraska")
NV_NSA <- bls_api("SMU32000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nevada")
NH_NSA <- bls_api("SMU33000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Hampshire")
NJ_NSA <- bls_api("SMU34000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Jersey")
NM_NSA <- bls_api("SMU35000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Mexico")
NY_NSA <- bls_api("SMU36000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New York")
NC_NSA <- bls_api("SMU37000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Carolina")
ND_NSA <- bls_api("SMU38000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Dakota")
OH_NSA <- bls_api("SMU39000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Ohio")
OK_NSA <- bls_api("SMU40000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oklahoma")
OR_NSA <- bls_api("SMU41000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oregon")
PA_NSA <- bls_api("SMU42000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Pennsylvania")
RI_NSA <- bls_api("SMU44000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Rhode Island")
SC_NSA <- bls_api("SMU45000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Carolina")
SD_NSA <- bls_api("SMU46000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Dakota")
TN_NSA <- bls_api("SMU47000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Tennessee")
TX_NSA <- bls_api("SMU48000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Texas")
UT_NSA <- bls_api("SMU49000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Utah")
VT_NSA <- bls_api("SMU50000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Vermont")
VA_NSA <- bls_api("SMU51000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virginia")
WA_NSA <- bls_api("SMU53000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Washington")
WV_NSA <- bls_api("SMU54000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "West Virginia")
WI_NSA <- bls_api("SMU55000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wisconsin")
WY_NSA <- bls_api("SMU56000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wyoming")


PR_NSA <- bls_api("SMU72000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Puerto Rico")
VI_NSA <- bls_api("SMU78000000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virgin Islands")



JOB_GROWTH_NSA <- rbind(AL_NSA, AK_NSA, AZ_NSA, AR_NSA, CA_NSA, CO_NSA, CT_NSA, DE_NSA, DC_NSA, FL_NSA, GA_NSA, HI_NSA, ID_NSA, IL_NSA, IN_NSA, IA_NSA, KS_NSA, KY_NSA, LA_NSA, ME_NSA, MD_NSA, MA_NSA, MI_NSA, MN_NSA, MS_NSA, MO_NSA, MT_NSA, NE_NSA, NV_NSA, NH_NSA, NJ_NSA, NM_NSA, NY_NSA, NC_NSA, ND_NSA, OH_NSA, OK_NSA, OR_NSA, PA_NSA, RI_NSA, SC_NSA, SD_NSA, TN_NSA, TX_NSA, UT_NSA, VT_NSA, VA_NSA, WA_NSA, WV_NSA, WI_NSA, WY_NSA, PR_NSA, VI_NSA) %>%
  select(date, value, name, seriesID) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(CAGR = (value / first(value)) ^ (1 / ((row_number() - 1) / 12)) - 1) %>%
  mutate(Growth = ((value - first(value)) / first(value))) %>%
  mutate(Yoy_Growth = (value-lag(value,12))/lag(value,12)) %>%
  filter(date == max(date))

states_job_growth_NSA <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_job_growth_NSA <- states_job_growth_NSA %>%
  mutate(name = state_name)

states_job_growth_NSA <- left_join(states_job_growth_NSA, JOB_GROWTH_NSA, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids_NSA <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_job_growth_NSA, .) %>%
  st_centroid()

states_territories_labls_NSA <- get_urbn_labels(map = "territories") %>%
  left_join(states_job_growth_NSA, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

states_job_growth_NSA <- states_job_growth_NSA %>%
  mutate(Yoy_Growth = case_when(
    Yoy_Growth < -0.02 ~ -0.02,
    Yoy_Growth > 0.02 ~ 0.02,
    TRUE ~ Yoy_Growth))

states_territories_centroids_NSA <- states_territories_centroids_NSA %>%
  mutate(Yoy_Growth_labels = Yoy_Growth) %>%
  mutate(Yoy_Growth = case_when(
    Yoy_Growth < -0.02 ~ -0.02,
    Yoy_Growth > 0.02 ~ 0.02,
    TRUE ~ Yoy_Growth))

JOB_GROWTH_STATE_YOY <- states_job_growth_NSA  %>%
  ggplot(aes(fill = Yoy_Growth)) +
  geom_sf(color = NA) +
  geom_sf(data = states_job_growth_NSA, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1)) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("PR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("VI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls_NSA, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(paste("Change in Nonfarm Payrolls,",format(ymd(states_job_growth_NSA$date[1]) %m-% months(12), "%b %Y"), "to", format(ymd(states_job_growth_NSA$date[1]), "%b %Y"))) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 26))

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE_YOY, "Job Growth By State Yoy Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

JOB_GROWTH_STATE_YOY_RAINBOW <- states_job_growth_NSA  %>%
  ggplot(aes(fill = Yoy_Growth)) +
  geom_sf(color = NA) +
  geom_sf(data = states_job_growth_NSA, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  #scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1),breaks = c(-.05,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06), expand = c(0,0)) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),breaks = c(-0.02,-0.01,0,0.01,0.02),label = c("-2%+","-1%","0%","1%","2%+"), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("PR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("VI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth_labels >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth_labels * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls_NSA, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(paste("Change in Nonfarm Payrolls,",format(ymd(states_job_growth_NSA$date[1]) %m-% months(12), "%b %Y"), "to", format(ymd(states_job_growth_NSA$date[1]), "%b %Y"))) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 26))

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE_YOY_RAINBOW, "Job Growth By State Yoy Map Rainbow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



PAYEMS_2020 <- fredr(series_id = "PAYEMS",observation_start = as.Date("2020-01-01"),realtime_start = NULL, realtime_end = NULL)%>%
  mutate(name = "Rest of America")

TX_FL_US_PAYEMS <- rbind(FL,TX,PAYEMS_2020) %>%
  select(date, value, name) %>%
  group_by(name) %>%
  mutate(value = (value - first(value))) %>%
  pivot_wider() %>%
  mutate(`Rest of America` = `Rest of America`-Texas-Florida) %>%
  drop_na() %>%
  pivot_longer(cols = Florida:`Rest of America`) %>%
  mutate(name = factor(name, levels = rev(c("Texas", "Florida", "Rest of America"))))

TX_FL_US_PAYEMS_graph <- ggplot(data = TX_FL_US_PAYEMS, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Millions of Jobs") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, suffix = "M"), breaks = c(-20,-15,-10,-5,0,5), limits = c(-22,5), expand = c(0,0)) +
  ggtitle("The State of US Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Texas and Florida Have Created the Most New Jobs Since the Pandemic") +
  theme_apricitas + theme(legend.position = c(.625,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Nonfarm Payrolls, Change Since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Texas","Florida","Rest of America")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -22-(.3*27), ymax = -22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TX_FL_US_PAYEMS_graph, "TX FL Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

#Adding Puerto Rico

JOB_GROWTH <- JOB_GROWTH %>%
  mutate(series_id = gsub("NA","",seriesID)) %>%
  .[order(.$Growth), ] %>%
  mutate(seriesID = factor(seriesID, levels = .$seriesID))

JOB_GROWTH_STATE_BAR_GRAPH <- ggplot(data = JOB_GROWTH, aes(x = series_id, y = Growth, fill = "Nonfarm Payrolls Growth Since Jan 2020")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +#plotting Deposits, Insured and Uninsured
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("State") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.05,0,0.05,0.10), limits = c(-.12,.11), expand = c(0,0)) +
  ggtitle("Job Growth By State/Territory") +
  labs(caption = "Graph created by @JosephPolitano using US Census data", subtitle = "Most States Have Now Seen Payrolls Fully Recover From the Pandemic") +
  theme_apricitas + theme(legend.position = c(.67,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  theme(axis.text.x = element_text(size = 7))

ggsave(dpi = "retina",plot = JOB_GROWTH_STATE_BAR_GRAPH, "Growth State Bar Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


AL_MANU <- bls_api("SMS01000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alabama")
AK_MANU <- bls_api("SMS02000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alaska")
AZ_MANU <- bls_api("SMS04000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arizona")
AR_MANU <- bls_api("SMS05000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arkansas")
CA_MANU <- bls_api("SMS06000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "California")
CO_MANU <- bls_api("SMS08000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Colorado")
CT_MANU <- bls_api("SMS09000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Connecticut")
DE_MANU <- bls_api("SMS10000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Delaware")
DC_MANU <- bls_api("SMS11000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "District of Columbia")
FL_MANU <- bls_api("SMS12000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Florida")
GA_MANU <- bls_api("SMS13000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Georgia")
HI_MANU <- bls_api("SMS15000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Hawaii")
ID_MANU <- bls_api("SMS16000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Idaho")
IL_MANU <- bls_api("SMS17000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Illinois")
IN_MANU <- bls_api("SMS18000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Indiana")
IA_MANU <- bls_api("SMS19000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Iowa")
KS_MANU <- bls_api("SMS20000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kansas")
KY_MANU <- bls_api("SMS21000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kentucky")
LA_MANU <- bls_api("SMS22000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Louisiana")
ME_MANU <- bls_api("SMS23000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maine")
MD_MANU <- bls_api("SMS24000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maryland")
MA_MANU <- bls_api("SMS25000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Massachusetts")
MI_MANU <- bls_api("SMS26000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Michigan")
MN_MANU <- bls_api("SMS27000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Minnesota")
MS_MANU <- bls_api("SMS28000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Mississippi")
MO_MANU <- bls_api("SMS29000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Missouri")
MT_MANU <- bls_api("SMS30000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Montana")
NE_MANU <- bls_api("SMS31000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nebraska")
NV_MANU <- bls_api("SMS32000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nevada")
NH_MANU <- bls_api("SMS33000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Hampshire")
NJ_MANU <- bls_api("SMS34000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Jersey")
NM_MANU <- bls_api("SMS35000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Mexico")
NY_MANU <- bls_api("SMS36000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New York")
NC_MANU <- bls_api("SMS37000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Carolina")
ND_MANU <- bls_api("SMS38000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Dakota")
OH_MANU <- bls_api("SMS39000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Ohio")
OK_MANU <- bls_api("SMS40000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oklahoma")
OR_MANU <- bls_api("SMS41000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oregon")
PA_MANU <- bls_api("SMS42000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Pennsylvania")
RI_MANU <- bls_api("SMS44000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Rhode Island")
SC_MANU <- bls_api("SMS45000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Carolina")
SD_MANU <- bls_api("SMS46000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Dakota")
TN_MANU <- bls_api("SMS47000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Tennessee")
TX_MANU <- bls_api("SMS48000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Texas")
UT_MANU <- bls_api("SMS49000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Utah")
VT_MANU <- bls_api("SMS50000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Vermont")
VA_MANU <- bls_api("SMS51000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virginia")
WA_MANU <- bls_api("SMS53000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Washington")
WV_MANU <- bls_api("SMS54000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "West Virginia")
WI_MANU <- bls_api("SMS55000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wisconsin")
WY_MANU <- bls_api("SMS56000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wyoming")


PR_MANU <- bls_api("SMS72000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Puerto Rico")


MANU_JOB_GROWTH <- rbind(AL_MANU, AK_MANU, AZ_MANU, AR_MANU, CA_MANU, CO_MANU, CT_MANU, DE_MANU, DC_MANU, FL_MANU, GA_MANU, HI_MANU, ID_MANU, IL_MANU, IN_MANU, IA_MANU, KS_MANU, KY_MANU, LA_MANU, ME_MANU, MD_MANU, MA_MANU, MI_MANU, MN_MANU, MS_MANU, MO_MANU, MT_MANU, NE_MANU, NV_MANU, NH_MANU, NJ_MANU, NM_MANU, NY_MANU, NC_MANU, ND_MANU, OH_MANU, OK_MANU, OR_MANU, PA_MANU, RI_MANU, SC_MANU, SD_MANU, TN_MANU, TX_MANU, UT_MANU, VT_MANU, VA_MANU, WA_MANU, WV_MANU, WI_MANU, WY_MANU, PR_MANU) %>%
  #select(date, value, name, series_id) %>%
  select(date, value, name, seriesID) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(CAGR = (value / first(value)) ^ (1 / ((row_number() - 1) / 12)) - 1) %>%
  mutate(Growth = ((value - first(value)) / first(value))) %>%
  mutate(Yoy_Growth = (value-lag(value,12))/lag(value,12)) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states_job_growth <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_job_growth <- states_job_growth %>%
  mutate(name = state_name)

states_job_growth <- left_join(states_job_growth, MANU_JOB_GROWTH, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66 & state_fips != 72) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_job_growth, .) %>%
  st_centroid()

states_territories_labls <- get_urbn_labels(map = "territories") %>%
  left_join(states_job_growth, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)


states_territories_centroids <- states_territories_centroids %>%
  mutate(Growth_bucket = case_when(
    Growth < -0.1 ~ -0.1,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  ))

states_territories_labls <- states_territories_labls %>%
  mutate(Growth_bucket = case_when(
    Growth < -0.1 ~ -0.1,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  ))






MANU_JOB_GROWTH_STATE_GRADIENT_RAINBOW <- states_job_growth  %>%
  mutate(Growth_bucket = case_when(
    Growth < -0.1 ~ -0.1,
    Growth > 0.15 ~ 0.15,
    TRUE ~ Growth
  )) %>%
  ggplot(aes(fill = Growth_bucket)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),limits = c(-.1,.15), label = c("-10%+","-5%","0%","5%","10%","15%+"),breaks = c(-.1,-0.05,0,0.05,0.1,0.15), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", "  "), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", "  "), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN","ME","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","IN","TN","ME","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(" Change in Manufacturing Jobs Since Jan 2020") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = MANU_JOB_GROWTH_STATE_GRADIENT_RAINBOW, "Manu Job Growth By State Map Gradient Rainbow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing






















AL_MANU_NSA <- bls_api("SMU01000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alabama")
AK_MANU_NSA <- bls_api("SMU02000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alaska")
AZ_MANU_NSA <- bls_api("SMU04000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arizona")
AR_MANU_NSA <- bls_api("SMU05000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arkansas")
CA_MANU_NSA <- bls_api("SMU06000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "California")
CO_MANU_NSA <- bls_api("SMU08000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Colorado")
CT_MANU_NSA <- bls_api("SMU09000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Connecticut")
DE_MANU_NSA <- bls_api("SMU10000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Delaware")
DC_MANU_NSA <- bls_api("SMU11000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "District of Columbia")
FL_MANU_NSA <- bls_api("SMU12000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Florida")
GA_MANU_NSA <- bls_api("SMU13000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Georgia")
HI_MANU_NSA <- bls_api("SMU15000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Hawaii")
ID_MANU_NSA <- bls_api("SMU16000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Idaho")
IL_MANU_NSA <- bls_api("SMU17000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Illinois")
IN_MANU_NSA <- bls_api("SMU18000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Indiana")
IA_MANU_NSA <- bls_api("SMU19000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Iowa")
KS_MANU_NSA <- bls_api("SMU20000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kansas")
KY_MANU_NSA <- bls_api("SMU21000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kentucky")
LA_MANU_NSA <- bls_api("SMU22000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Louisiana")
ME_MANU_NSA <- bls_api("SMU23000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maine")
MD_MANU_NSA <- bls_api("SMU24000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maryland")
MA_MANU_NSA <- bls_api("SMU25000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Massachusetts")
MI_MANU_NSA <- bls_api("SMU26000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Michigan")
MN_MANU_NSA <- bls_api("SMU27000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Minnesota")
MS_MANU_NSA <- bls_api("SMU28000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Mississippi")
MO_MANU_NSA <- bls_api("SMU29000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Missouri")
MT_MANU_NSA <- bls_api("SMU30000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Montana")
NE_MANU_NSA <- bls_api("SMU31000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nebraska")
NV_MANU_NSA <- bls_api("SMU32000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nevada")
NH_MANU_NSA <- bls_api("SMU33000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Hampshire")
NJ_MANU_NSA <- bls_api("SMU34000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Jersey")
NM_MANU_NSA <- bls_api("SMU35000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Mexico")
NY_MANU_NSA <- bls_api("SMU36000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New York")
NC_MANU_NSA <- bls_api("SMU37000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Carolina")
ND_MANU_NSA <- bls_api("SMU38000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Dakota")
OH_MANU_NSA <- bls_api("SMU39000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Ohio")
OK_MANU_NSA <- bls_api("SMU40000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oklahoma")
OR_MANU_NSA <- bls_api("SMU41000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oregon")
PA_MANU_NSA <- bls_api("SMU42000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Pennsylvania")
RI_MANU_NSA <- bls_api("SMU44000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Rhode Island")
SC_MANU_NSA <- bls_api("SMU45000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Carolina")
SD_MANU_NSA <- bls_api("SMU46000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Dakota")
TN_MANU_NSA <- bls_api("SMU47000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Tennessee")
TX_MANU_NSA <- bls_api("SMU48000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Texas")
UT_MANU_NSA <- bls_api("SMU49000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Utah")
VT_MANU_NSA <- bls_api("SMU50000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Vermont")
VA_MANU_NSA <- bls_api("SMU51000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virginia")
WA_MANU_NSA <- bls_api("SMU53000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Washington")
WV_MANU_NSA <- bls_api("SMU54000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "West Virginia")
WI_MANU_NSA <- bls_api("SMU55000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wisconsin")
WY_MANU_NSA <- bls_api("SMU56000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wyoming")


PR_MANU_NSA <- bls_api("SMU72000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Puerto Rico")
VI_MANU_NSA <- bls_api("SMU78000003000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virgin Islands")



MANU_JOB_GROWTH_NSA <- rbind(AL_MANU_NSA, AK_MANU_NSA, AZ_MANU_NSA, AR_MANU_NSA, CA_MANU_NSA, CO_MANU_NSA, CT_MANU_NSA, DE_MANU_NSA, DC_MANU_NSA, FL_MANU_NSA, GA_MANU_NSA, HI_MANU_NSA, ID_MANU_NSA, IL_MANU_NSA, IN_MANU_NSA, IA_MANU_NSA, KS_MANU_NSA, KY_MANU_NSA, LA_MANU_NSA, ME_MANU_NSA, MD_MANU_NSA, MA_MANU_NSA, MI_MANU_NSA, MN_MANU_NSA, MS_MANU_NSA, MO_MANU_NSA, MT_MANU_NSA, NE_MANU_NSA, NV_MANU_NSA, NH_MANU_NSA, NJ_MANU_NSA, NM_MANU_NSA, NY_MANU_NSA, NC_MANU_NSA, ND_MANU_NSA, OH_MANU_NSA, OK_MANU_NSA, OR_MANU_NSA, PA_MANU_NSA, RI_MANU_NSA, SC_MANU_NSA, SD_MANU_NSA, TN_MANU_NSA, TX_MANU_NSA, UT_MANU_NSA, VT_MANU_NSA, VA_MANU_NSA, WA_MANU_NSA, WV_MANU_NSA, WI_MANU_NSA, WY_MANU_NSA, PR_MANU_NSA, VI_MANU_NSA) %>%
  select(date, value, name, seriesID) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(CAGR = (value / first(value)) ^ (1 / ((row_number() - 1) / 12)) - 1) %>%
  mutate(Growth = ((value - first(value)) / first(value))) %>%
  mutate(Yoy_Growth = (value-lag(value,12))/lag(value,12)) %>%
  mutate(Change = value - lag(value,12)) %>%
  filter(date == max(date))

states_job_growth_NSA <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_job_growth_NSA <- states_job_growth_NSA %>%
  mutate(name = state_name)

states_job_growth_NSA <- left_join(states_job_growth_NSA, MANU_JOB_GROWTH_NSA, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids_NSA <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66) %>% #ex guam, northern mariana islansa, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_job_growth_NSA, .) %>%
  st_centroid()

states_territories_labls_NSA <- get_urbn_labels(map = "territories") %>%
  left_join(states_job_growth_NSA, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)



MANU_JOB_GROWTH_STATE_YOY_RAINBOW <- states_job_growth_NSA  %>%
  ggplot(aes(fill = Yoy_Growth)) +
  geom_sf(color = NA) +
  geom_sf(data = states_job_growth_NSA, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  #scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),limits = c(-0.06,0.06),label = scales::percent_format(accuracy = 1),breaks = c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06), expand = c(0,0)) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("PR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_NSA, state_abbv %in% c("VI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls_NSA, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_NSA, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Yoy_Growth >= 0, " ", ""), sprintf("%.1f", round(Yoy_Growth * 100, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(paste("Change in Manufacturing Jobs,",format(ymd(states_job_growth_NSA$date[1]) %m-% months(12), "%b %Y"), "to", format(ymd(states_job_growth_NSA$date[1]), "%b %Y"))) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = MANU_JOB_GROWTH_STATE_YOY_RAINBOW, "Manufacturing Job Growth By State Yoy Map Rainbow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


AL_FED <- bls_api("SMS01000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alabama")
AK_FED <- bls_api("SMS02000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alaska")
AZ_FED <- bls_api("SMS04000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arizona")
AR_FED <- bls_api("SMS05000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arkansas")
CA_FED <- bls_api("SMS06000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "California")
CO_FED <- bls_api("SMS08000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Colorado")
CT_FED <- bls_api("SMS09000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Connecticut")
DE_FED <- bls_api("SMS10000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Delaware")
DC_FED <- bls_api("SMS11000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "District of Columbia")
FL_FED <- bls_api("SMS12000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Florida")
GA_FED <- bls_api("SMS13000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Georgia")
HI_FED <- bls_api("SMS15000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Hawaii")
ID_FED <- bls_api("SMS16000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Idaho")
IL_FED <- bls_api("SMS17000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Illinois")
IN_FED <- bls_api("SMS18000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Indiana")
IA_FED <- bls_api("SMS19000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Iowa")
KS_FED <- bls_api("SMS20000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kansas")
KY_FED <- bls_api("SMS21000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kentucky")
LA_FED <- bls_api("SMS22000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Louisiana")
ME_FED <- bls_api("SMS23000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maine")
MD_FED <- bls_api("SMS24000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maryland")
MA_FED <- bls_api("SMS25000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Massachusetts")
MI_FED <- bls_api("SMS26000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Michigan")
MN_FED <- bls_api("SMS27000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Minnesota")
MS_FED <- bls_api("SMS28000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Mississippi")
MO_FED <- bls_api("SMS29000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Missouri")
MT_FED <- bls_api("SMS30000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Montana")
NE_FED <- bls_api("SMS31000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nebraska")
NV_FED <- bls_api("SMS32000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nevada")
NH_FED <- bls_api("SMS33000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Hampshire")
NJ_FED <- bls_api("SMS34000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Jersey")
NM_FED <- bls_api("SMS35000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Mexico")
NY_FED <- bls_api("SMS36000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New York")
NC_FED <- bls_api("SMS37000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Carolina")
ND_FED <- bls_api("SMS38000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Dakota")
OH_FED <- bls_api("SMS39000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Ohio")
OK_FED <- bls_api("SMS40000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oklahoma")
OR_FED <- bls_api("SMS41000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oregon")
PA_FED <- bls_api("SMS42000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Pennsylvania")
RI_FED <- bls_api("SMS44000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Rhode Island")
SC_FED <- bls_api("SMS45000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Carolina")
SD_FED <- bls_api("SMS46000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Dakota")
TN_FED <- bls_api("SMS47000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Tennessee")
TX_FED <- bls_api("SMS48000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Texas")
UT_FED <- bls_api("SMS49000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Utah")
VT_FED <- bls_api("SMS50000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Vermont")
VA_FED <- bls_api("SMS51000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virginia")
WA_FED <- bls_api("SMS53000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Washington")
WV_FED <- bls_api("SMS54000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "West Virginia")
WI_FED <- bls_api("SMS55000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wisconsin")
WY_FED <- bls_api("SMS56000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wyoming")
PR_FED <- bls_api("SMS72000009091000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Puerto Rico")

JOB_GROWTH <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, PR, VI) %>%
  #select(date, value, name, series_id) %>%
  select(date, value, name, seriesID) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(CAGR = (value / first(value)) ^ (1 / ((row_number() - 1) / 12)) - 1) %>%
  mutate(Growth = ((value - first(value)) / first(value))) %>%
  mutate(Yoy_Growth = (value-lag(value,12))/lag(value,12))

FED_GROWTH <- rbind(AL_FED, AK_FED, AZ_FED, AR_FED, CA_FED, CO_FED, CT_FED, DE_FED, DC_FED, FL_FED, GA_FED, HI_FED, ID_FED, IL_FED, IN_FED, IA_FED, KS_FED, KY_FED, LA_FED, ME_FED, MD_FED, MA_FED, MI_FED, MN_FED, MS_FED, MO_FED, MT_FED, NE_FED, NV_FED, NH_FED, NJ_FED, NM_FED, NY_FED, NC_FED, ND_FED, OH_FED, OK_FED, OR_FED, PA_FED, RI_FED, SC_FED, SD_FED, TN_FED, TX_FED, UT_FED, VT_FED, VA_FED, WA_FED, WV_FED, WI_FED, WY_FED, PR_FED) %>%
  #select(date, value, name, series_id) %>%
  transmute(date, fed_value = value, name) %>%
  filter(date >= as.Date("2025-01-01")) %>%
  left_join(.,JOB_GROWTH %>% select(date,value,name)) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(Change = fed_value - first(fed_value)) %>%
  mutate(Growth = Change/first(fed_value)) %>%
  mutate(Growth_Pct_Workforce = Change/first(value)) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

states_job_growth <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_job_growth <- states_job_growth %>%
  mutate(name = state_name)

states_job_growth <- left_join(states_job_growth, FED_GROWTH, by = "name") %>%
  drop_na() %>%
  mutate(Growth_bucket = cut(Growth, breaks = c(-Inf, 0, 0.015, 0.03, 0.045, 0.06, Inf), labels = c("<0", "0-0.015", "0.015-0.03", "0.03-0.045", "0.045-0.06","0.06+")))

states_territories_centroids <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66) %>% #ex guam, northern mariana islansa, and American Samoa
  filter(state_fips != 78) %>%
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_job_growth, .) %>%
  st_centroid()

states_territories_labls <- get_urbn_labels(map = "territories") %>%
  left_join(states_job_growth, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

FED_JOB_GROWTH_STATE_GRADIENT_RAINBOW <- states_job_growth  %>%
  ggplot(aes(fill = Growth)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F"),label = scales::percent_format(accuracy = 1), breaks = c(0,-0.02,-0.04,-0.06,-0.08,-.10,-.12,-.14), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", "  "), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", "  "), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", "  "), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", "  "), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","IN","TN","MS")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth >= 0, " ", ""), sprintf("%.1f", round(Growth * 100, 1)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(" Change in Federal Employment Since Jan 2025") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = FED_JOB_GROWTH_STATE_GRADIENT_RAINBOW, "Fed Job Growth By State Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FED_JOB_GROWTH_PCT_WORKFORCE_STATE_GRADIENT_RAINBOW <- states_job_growth  %>%
  ggplot(aes(fill = Growth_Pct_Workforce)) +
  geom_sf(color = NA) +
  geom_sf(color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F"),label = scales::percent_format(accuracy = 0.1),breaks = c(0,-.005,-0.01,-0.015,-0.02,-0.025,-0.03,-0.035), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
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
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), 
    size = 3.5, 
    color = "black",
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN","MS","ME","MN","WI","SC","AL","IL","MI","OH","NY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), size = 3, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("FL","TN","WI","SC","AL","IL","MI","OH","NY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), size = 2.5, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("MS","KY","ME","MN","IN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), size = 2.25, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), size = 2, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls, state_abbv %in% c("LA")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(Growth_Pct_Workforce >= 0, " ", ""), sprintf("%.2f", round(Growth_Pct_Workforce * 100, 2)), "%")), size = 1.75, color = "black", check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle("Change in Federal Employment Since Jan 2025\n   As a % of Total Public & Private Workforce") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = FED_JOB_GROWTH_PCT_WORKFORCE_STATE_GRADIENT_RAINBOW, "Fed Job Growth Pct Workforce By State Yoy Map.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

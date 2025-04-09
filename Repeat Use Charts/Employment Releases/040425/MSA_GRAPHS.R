pacman::p_load(tigris,sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

NYC <- bls_api("SMS36356200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "NYC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

LAX <- bls_api("SMS06310800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "LAX") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

CHI <- bls_api("SMS17169800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CHI") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

DAL <- bls_api("SMS48191000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DAL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

HOU <- bls_api("SMS48264200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "HOU") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

DMV <- bls_api("SMS11479000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DMV") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

PHI <- bls_api("SMS42379800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PHI") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

MIA <- bls_api("SMS12331000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MIA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


ATL <- bls_api("SMS13120600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "ATL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BOS <- bls_api("SMS25144600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BOS") %>%
  mutate(FIPS = "14460")

PHO <- bls_api("SMS04380600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PHO") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SFO <- bls_api("SMS06418600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SFO") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


RIV <- bls_api("SMS06401400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "RIV") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


DET <- bls_api("SMS26198200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DET") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SEA <- bls_api("SMS53426600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SEA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


MPL <- bls_api("SMS27334600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MPL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SDG <- bls_api("SMS06417400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SDG") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


TPA <- bls_api("SMS12453000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "TPA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


DEN <- bls_api("SMS08197400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DEN") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BAL <- bls_api("SMS24125800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BAL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


STL <- bls_api("SMS29411800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "STL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


ORL <- bls_api("SMS12367400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "ORL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


CHA <- bls_api("SMS37167400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CHA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


ANT <- bls_api("SMS48417000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "ANT") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


POR <- bls_api("SMS41389000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "POR") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


PIT <- bls_api("SMS42383000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PIT") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SAC <- bls_api("SMS06409000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SAC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


AUS <- bls_api("SMS48124200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "AUS") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


LVA <- bls_api("SMS32298200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "LVA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


CIN <- bls_api("SMS39171400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CIN") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


KAN <- bls_api("SMS29281400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "KAN") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


CLE <- bls_api("SMS39174100000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CLE") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


COL <- bls_api("SMS39181400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "COL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


IND <- bls_api("SMS18269000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "IND") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


#SAN JUAN PUERTO RICO
SJP <- bls_api("SMS72419800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SJP") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


NAS <- bls_api("SMS47349800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "NAS") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


JOS <- bls_api("SMS06419400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "HOU") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


VIR <- bls_api("SMS51472600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "VIR") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


PRO <- bls_api("SMS44393000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PRO") %>%
  mutate(FIPS = "39300")


JAC <- bls_api("SMS12272600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "JAC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


MIL <- bls_api("SMS55333400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MIL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


OKC <- bls_api("SMS40364200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "OKC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


RAL <- bls_api("SMS37395800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "RAL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


LOU <- bls_api("SMS21311400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "LOU") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


MEM <- bls_api("SMS47328200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MEM") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


RIC <- bls_api("SMS51400600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "RIC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SLC <- bls_api("SMS49416200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SLC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BIR <- bls_api("SMS01138200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BIR") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BUF <- bls_api("SMS36153800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BUF") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


FRE <- bls_api("SMS06234200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "FRE") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

ALL_MSAs <- rbind(NYC, LAX, CHI, DAL, HOU, DMV, PHI, MIA, ATL, BOS, PHO, SFO, RIV, DET, SEA, MPL, SDG, TPA, DEN, BAL, STL, ORL, CHA, ANT, POR, PIT, SAC, AUS, LVA, CIN, KAN, CLE, COL, IND, SJP, NAS, JOS, VIR, PRO, JAC, MIL, OKC, RAL, LOU, MEM, RIC, SLC, BIR, BUF, FRE) %>%
  filter(date == max(date)) %>%
  select(-period,-periodName,-latest,-seriesID,-footnotes)

MSA_map <- core_based_statistical_areas(cb = TRUE, year = 2023) %>%
  mutate(FIPS = GEOID)

states <- states(cb = TRUE, year = 2023) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS)) %>%
  shift_geometry(position = "outside")

MSA_map_US <- merge(MSA_map, ALL_MSAs, by = "FIPS") %>%
  mutate(Growth_bucket = cut(pct_growth_2020 , breaks = c(-Inf, 0, 0.025, 0.05, 0.075,0.10, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  shift_geometry(position = "outside")

BLS_NFP_MSA_BINS_2020 <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = Growth_bucket), color = "black", lwd = 0.5) +
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),
                    na.value = "grey50", 
                    #guide = "legend", 
                    labels = c("<0%", "0-2.5%", "2.5-5%", "5-7.5%", "7.5-10%","10+%"),
                    guide = guide_legend(override.aes = list(color = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")))) +
  ggtitle("   Change in Nonfarm Payrolls Since Jan 2020\n      50 Largest Metro Areas by Population") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_BINS_2020, "BLS NFP MSA BINS 2020.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BLS_NFP_MSA_GRAD_2020 <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = pct_growth_2020), color = "black", lwd = 0.5) +
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(.0,.05,.10,.15), expand = c(0,0)) +
  ggtitle("  Change in Nonfarm Payrolls Since Jan 2020\n      50 Largest Metro Areas by Population") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_GRAD_2020, "BLS NFP MSA GRAD 2020.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BLS_NFP_MSA_GRAD_2020_RAINBOW <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = pct_growth_2020), color = "black", lwd = 0.5) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1), breaks = c(-0.05,0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ggtitle("  Change in Nonfarm Payrolls Since Jan 2020\n      50 Largest Metro Areas by Population") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_GRAD_2020_RAINBOW, "BLS NFP MSA GRAD 2020 RAINBOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


MSA_map_US_centroids <- MSA_map_US %>%
  st_centroid()

BLS_NFP_MSA_BUB_2020_GRAPH <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(fill = "grey75") +
  geom_point(data = MSA_map_US_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_2020 > 0, size = raw_growth_2020/1000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = MSA_map_US_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = raw_growth_2020/1000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of Change",
                  max_size = 15,
                  breaks = c(0,0.15,.3,.45),
                  labels = c("0","150k","300k","450k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("       Change in Nonfarm Payrolls Since Jan 2020\n            50 Largest Metro Areas by Population") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_BUB_2020_GRAPH, "BLS NFP MSA BUB 2020 Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


NYC_NSA <- bls_api("SMU36356200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "NYC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

LAX_NSA <- bls_api("SMU06310800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "LAX") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

CHI_NSA <- bls_api("SMU17169800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CHI") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

DAL_NSA <- bls_api("SMU48191000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DAL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

HOU_NSA <- bls_api("SMU48264200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "HOU") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

DMV_NSA <- bls_api("SMU11479000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DMV") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

PHI_NSA <- bls_api("SMU42379800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PHI") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

MIA_NSA <- bls_api("SMU12331000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MIA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


ATL_NSA <- bls_api("SMU13120600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "ATL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BOS_NSA <- bls_api("SMU25144600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BOS") %>%
  mutate(FIPS = "14460")

PHO_NSA <- bls_api("SMU04380600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PHO") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SFO_NSA <- bls_api("SMU06418600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SFO") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


RIV_NSA <- bls_api("SMU06401400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "RIV") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


DET_NSA <- bls_api("SMU26198200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DET") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SEA_NSA <- bls_api("SMU53426600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SEA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


MPL_NSA <- bls_api("SMU27334600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MPL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SDG_NSA <- bls_api("SMU06417400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SDG") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


TPA_NSA <- bls_api("SMU12453000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "TPA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


DEN_NSA <- bls_api("SMU08197400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "DEN") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BAL_NSA <- bls_api("SMU24125800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BAL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


STL_NSA <- bls_api("SMU29411800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "STL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


ORL_NSA <- bls_api("SMU12367400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "ORL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


CHA_NSA <- bls_api("SMU37167400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CHA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


ANT_NSA <- bls_api("SMU48417000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "ANT") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


POR_NSA <- bls_api("SMU41389000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "POR") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


PIT_NSA <- bls_api("SMU42383000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PIT") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SAC_NSA <- bls_api("SMU06409000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SAC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


AUS_NSA <- bls_api("SMU48124200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "AUS") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


LVA_NSA <- bls_api("SMU32298200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "LVA") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


CIN_NSA <- bls_api("SMU39171400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CIN") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


KAN_NSA <- bls_api("SMU29281400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "KAN") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


CLE_NSA <- bls_api("SMU39174100000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "CLE") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


COL_NSA <- bls_api("SMU39181400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "COL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


IND_NSA <- bls_api("SMU18269000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "IND") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


#SAN JUAN PUERTO RICO
SJP_NSA <- bls_api("SMU72419800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SJP") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


NAS_NSA <- bls_api("SMU47349800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "NAS") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


JOS_NSA <- bls_api("SMU06419400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "HOU") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


VIR_NSA <- bls_api("SMU51472600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "VIR") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


PRO_NSA <- bls_api("SMU44393000000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "PRO") %>%
  mutate(FIPS = "39300")


JAC_NSA <- bls_api("SMU12272600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "JAC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


MIL_NSA <- bls_api("SMU55333400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MIL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


OKC_NSA <- bls_api("SMU40364200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "OKC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


RAL_NSA <- bls_api("SMU37395800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "RAL") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


LOU_NSA <- bls_api("SMU21311400000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "LOU") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


MEM_NSA <- bls_api("SMU47328200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "MEM") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


RIC_NSA <- bls_api("SMU51400600000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "RIC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


SLC_NSA <- bls_api("SMU49416200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "SLC") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BIR_NSA <- bls_api("SMU01138200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BIR") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


BUF_NSA <- bls_api("SMU36153800000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "BUF") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))


FRE_NSA <- bls_api("SMU06234200000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(pct_growth_2020 = (value-value[1])/value[1],
         raw_growth_2020 = (value-value[1]),
         pct_growth_yoy = (value-lag(value,12))/lag(value,12),
         raw_growth_yoy = (value-lag(value,12))) %>%
  mutate(name = "FRE") %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10)))

ALL_MSAs_NSA <- rbind(NYC_NSA, LAX_NSA, CHI_NSA, DAL_NSA, HOU_NSA, DMV_NSA, PHI_NSA, MIA_NSA, ATL_NSA, BOS_NSA, PHO_NSA, SFO_NSA, RIV_NSA, DET_NSA, SEA_NSA, MPL_NSA, SDG_NSA, TPA_NSA, DEN_NSA, BAL_NSA, STL_NSA, ORL_NSA, CHA_NSA, ANT_NSA, POR_NSA, PIT_NSA, SAC_NSA, AUS_NSA, LVA_NSA, CIN_NSA, KAN_NSA, CLE_NSA, COL_NSA, IND_NSA, SJP_NSA, NAS_NSA, JOS_NSA, VIR_NSA, PRO_NSA, JAC_NSA, MIL_NSA, OKC_NSA, RAL_NSA, LOU_NSA, MEM_NSA, RIC_NSA, SLC_NSA, BIR_NSA, BUF_NSA, FRE_NSA) %>%
  filter(date == max(date)) %>%
  select(-period,-periodName,-latest,-seriesID,-footnotes) #%>%
  # mutate(pct_growth_yoy = round(pct_growth_yoy*100,2))

MSA_map_US_NSA <- merge(MSA_map, ALL_MSAs_NSA, by = "FIPS") %>%
  mutate(Growth_bucket = cut(pct_growth_2020 , breaks = c(-Inf, 0, 0.025, 0.05, 0.075,0.10, Inf), labels = c("<0", "0-0.04", "0.04-0.08", "0.08-0.12", "0.12-0.16","0.16+"))) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  shift_geometry(position = "outside")

MSA_map_US_NSA_centroids <- MSA_map_US_NSA %>%
  st_centroid()

BLS_NFP_MSA_BUB_YOY_GRAPH <- MSA_map_US_NSA %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(fill = "grey75") +
  geom_point(data = MSA_map_US_NSA_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = raw_growth_yoy > 0, size = raw_growth_yoy/1000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = MSA_map_US_NSA_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = raw_growth_yoy/1000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  scale_fill_manual(name = NULL,
                    values = c("#3083DC","#EE6055"),
                    breaks = c(TRUE, FALSE), 
                    labels = c("Increase", "Decrease"),
                    guide = guide_legend(override.aes = list(color = c("#3083DC","#EE6055"), size = 5))) +
  scale_size_area(name = "Size of Change",
                  max_size = 15,
                  breaks = c(0,0.025,.050,.075),
                  labels = c("0","25k","50k","75k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("       Change in Nonfarm Payrolls, Year-on-Year\n            50 Largest Metro Areas by Population") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank()) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), # Set color legend order and size
         area = guide_legend(order = 2))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_BUB_YOY_GRAPH, "BLS NFP MSA BUB YOY Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BLS_NFP_MSA_GRAD_YOY <- MSA_map_US_NSA %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = pct_growth_yoy), color = "black", lwd = 0.5) +
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1), breaks = c(-0.04,-0.03,-0.02,-0.01,.0,.01,.02,.03,0.04), expand = c(0,0)) +
  ggtitle("  Change in Nonfarm Payrolls, Year-on-Year\n      50 Largest Metro Areas by Population") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_GRAD_YOY, "BLS NFP GRAD YOY.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BLS_NFP_MSA_GRAD_YOY_RAINBOW <- MSA_map_US_NSA %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(aes(fill = pct_growth_yoy), color = "black", lwd = 0.5) +
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = 1), breaks = c(-0.04,-0.03,-0.02,-0.01,.0,.01,.02,.03,0.04), expand = c(0,0)) +
  ggtitle("  Change in Nonfarm Payrolls, Year-on-Year\n      50 Largest Metro Areas by Population") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0.1, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = BLS_NFP_MSA_GRAD_YOY_RAINBOW, "BLS NFP GRAD YOY RAINBOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


  
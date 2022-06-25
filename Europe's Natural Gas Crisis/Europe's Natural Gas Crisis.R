pacman::p_load(magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

#Creating a theme for charts
theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), plot.title = element_text(size = 28, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"
apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

#Pipeline Imports

#Downloading Russian export data
#Downloading Nordstream import data 
NordStream_Pipeline_Import <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ru-tso-0002itp-00120exit,de-tso-0018itp-00297entry,de-tso-0016itp-00251entry,de-tso-0005itp-00491entry,de-tso-0001itp-00247entry,de-tso-0015itp-00250entry,de-tso-0001itp-00251entry,de-tso-0020itp-00454entry,de-tso-0017itp-00247entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1") %>%
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  pivot_wider(names_from = operatorLabel) %>%
  transmute(value = `OPAL Gastransport` + `Gasunie Deutschland `, operatorLabel = "Nordstream", periodFrom) %>%
  mutate(location = "Nordstream")

#Downloading Turkstream import data
TurkStream_Pipeline_Import <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=tr-tso-0004itp-00549exit,bg-tso-0001itp-00549entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "Turkstream")
#Downloading Finland import data
VARSKA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ru-tso-0001itp-00187exit,ee-tso-0001itp-00187entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(location = "Varska")

VARSKA_Pipeline_Imports <- VARSKA_Pipeline_Imports[!duplicated(VARSKA_Pipeline_Imports[c('periodFrom')]),] #for some reason the data repeats in 2020 several times, this just removes duplicates

#Downloading Lithuania import data
KOTLOVKA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=by-tso-0001itp-00085exit,lt-tso-0001itp-00085entry&from=2019-01-01&indicator=Allocation&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(location = "Kotlovka")
#Downloading Poland import data
YAMAL_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=by-tso-0001itp-00104exit,pl-tso-0001itp-00104entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "Yamal")
TIETEROWKA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=by-tso-0001itp-00094exit,pl-tso-0002itp-00094entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "Tieterowka")
WYSOKOJE_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=by-tso-0001itp-00092exit,pl-tso-0002itp-00092entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "Wysokoje")
  
#Note-Ukraine Gas Transit
#NOTE-NEED TO MERGE UKRTRANSGAS WITH GAZ-SYSTEM
DROZDOVICHI_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-10008exit,pl-tso-0002itp-10008entry,ua-tso-0001itp-00089exit&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "Drozdovichi")

DROZDOVICHI_Pipeline_Imports <- DROZDOVICHI_Pipeline_Imports[!duplicated(DROZDOVICHI_Pipeline_Imports[c('periodFrom')]),] #for some reason the data repeats in 2020 several times, this just removes duplicates


#Downloading Slovakia import data
#Note-NEED TO SELECT ONLY EUSTREAM
UZHGOROD_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-00434exit,ua-tso-0001itp-00432exit,ua-tso-0001itp-00117exit,ua-tso-0001itp-00431exit,ua-tso-0001itp-00433exit,sk-tso-0001itp-00117entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  subset(operatorLabel == "eustream") %>%
  mutate(location = "Uzhgorod")
#Downloading Hungary import data
#Note-NEED ONLY 1 operator needed
BEREGDAROK1_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-00095exit,hu-tso-0001itp-00095entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  subset(operatorLabel == "FGSZ")%>%
  mutate(location = "Bereg")

BEREGDAROK_Virtual_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-10006exit,ua-tso-0001itp-10006entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  subset(directionKey == "exit") %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "BeregVirtual")

BEREGDAROK_Virtual_Pipeline_Imports <- BEREGDAROK_Virtual_Pipeline_Imports[!duplicated(BEREGDAROK_Virtual_Pipeline_Imports[c('periodFrom')]),] #for some reason the data repeats in 2020 several times, this just removes duplicates


BEREGDAROK_Pipeline_Imports <- rbind(BEREGDAROK1_Pipeline_Imports,BEREGDAROK_Virtual_Pipeline_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom,value = Bereg + BeregVirtual) %>%
  mutate(location = "Beregdarok") %>%
  mutate(operatorLabel = "Beregdarok")



#Downloading Romania import data
MEDIESU_AURIT_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-00084exit,ro-tso-0001itp-00084entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(location = "Mediesu_Aurit")
#NOTE-only need Transgaz
ISACCEA1_AURIT_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-00087exit,ro-tso-0001itp-00087entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  subset(operatorLabel == "Transgaz") %>%
  mutate(location = "Isaccea1")

ISACCEA2_AURIT_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-00299exit,ro-tso-0001itp-00299entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  subset(operatorLabel == "Transgaz") %>%
  mutate(location = "Isaccea2")

ISACCEA3_AURIT_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ua-tso-0001itp-00300exit,ro-tso-0001itp-00300entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  subset(operatorLabel == "Transgaz") %>%
  mutate(location = "Isaccea3")

Russia_Pipeline_Imports <- rbind(ISACCEA3_AURIT_Pipeline_Imports,
                                  ISACCEA2_AURIT_Pipeline_Imports,
                                  ISACCEA1_AURIT_Pipeline_Imports,
                                  MEDIESU_AURIT_Pipeline_Imports,
                                  BEREGDAROK_Pipeline_Imports,
                                  UZHGOROD_Pipeline_Imports,
                                  DROZDOVICHI_Pipeline_Imports,
                                  WYSOKOJE_Pipeline_Imports,
                                  TIETEROWKA_Pipeline_Imports,
                                  YAMAL_Pipeline_Imports,
                                  KOTLOVKA_Pipeline_Imports,
                                  VARSKA_Pipeline_Imports,
                                  TurkStream_Pipeline_Import,
                                  NordStream_Pipeline_Import
                                  ) %>%
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(yw = paste(year(periodFrom), week(periodFrom))) %>%
  mutate_if(is.numeric, ~mean(.)) %>%
  mutate(Total = Isaccea3 + Isaccea2 + Isaccea1 + Mediesu_Aurit + Beregdarok + Uzhgorod + Drozdovichi + Wysokoje + Tieterowka + Yamal + Kotlovka + Varska + Turkstream + Nordstream)

#Downloading Algeria export data
#Downloading Algerian exports to Italy
MAZARA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=tn-tso-0001itp-00093exit,it-tso-0001itp-00093entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Algeria") %>%
  mutate(import_country = "Italy") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Mazara")
#Downloading Algerian exports to Spain
ALMERIA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0001itp-00048exit,es-tso-0006itp-00048entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Algeria") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Almeria") 

TARIFA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0008itp-00082exit,es-tso-0006itp-00082entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "Algeria") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "Pipeline")%>%
  mutate(location = "Tarifa")

#creating Algeria aggregate
Algeria_Pipeline_Imports <- rbind(MAZARA_Pipeline_Imports,ALMERIA_Pipeline_Imports,TARIFA_Pipeline_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom,value = Mazara + Almeria + Tarifa) %>%
  mutate(location = "Algeria")

#Downloading Libyan export data
GELA_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=ly-tso-0001itp-00074exit,it-tso-0001itp-00074entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Libya") %>%
  mutate(import_country = "Italy") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Gela")

#creating Libyan aggregate
Libya_Pipeline_Imports <- GELA_Pipeline_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, value = Gela)%>%
  mutate(location = "Libya")


North_Africa_Pipeline_Imports <- rbind(Algeria_Pipeline_Imports,Libya_Pipeline_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  group_by(yw = paste(year(periodFrom), week(periodFrom))) %>%
  mutate_if(is.numeric, ~mean(.)) %>%
  transmute(periodFrom, North_Africa = Libya + Algeria)


#Downloading Azerbaijani Import data
KIPOI_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=tr-tso-0002itp-00274exit,al-tso-0001itp-00274entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Azerbaijan") %>%
  mutate(import_country = "Greece") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Kipoi")

KIPI_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=tr-tso-0001itp-00046exit,gr-tso-0002itp-00046entry,gr-tso-0001itp-00046entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true") %>% 
  mutate(periodFrom = as.Date(periodFrom)) %>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Azerbaijan") %>%
  mutate(import_country = "Greece") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Kipi")

Azerbaijan_Pipeline_Imports <- rbind(KIPI_Pipeline_Imports,KIPOI_Pipeline_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  group_by(yw = paste(year(periodFrom), week(periodFrom))) %>%
  mutate_if(is.numeric, ~mean(.)) %>%
  transmute(periodFrom,Azerbaijan = Kipoi + Kipi)

#Downloading Norwegian export Data
#Downloading Norwegian exports to the UK
ST_FERGUS_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=no-tso-0001itp-00022exit,uk-tso-0001itp-00022entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "UK") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "St_Fergus")

ST_FERGUS_Pipeline_Imports <- ST_FERGUS_Pipeline_Imports[!duplicated(ST_FERGUS_Pipeline_Imports[c('periodFrom')]),] #for some reason the UK data repeats in 2020 several times, this just removes duplicates


#NOTE-EASINGTON DATA Has Small Gaps
EASINGTON_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=no-tso-0001itp-00091exit,uk-tso-0001itp-00091entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "UK") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Easington")
  
EASINGTON_Pipeline_Imports <- EASINGTON_Pipeline_Imports[!duplicated(EASINGTON_Pipeline_Imports[c('periodFrom')]),] #for some reason the UK data repeats in 2020 several times, this just removes duplicates

#Downloading Norwegian exports to France
DUNKERQUE_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=no-tso-0001itp-00045exit,fr-tso-0003itp-00045entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "France") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Dunkerque_FRA")


#Downloading Norwegian exports to Belgium
ZEEBRUGGE_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=no-tso-0001itp-00106exit,be-tso-0001itp-00106entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "Belgium") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Dunkerque_BEL")

#Downloading Norwegian exports to Germany
DORNUM_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=de-tso-0009itp-00525entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "Germany") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "Dornum")

#NOTE-NEEDS PIVOTWIDER BECAUSE Thyssengas and Open Grid Europe are Different
EMDEN_DEU_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=no-tso-0001itp-00209exit,de-tso-0009itp-00080entry,de-tso-0002itp-00105entry,de-tso-0005itp-00081entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  subset(operatorLabel != "Thyssengas") %>%
  pivot_wider(names_from = operatorLabel, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, value = `Gasunie Deutschland `+`Open Grid Europe`) %>%
  mutate(operatorLabel = "Gasunie + Open Grid") %>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "Germany") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "EMDEN_DEU")

#Downloading Norwegian exports to Netherlands
EMDEN_NLD_Pipeline_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=no-tso-0001itp-00209exit,nl-tso-0001itp-00160entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "Norway") %>%
  mutate(import_country = "Netherlands") %>%
  mutate(import_type = "Pipeline") %>%
  mutate(location = "EMDEN_NLD")

Norway_Pipeline_Imports <- rbind(ST_FERGUS_Pipeline_Imports,EASINGTON_Pipeline_Imports,DUNKERQUE_Pipeline_Imports,ZEEBRUGGE_Pipeline_Imports,DORNUM_Pipeline_Imports,EMDEN_DEU_Pipeline_Imports,EMDEN_NLD_Pipeline_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(yw = paste(year(periodFrom), week(periodFrom))) %>%
  mutate_if(is.numeric, ~mean(.)) %>%
  transmute(periodFrom, Norway = St_Fergus + Dunkerque_FRA + Dunkerque_BEL + Dornum + EMDEN_DEU + EMDEN_NLD)

#LNG Imports
#Spain LNG Imports
BARCELONA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0006lng-00012entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Barcelona")

SAGUNTO_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0003lng-00023exit,es-tso-0006lng-00023entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Sagunto")

CARTAGENA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0006lng-00022entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Cartagena")

HUELVA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0006lng-00018entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Huelva")

BILBAO_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=es-tso-0005lng-00013exit,es-tso-0006lng-00013entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Spain") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Bilbao")

Spain_LNG_Imports <- rbind(BARCELONA_LNG_Imports,SAGUNTO_LNG_Imports,CARTAGENA_LNG_Imports,HUELVA_LNG_Imports,BILBAO_LNG_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Spain", value = Barcelona + Sagunto + Cartagena + Huelva + Bilbao)


#Portugal LNG Imports
SINES_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=pt-tso-0002lng-00026exit,pt-tso-0001lng-00026entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom") %>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Portugal") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Sines")

Portugal_LNG_Imports <- SINES_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Portugal", value = Sines)

#French LNG Imports
FOS_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=fr-lso-0003lng-00025exit,fr-lso-0004lng-00020exit,fr-tso-0003lng-00029entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "France") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "FOS")

MONTOIRDEBRETAGNE_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=fr-lso-0003lng-00024exit,fr-tso-0003lng-00024entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "France") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Montoir De Bretagne")

DUNKERQUE_FRA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=fr-lso-0001lng-00047exit,fr-tso-0003lng-00003entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "France") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Dunkerque_FRA")

France_LNG_Imports <- rbind(FOS_LNG_Imports,MONTOIRDEBRETAGNE_LNG_Imports,DUNKERQUE_FRA_LNG_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "France",value = FOS + `Montoir De Bretagne` + Dunkerque_FRA)


#Belgian LNG
ZEEBRUGGE_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=be-lso-0001lng-00017exit,be-tso-0001lng-00017entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Belgian") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Zeebrugge")

Belgium_LNG_Imports <- ZEEBRUGGE_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Belgium",value = Zeebrugge)

#Netherlands LNG
GATE_TERMINAL_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=nl-lso-0001lng-00027exit,nl-tso-0001lng-00027entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Netherlands") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Gate_Terminal")

Netherlands_LNG_Imports <- GATE_TERMINAL_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Netherlands",value = Gate_Terminal)

#UK LNG
TEESSIDE_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=uk-lso-0006lng-00007exit,uk-lso-0005lng-00007exit,uk-tso-0001lng-00007entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "UK") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Teesside") %>%
  distinct(periodFrom, .keep_all = TRUE) #for some reason the UK data repeats in 2020 several times, this just removes duplicates

MILFORD_HAVEN_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=uk-lso-0004lng-00049exit,uk-lso-0002lng-00049exit,uk-tso-0001lng-00049entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "UK") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Milford_Haven") %>%
  distinct(periodFrom, .keep_all = TRUE) #for some reason the UK data repeats in 2020 several times, this just removes duplicates

ISLEOFGRAIN_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=uk-lso-0001lng-00008exit,uk-tso-0001lng-00008entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "UK") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Isle_of_Grain") %>%
  distinct(periodFrom, .keep_all = TRUE) #for some reason the UK data repeats in 2020 several times, this just removes duplicates

UK_LNG_Imports <- rbind(TEESSIDE_LNG_Imports,MILFORD_HAVEN_LNG_Imports,ISLEOFGRAIN_LNG_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "UK", value = Teesside + Milford_Haven + Isle_of_Grain)

#Poland LNG
SWINOUJSCIE_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=pl-lso-0002lng-00006exit,pl-tso-0002lng-00006entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Poland") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Swinoujscie")

Poland_LNG_Imports <- SWINOUJSCIE_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Poland",value = Swinoujscie)

#Lithuania LNG
KLAIPEDA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=lt-lso-0001lng-00030exit,lt-tso-0001lng-00030entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Lithuania") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Klaipeda")

Lithuania_LNG_Imports <- KLAIPEDA_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom,location = "Lithuania", value = Klaipeda)

#Greece LNG
AGIATRIADA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=gr-tso-0001lng-00014entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Greece") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Agia_Triada")

Greece_LNG_Imports <- AGIATRIADA_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Greece", value = Agia_Triada)

#Croatia LNG
HRVATSKA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=hr-lso-0001lng-00032exit,hr-tso-0001lng-00032entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Croatia") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Hrvatska")

Croatia_LNG_Imports <- HRVATSKA_LNG_Imports %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom,location = "Croatia", value = Hrvatska)

#Italy LNG
CAVARZERE_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=it-lso-0001lng-00015exit,it-tso-0001lng-00015entry,it-tso-0004lng-00015entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Italy") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Cavazere")

LIVORNO_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=it-lso-0004lng-00004exit,it-tso-0001lng-00004entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Italy") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Livorno")

PANIGAGLIA_LNG_Imports <- read.csv("https://transparency.entsog.eu/api/v1/operationalData.csv?forceDownload=true&pointDirection=it-lso-0002lng-00019exit,it-tso-0001lng-00019entry&from=2019-01-01&indicator=Physical%20Flow&periodType=day&timezone=CET&limit=-1&dataset=1&directDownload=true")%>% 
  mutate(periodFrom = as.Date(periodFrom))%>%
  select("value","operatorLabel","periodFrom")%>%
  mutate(export_country = "LNG") %>%
  mutate(import_country = "Italy") %>%
  mutate(import_type = "LNG") %>%
  mutate(location = "Panigaglia")

Italy_LNG_Imports <- rbind(CAVARZERE_LNG_Imports,LIVORNO_LNG_Imports,PANIGAGLIA_LNG_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  transmute(periodFrom, location = "Italy", value = Cavazere + Livorno + Panigaglia)

TOTAL_LNG_Imports <- rbind(Spain_LNG_Imports,Portugal_LNG_Imports,France_LNG_Imports,Belgium_LNG_Imports,Netherlands_LNG_Imports,UK_LNG_Imports,Poland_LNG_Imports,Lithuania_LNG_Imports,Greece_LNG_Imports,Croatia_LNG_Imports,Italy_LNG_Imports) %>% 
  drop_na() %>% 
  select(value,periodFrom,location) %>% 
  pivot_wider(names_from = location, values_from = value) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(yw = paste(year(periodFrom), week(periodFrom))) %>%
  mutate_if(is.numeric, ~mean(.)) %>%
  mutate(Total = Spain + Portugal + France + Belgium + Netherlands + UK + Poland + Lithuania + Greece + Croatia + Italy)

TTF_FUTURES <- tq_get("TTFQ22.NYM", from = "2019-01-01") #Dutch TTF Futures
TTF_FUTURES <- drop_na(TTF_FUTURES)

EU_NAT_GAS_FUTURES_Graph <- ggplot() + #plotting EU Nat Gas Prices
  geom_line(data=TTF_FUTURES, aes(x=date,y= close, color= "EU Natural Gas Prices (TTF)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(prefix = "???"), limits = c(0,210), breaks = c(0,50,100,150,200), expand = c(0,0)) +
  ylab("Euros per MWh") +
  ggtitle("The EU's Different Inflation Problem") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data",subtitle = "Energy Prices Are Spiking in the EU, Pulling Up Inflation") +
  theme_apricitas + theme(legend.position = c(.30,.80)) +
  scale_color_manual(name= "August 2022 Futures",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1300), xmax = as.Date("2019-01-01")-(0.049*1300), ymin = 0-(.3*200), ymax = 0) +
  coord_cartesian(clip = "off")

NordStream_Pipeline_Import_Graph <- ggplot() + #plotting norstream pipeline imports
  geom_line(data=subset(NordStream_Pipeline_Import,periodFrom > as.Date("2021-09-01")) , aes(x=periodFrom,y= value/1000000000, color = "Nord Stream Pipeline Imports"), size = 1.25)+ 
  xlab("Date") +
  ylab("Daily Import Volumes, TWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh", accuracy = 0.5), limits = c(0,2), breaks = c(0,.5,1,1.5,2), expand = c(0,0)) +
  ggtitle("Europe's Natural Gas Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Entsog data with assistance from Bruegel", subtitle = "Imports Through the Nord Stream Pipeline have Decreased Nearly 60%") +
  theme_apricitas + theme(legend.position = c(0.25,0.55)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-09-01")-(.1861*(300)), xmax = as.Date("2021-09-01")-(0.049*(300)), ymin = 0-(.3*2), ymax = 0) +
  coord_cartesian(clip = "off")

LNG_Import_Graph <- ggplot() + #plotting LNG pipeline imports
  geom_line(data= TOTAL_LNG_Imports , aes(x=periodFrom,y= Total/1000000000, color = "LNG Imports to Europe"), size = 1.25)+ 
  xlab("Date") +
  ylab("Daily Import Volumes, Weekly Average, TWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh", accuracy = 1), limits = c(0,6), breaks = c(0,1,2,3,4,5,6), expand = c(0,0)) +
  ggtitle("Europe's Natural Gas Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Entsog data with assistance from Bruegel", subtitle = "LNG Imports Have Increased Significantly As Europe Tries to Replace Russian Gas") +
  theme_apricitas + theme(legend.position = c(0.25,0.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(1200)), xmax = as.Date("2019-01-01")-(0.049*(1200)), ymin = 0-(.3*6), ymax = 0) +
  coord_cartesian(clip = "off")

LNG_Pipeline_Import_Graph <- ggplot() + #plotting LNG pipeline imports
  geom_line(data= TOTAL_LNG_Imports , aes(x=periodFrom,y= Total/1000000000, color = "LNG"), size = 1.25)+ 
  geom_line(data= Russia_Pipeline_Imports , aes(x=periodFrom,y= Total/1000000000, color = "Russia"), size = 1.25)+ 
  geom_line(data= North_Africa_Pipeline_Imports, aes(x=periodFrom,y= North_Africa/1000000000, color = "North Africa"), size = 1.25)+ 
  geom_line(data= Norway_Pipeline_Imports , aes(x=periodFrom,y= Norway/1000000000, color = "Norway"), size = 1.25)+ 
  geom_line(data= Azerbaijan_Pipeline_Imports, aes(x=periodFrom,y= Azerbaijan/1000000000, color = "Azerbaijan"), size = 1.25)+ 
  xlab("Date") +
  ylab("Daily Import Volumes, Weekly Average, TWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh", accuracy = 1), limits = c(0,7), breaks = c(0,1,2,3,4,5,6,7), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2019-01-01"),today() - 7)) +
  ggtitle("Europe's Natural Gas Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Entsog data with assistance from Bruegel", subtitle = "LNG Imports Have Increased Significantly As Europe Tries to Replace Russian Gas") +
  theme_apricitas + theme(legend.position = c(0.41,0.84), legend.text = element_text(size = 14, color = "white")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#EE6055","#9A348E"), breaks = c("Russia","LNG","Norway","North Africa","Azerbaijan")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(1200)), xmax = as.Date("2019-01-01")-(0.049*(1200)), ymin = 0-(.3*7), ymax = 0) +
  coord_cartesian(clip = "off")

Russia_Pipeline_Import_Graph <- ggplot() + #plotting Russian pipeline imports
  geom_line(data=subset(Russia_Pipeline_Imports) , aes(x=periodFrom,y= Nordstream/1000000000, color = "Nord Stream"), size = 1.25)+ 
  geom_line(data=subset(Russia_Pipeline_Imports) , aes(x=periodFrom,y= Turkstream/1000000000, color = "TurkStream"), size = 1.25)+ 
  geom_line(data=subset(Russia_Pipeline_Imports) , aes(x=periodFrom,y= Yamal/1000000000, color = "Yamal (Poland)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Daily Import Volumes, Weekly Average, TWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh", accuracy = 0.5), limits = c(0,2), breaks = c(0,.5,1,1.5,2), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2021-01-01"),today() - 7)) +
  ggtitle("Europe's Natural Gas Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Entsog data with assistance from Bruegel", subtitle = "Imports Through Key Russian Gas Pipelines Are Declining Significantly") +
  theme_apricitas + theme(legend.position = c(0.23,0.68)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2021-01-01")-(.1861*(600)), xmax = as.Date("2021-01-01")-(0.049*(600)), ymin = 0-(.3*2), ymax = 0) +
  coord_cartesian(clip = "off")

Ukraine_Pipeline_Import_Graph <- ggplot() + #plotting Ukraine Gas pipeline imports
  geom_line(data=subset(Russia_Pipeline_Imports) , aes(x=periodFrom,y= (Drozdovichi + Uzhgorod + Mediesu_Aurit + Beregdarok + Isaccea3 + Isaccea2 + Isaccea1)/1000000000, color = "Russian Gas Transported Through Ukraine"), size = 1.25)+ 
  xlab("Date") +
  ylab("Daily Import Volumes, Weekly Average, TWh") +
  scale_y_continuous(labels = scales::number_format(suffix = "TWh", accuracy = 0.5), limits = c(0,3.25), breaks = c(0,.5,1,1.5,2,2.5,3), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("2019-01-01"),today() - 7)) +
  ggtitle("Europe's Natural Gas Crisis") +
  labs(caption = "Graph created by @JosephPolitano using Entsog data with assistance from Bruegel", subtitle = "Russian Gas Flows Through Ukraine are Down 80%-And Declined Before the War") +
  theme_apricitas + theme(legend.position = c(0.63,0.68)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(1200)), xmax = as.Date("2019-01-01")-(0.049*(1200)), ymin = 0-(.3*3.25), ymax = 0) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = NordStream_Pipeline_Import_Graph, "Nord Stream Pipeline Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = LNG_Import_Graph, "LNG Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = LNG_Pipeline_Import_Graph, "LNG Pipeline Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Russia_Pipeline_Import_Graph, "Russia Pipeline Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = Ukraine_Pipeline_Import_Graph, "Ukraine Pipeline Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = EU_NAT_GAS_FUTURES_Graph, "EU Nat Gas Futures.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
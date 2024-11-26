#MINING
OILANDGAS_211_EMP <- bls_api("CES1021100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OILANDGAS_211")
MININGXOG_212_EMP <- bls_api("CES1021200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MININGXOG_212")
MININGSUP_213_EMP <- bls_api("CES1021300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MININGSUP_213")

#UTILITIES
UTILITIES_221_EMP <- bls_api("CES4422000001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "UTILITIES_221")

#MANUFACTURING

CONSBLDNG_236_EMP <- bls_api("CES2023600001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CONSBLDNG_236")

CONSHVYCV_237_EMP <- bls_api("CES2023700001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CONSHVYCV_237")

CONSSPECL_238_EMP <- bls_api("CES2023800001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CONSSPECL_238")

#MANUFACTURING

MANUFOODS_311_EMP <- bls_api("CES3231100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUFOODS_311")

MANUDKTBL_312_EMP <- bls_api("CES3232900001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUDKTBL_312")

MANUTXTIL_313_EMP <- bls_api("CES3231300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUTXTIL_313")

MANUTXPRD_314_EMP <- bls_api("CES3231400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUTXPRD_314")

MANUAPPRL_315_EMP <- bls_api("CES3231500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUAPPRL_315")

MANUWOODS_321_EMP <- bls_api("CES3132100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUWOODS_321")

MANUPAPER_322_EMP <- bls_api("CES3232200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPAPER_322")

MANUPRINT_323_EMP <- bls_api("CES3232300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPRINT_323")

MANUPETRO_324_EMP <- bls_api("CES3232400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPAPER_324")

MANUCHEMI_325_EMP <- bls_api("CES3232500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUCHEMI_325")

MANUPLAST_326_EMP <- bls_api("CES3232600001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPLAST_326")

MANUNOMET_327_EMP <- bls_api("CES3132700001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_327")

MANUPRIMT_331_EMP <- bls_api("CES3133100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_331")

MANUFBMET_332_EMP <- bls_api("CES3133200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_332")

MANUMACHN_333_EMP <- bls_api("CES3133300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_333")

MANUCOMPU_334_EMP <- bls_api("CES3133400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUCOMPU_334")

MANUELECT_335_EMP <- bls_api("CES3133500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUCOMPU_335")

MANUTRANS_336_EMP <- bls_api("CES3133600001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUTRANS_336")

MANUFURNI_337_EMP <- bls_api("CES3133700001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUFURNI_337")

MANUMISCE_339_EMP <- bls_api("CES3133900001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUFURNI_339")

#WHOLESALERS

WHOLEMCHD_423_EMP <- bls_api("CES4142300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WHOLEMCHD_423")

WHOLEMCHN_424_EMP <- bls_api("CES4142400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WHOLEMCHN_424")

WHOLEMCHT_425_EMP <- bls_api("CES4142500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WHOLEMCHT_425")

#RETAILERS

RETAILVCL_441_EMP <- bls_api("CES4244100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILVCL_441")

RETAILBLD_444_EMP <- bls_api("CES4244400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILBLD_444")

RETAILFOD_445_EMP <- bls_api("CES4244500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILFOD_445")

RETAILFUR_449_EMP <- bls_api("CES4244900001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILFUR_449")

RETAILGEN_455_EMP <- bls_api("CES4245500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILGEN_455")

RETAILHEL_456_EMP <- bls_api("CES4245600001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILGEN_456")

RETAILGAS_457_EMP <- bls_api("CES4245700001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILGEN_457")

RETAILCLO_458_EMP <- bls_api("CES4245800001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILCLO_458")

RETAILSPO_459_EMP <- bls_api("CES4245900001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILCLO_459")

#Transportation 

AIRPTRANS_481_EMP <- bls_api("CES4348100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "AIRPTRANS_481")

RAILTRANS_482_EMP <- bls_api("CES4348200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RAILTRANS_482")

WATRTRANS_483_EMP <- bls_api("CES4348300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WATRTRANS_483")

TRCKTRANS_484_EMP <- bls_api("CES4348400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TRCKTRANS_484")

TRANTRANS_485_EMP <- bls_api("CES4348500001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TRANTRANS_485")

PIPETRANS_486_EMP <- bls_api("CES4348600001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PIPETRANS_486")

SCENTRANS_487_EMP <- bls_api("CES4348700001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SCENTRANS_487")

SUPPTRANS_488_EMP <- bls_api("CES4348800001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SUPPTRANS_488")

#MAYBE EXCLUDE
# POSTTRANS_491_EMP <- bls_api("CES4245900001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
#   mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
#   arrange(date) %>%
#   transmute(date,value,seriesID,name = "RETAILCLO_459")

COURTRANS_492_EMP <- bls_api("CES4349200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "COURTRANS_492")

WARESTORE_493_EMP <- bls_api("CES4349300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WARESTORE_493")

#INFORMATION

MOVIEINFO_512_EMP <- bls_api("CES5051200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MOVIEINFO_512")

PUBLSINFO_513_EMP <- bls_api("CES5051300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PUBLSINFO_513")

BROADINFO_516_EMP <- bls_api("CES5051600001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "BROADINFO_516")

TELECINFO_517_EMP <- bls_api("CES5051700001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TELECINFO_517")

COMPUINFO_518_EMP <- bls_api("CES5051800001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "COMPUINFO_518")

WEBSEINFO_519_EMP <- bls_api("CES5051900001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WEBSEINFO_519")

#Finance

# WARESTORE_493_EMP <- bls_api("CES4349300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
#   mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
#   arrange(date) %>%
#   transmute(date,value,seriesID,name = "WARESTORE_493")

CREDITFIN_522_EMP <- bls_api("CES5552200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CREDITFIN_522")

SECUREFIN_523_EMP <- bls_api("CES5552300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SECUREFIN_523")

INSUREFIN_524_EMP <- bls_api("CES5552400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "INSUREFIN_524")

#REAL ESTATE

REALESTAT_531_EMP <- bls_api("CES5553100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "REALESTAT_531")

RENTALEAS_532_EMP <- bls_api("CES5553200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RENTALEAS_532")

LESSORNFN_533_EMP <- bls_api("CES5553300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "LESSORNFN_533")

#Professional and Business Services

LEGALPROF_5411_EMP <- bls_api("CES6054110001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "LEGALPROF_5411")

ACCNTPROF_5412_EMP <- bls_api("CES6054120001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ACCNTPROF_5412")

ARCHIPROF_5413_EMP <- bls_api("CES6054130001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ARCHIPROF_5413")

SPECLPROF_5414_EMP <- bls_api("CES6054140001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SPECLPROF_5414")

COMPTPROF_5415_EMP <- bls_api("CES6054150001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "COMPTPROF_5415")

MNGMTPROF_5416_EMP <- bls_api("CES6054160001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MNGMTPROF_5416")

SCIENPROF_5417_EMP <- bls_api("CES6054170001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SCIENPROF_5417")

ADVRTPROF_5418_EMP <- bls_api("CES6054180001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ADVRTPROF_5418")

OTHERPROF_5419_EMP <- bls_api("CES6054190001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERPROF_5419")

#Management

MANAGEMNT_551_EMP <- bls_api("CES6055000001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANAGEMNT_551")

#Administrative Support

OFFICADMN_5611_EMP <- bls_api("CES6056110001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5611")

OFFICADMN_5612_EMP <- bls_api("CES6056120001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5612")

OFFICADMN_5613_EMP <- bls_api("CES6056130001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5613")

OFFICADMN_5614_EMP <- bls_api("CES6056140001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5614")

OFFICADMN_5615_EMP <- bls_api("CES6056150001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5615")

OFFICADMN_5616_EMP <- bls_api("CES6056160001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5616")

OFFICADMN_5617_EMP <- bls_api("CES6056170001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5617")

OFFICADMN_5619_EMP <- bls_api("CES6056190001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5619")

#WasteManagement

WSTEMGMNT_562_EMP <- bls_api("CES6056200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_562")

#Education Services
PRIVATEDU_610_EMP <- bls_api("CES6561000001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PRIVATEDU_610")

ELEMNTEDU_6111_EMP <- bls_api("CES6561110001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ELEMNTEDU_6111")

JUNIOREDU_6113_EMP <- bls_api("CES6561130001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "JUNIOREDU_6113")

BUSNSSEDU_6114_EMP <- bls_api("CES6561140001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "BUSNSSEDU_6114")

TCHNCLEDU_6115_EMP <- bls_api("CES6561150001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TCHNCLEDU_6115")

OTHERSEDU_6116_EMP <- bls_api("CES6561160001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERSEDU_6116")

SUPPRTEDU_6117_EMP <- bls_api("CES6561170001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SUPPRTEDU_6117")

#Healthcare

PHYSCHEAL_6211_EMP <- bls_api("CES6562110001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PHYSCHEAL_6211")

DENTIHEAL_6212_EMP <- bls_api("CES6562120001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "DENTIHEAL_6212")

OTHERHEAL_6213_EMP <- bls_api("CES6562130001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERHEAL_6213")

OUTPAHEAL_6214_EMP <- bls_api("CES6562140001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OUTPAHEAL_6214")

MLABSHEAL_6215_EMP <- bls_api("CES6562150001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MLABSHEAL_6215")

HOMESHEAL_6216_EMP <- bls_api("CES6562160001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "HOMESHEAL_6216")

OTHERHEAL_6219_EMP <- bls_api("CES6562190001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERHEAL_6219")

GENRLHEAL_6221_EMP <- bls_api("CES6562210001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "GENRLHEAL_6221")

PSYCHHEAL_6222_EMP <- bls_api("CES6562220001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SUPPRTEDU_6117")

SPECLHEAL_6223_EMP <- bls_api("CES6562230001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SPECLHEAL_6223")

NURSEHEAL_623_EMP <- bls_api("CES6562300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "NURSEHEAL_623")

SOCALHEAL_624_EMP <- bls_api("CES6562400001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SOCALHEAL_624")

#Art, Entertainment, Recreation

PERFRMART_711_EMP <- bls_api("CES7071100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PERFRMART_711")

MUSEUMART_712_EMP <- bls_api("CES7071200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MUSEUMART_712")

AMUSEMART_713_EMP <- bls_api("CES7071300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "AMUSEMART_713")

#Accomodation and Food Services

ACCMODATE_721_EMP <- bls_api("CES7072100001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ACCMODATE_721")

SPECLFOOD_7223_EMP <- bls_api("CES7072230001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SPECLFOOD_7223")

DRINKFOOD_7224_EMP <- bls_api("CES7072240001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "DRINKFOOD_7224")

FULLSFOOD_722511_EMP <- bls_api("CES7072251101", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "FULLSFOOD_722511")

FULLSFOOD_722513_EMP <- bls_api("CES7072251301", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "FULLSFOOD_722513")

FULLSFOOD_722514_EMP <- bls_api("CES7072251401", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "FULLSFOOD_722514")

SNACKFOOD_722515_EMP <- bls_api("CES7072251501", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SNACKFOOD_722515")

#Other Services

REPAIRSRV_811_EMP <- bls_api("CES8081200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "REPAIRSRV_811")

PERSNLSRV_812_EMP <- bls_api("CES8081200001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PERSNLSRV_812")

ORGNSMSRV_813_EMP <- bls_api("CES8081300001", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ORGNSMSRV_813")

BULK_EMP <- rbind(OILANDGAS_211_EMP,
                  MININGXOG_212_EMP,
                  MININGSUP_213_EMP,
                  
                  UTILITIES_221_EMP,
                  
                  CONSBLDNG_236_EMP,
                  CONSHVYCV_237_EMP,
                  CONSSPECL_238_EMP,
                  
                  MANUFOODS_311_EMP,
                  MANUDKTBL_312_EMP,
                  MANUTXTIL_313_EMP,
                  MANUTXPRD_314_EMP,
                  MANUAPPRL_315_EMP,
                  MANUWOODS_321_EMP,
                  MANUPAPER_322_EMP,
                  MANUPRINT_323_EMP,
                  MANUPETRO_324_EMP,
                  MANUCHEMI_325_EMP,
                  MANUPLAST_326_EMP,
                  MANUNOMET_327_EMP,
                  MANUPRIMT_331_EMP,
                  MANUFBMET_332_EMP,
                  MANUMACHN_333_EMP,
                  MANUCOMPU_334_EMP,
                  MANUELECT_335_EMP,
                  MANUTRANS_336_EMP,
                  MANUFURNI_337_EMP,
                  MANUMISCE_339_EMP,
                  
                  WHOLEMCHD_423_EMP,
                  WHOLEMCHN_424_EMP,
                  WHOLEMCHT_425_EMP,
                  
                  RETAILVCL_441_EMP,
                  RETAILBLD_444_EMP,
                  RETAILFOD_445_EMP,
                  RETAILFUR_449_EMP,
                  RETAILGEN_455_EMP,
                  RETAILHEL_456_EMP,
                  RETAILGAS_457_EMP,
                  RETAILCLO_458_EMP,
                  RETAILSPO_459_EMP,
                  
                  AIRPTRANS_481_EMP,
                  RAILTRANS_482_EMP,
                  WATRTRANS_483_EMP,
                  TRCKTRANS_484_EMP,
                  TRANTRANS_485_EMP,
                  PIPETRANS_486_EMP,
                  SCENTRANS_487_EMP,
                  SUPPTRANS_488_EMP,
                  COURTRANS_492_EMP,
                  
                  WARESTORE_493_EMP,
                  MOVIEINFO_512_EMP,
                  PUBLSINFO_513_EMP,
                  BROADINFO_516_EMP,
                  TELECINFO_517_EMP,
                  COMPUINFO_518_EMP,
                  WEBSEINFO_519_EMP,
                  
                  CREDITFIN_522_EMP,
                  SECUREFIN_523_EMP,
                  INSUREFIN_524_EMP,
                  REALESTAT_531_EMP,
                  RENTALEAS_532_EMP,
                  LESSORNFN_533_EMP,
                  
                  LEGALPROF_5411_EMP,
                  ACCNTPROF_5412_EMP,
                  ARCHIPROF_5413_EMP,
                  SPECLPROF_5414_EMP,
                  COMPTPROF_5415_EMP,
                  MNGMTPROF_5416_EMP,
                  SCIENPROF_5417_EMP,
                  ADVRTPROF_5418_EMP,
                  OTHERPROF_5419_EMP,
                  
                  MANAGEMNT_551_EMP,
                  
                  OFFICADMN_5611_EMP,
                  OFFICADMN_5612_EMP,
                  OFFICADMN_5613_EMP,
                  OFFICADMN_5614_EMP,
                  OFFICADMN_5615_EMP,
                  OFFICADMN_5616_EMP,
                  OFFICADMN_5617_EMP,
                  OFFICADMN_5619_EMP,
                  
                  WSTEMGMNT_562_EMP,
                  
                  ELEMNTEDU_6111_EMP,
                  JUNIOREDU_6113_EMP,
                  BUSNSSEDU_6114_EMP,
                  TCHNCLEDU_6115_EMP,
                  OTHERSEDU_6116_EMP,
                  SUPPRTEDU_6117_EMP,
                  
                  PHYSCHEAL_6211_EMP,
                  DENTIHEAL_6212_EMP,
                  OTHERHEAL_6213_EMP,
                  OUTPAHEAL_6214_EMP,
                  MLABSHEAL_6215_EMP,
                  HOMESHEAL_6216_EMP,
                  OTHERHEAL_6219_EMP,
                  GENRLHEAL_6221_EMP,
                  PSYCHHEAL_6222_EMP,
                  SPECLHEAL_6223_EMP,
                  
                  NURSEHEAL_623_EMP,
                  SOCALHEAL_624_EMP,
                  
                  PERFRMART_711_EMP,
                  MUSEUMART_712_EMP,
                  AMUSEMART_713_EMP,
                  ACCMODATE_721_EMP,
                  SPECLFOOD_7223_EMP,
                  DRINKFOOD_7224_EMP,
                  FULLSFOOD_722511_EMP,
                  FULLSFOOD_722513_EMP,
                  FULLSFOOD_722514_EMP,
                  SNACKFOOD_722515_EMP,
                  
                  REPAIRSRV_811_EMP,
                  PERSNLSRV_812_EMP,
                  ORGNSMSRV_813_EMP
)

BULK_EMP <- BULK_EMP %>%
  group_by(date) %>%
  filter(n() == 111) #%>%
#summarize(value = sum(value, na.rm = TRUE))


OILANDGAS_211_WAG <- bls_api("CES1021100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OILANDGAS_211")
MININGXOG_212_WAG <- bls_api("CES1021200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MININGXOG_212")
MININGSUP_213_WAG <- bls_api("CES1021300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MININGSUP_213")

#UTILITIES
UTILITIES_221_WAG <- bls_api("CES4422000003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "UTILITIES_221")

#MANUFACTURING

CONSBLDNG_236_WAG <- bls_api("CES2023600003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CONSBLDNG_236")

CONSHVYCV_237_WAG <- bls_api("CES2023700003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CONSHVYCV_237")

CONSSPECL_238_WAG <- bls_api("CES2023800003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CONSSPECL_238")

#MANUFACTURING

MANUFOODS_311_WAG <- bls_api("CES3231100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUFOODS_311")

MANUDKTBL_312_WAG <- bls_api("CES3232900003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUDKTBL_312")

MANUTXTIL_313_WAG <- bls_api("CES3231300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUTXTIL_313")

MANUTXPRD_314_WAG <- bls_api("CES3231400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUTXPRD_314")

MANUAPPRL_315_WAG <- bls_api("CES3231500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUAPPRL_315")

MANUWOODS_321_WAG <- bls_api("CES3132100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUWOODS_321")

MANUPAPER_322_WAG <- bls_api("CES3232200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPAPER_322")

MANUPRINT_323_WAG <- bls_api("CES3232300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPRINT_323")

MANUPETRO_324_WAG <- bls_api("CES3232400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPAPER_324")

MANUCHEMI_325_WAG <- bls_api("CES3232500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUCHEMI_325")

MANUPLAST_326_WAG <- bls_api("CES3232600003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUPLAST_326")

MANUNOMET_327_WAG <- bls_api("CES3132700003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_327")

MANUPRIMT_331_WAG <- bls_api("CES3133100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_331")

MANUFBMET_332_WAG <- bls_api("CES3133200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_332")

MANUMACHN_333_WAG <- bls_api("CES3133300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUNOMET_333")

MANUCOMPU_334_WAG <- bls_api("CES3133400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUCOMPU_334")

MANUELECT_335_WAG <- bls_api("CES3133500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUCOMPU_335")

MANUTRANS_336_WAG <- bls_api("CES3133600003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUTRANS_336")

MANUFURNI_337_WAG <- bls_api("CES3133700003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUFURNI_337")

MANUMISCE_339_WAG <- bls_api("CES3133900003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANUFURNI_339")

#WHOLESALERS

WHOLEMCHD_423_WAG <- bls_api("CES4142300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WHOLEMCHD_423")

WHOLEMCHN_424_WAG <- bls_api("CES4142400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WHOLEMCHN_424")

WHOLEMCHT_425_WAG <- bls_api("CES4142500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WHOLEMCHT_425")

#RETAILERS

RETAILVCL_441_WAG <- bls_api("CES4244100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILVCL_441")

RETAILBLD_444_WAG <- bls_api("CES4244400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILBLD_444")

RETAILFOD_445_WAG <- bls_api("CES4244500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILFOD_445")

RETAILFUR_449_WAG <- bls_api("CES4244900003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILFUR_449")

RETAILGEN_455_WAG <- bls_api("CES4245500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILGEN_455")

RETAILHEL_456_WAG <- bls_api("CES4245600003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILGEN_456")

RETAILGAS_457_WAG <- bls_api("CES4245700003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILGEN_457")

RETAILCLO_458_WAG <- bls_api("CES4245800003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILCLO_458")

RETAILSPO_459_WAG <- bls_api("CES4245900003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RETAILCLO_459")

#Transportation 

AIRPTRANS_481_WAG <- bls_api("CES4348100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "AIRPTRANS_481")

RAILTRANS_482_WAG <- bls_api("CES4348200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RAILTRANS_482")

WATRTRANS_483_WAG <- bls_api("CES4348300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WATRTRANS_483")

TRCKTRANS_484_WAG <- bls_api("CES4348400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TRCKTRANS_484")

TRANTRANS_485_WAG <- bls_api("CES4348500003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TRANTRANS_485")

PIPETRANS_486_WAG <- bls_api("CES4348600003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PIPETRANS_486")

SCENTRANS_487_WAG <- bls_api("CES4348700003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SCENTRANS_487")

SUPPTRANS_488_WAG <- bls_api("CES4348800003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SUPPTRANS_488")

#MAYBE EXCLUDE
# POSTTRANS_491_WAG <- bls_api("CES4245900003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
#   mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
#   arrange(date) %>%
#   transmute(date,value,seriesID,name = "RETAILCLO_459")

COURTRANS_492_WAG <- bls_api("CES4349200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "COURTRANS_492")

WARESTORE_493_WAG <- bls_api("CES4349300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WARESTORE_493")

#INFORMATION

MOVIEINFO_512_WAG <- bls_api("CES5051200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MOVIEINFO_512")

PUBLSINFO_513_WAG <- bls_api("CES5051300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PUBLSINFO_513")

BROADINFO_516_WAG <- bls_api("CES5051600003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "BROADINFO_516")

TELECINFO_517_WAG <- bls_api("CES5051700003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TELECINFO_517")

COMPUINFO_518_WAG <- bls_api("CES5051800003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "COMPUINFO_518")

WEBSEINFO_519_WAG <- bls_api("CES5051900003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "WEBSEINFO_519")

#Finance

# WARESTORE_493_WAG <- bls_api("CES4349300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
#   mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
#   arrange(date) %>%
#   transmute(date,value,seriesID,name = "WARESTORE_493")

CREDITFIN_522_WAG <- bls_api("CES5552200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "CREDITFIN_522")

SECUREFIN_523_WAG <- bls_api("CES5552300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SECUREFIN_523")

INSUREFIN_524_WAG <- bls_api("CES5552400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "INSUREFIN_524")

#REAL ESTATE

REALESTAT_531_WAG <- bls_api("CES5553100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "REALESTAT_531")

RENTALEAS_532_WAG <- bls_api("CES5553200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "RENTALEAS_532")

LESSORNFN_533_WAG <- bls_api("CES5553300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "LESSORNFN_533")

#Professional and Business Services

LEGALPROF_5411_WAG <- bls_api("CES6054110003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "LEGALPROF_5411")

ACCNTPROF_5412_WAG <- bls_api("CES6054120003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ACCNTPROF_5412")

ARCHIPROF_5413_WAG <- bls_api("CES6054130003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ARCHIPROF_5413")

SPECLPROF_5414_WAG <- bls_api("CES6054140003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SPECLPROF_5414")

COMPTPROF_5415_WAG <- bls_api("CES6054150003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "COMPTPROF_5415")

MNGMTPROF_5416_WAG <- bls_api("CES6054160003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MNGMTPROF_5416")

SCIENPROF_5417_WAG <- bls_api("CES6054170003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SCIENPROF_5417")

ADVRTPROF_5418_WAG <- bls_api("CES6054180003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ADVRTPROF_5418")

OTHERPROF_5419_WAG <- bls_api("CES6054190003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERPROF_5419")

#Management

MANAGEMNT_551_WAG <- bls_api("CES6055000003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MANAGEMNT_551")

#Administrative Support

OFFICADMN_5611_WAG <- bls_api("CES6056110003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5611")

OFFICADMN_5612_WAG <- bls_api("CES6056120003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5612")

OFFICADMN_5613_WAG <- bls_api("CES6056130003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5613")

OFFICADMN_5614_WAG <- bls_api("CES6056140003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5614")

OFFICADMN_5615_WAG <- bls_api("CES6056150003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5615")

OFFICADMN_5616_WAG <- bls_api("CES6056160003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5616")

OFFICADMN_5617_WAG <- bls_api("CES6056170003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5617")

OFFICADMN_5619_WAG <- bls_api("CES6056190003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_5619")

#WasteManagement

WSTEMGMNT_562_WAG <- bls_api("CES6056200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OFFICADMN_562")

#Education Services

PRIVATEDU_610_WAG <- bls_api("CES6561000003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PRIVATEDU_610")

ELEMNTEDU_6111_WAG <- bls_api("CES6561110003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ELEMNTEDU_6111")

JUNIOREDU_6113_WAG <- bls_api("CES6561130003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "JUNIOREDU_6113")

BUSNSSEDU_6114_WAG <- bls_api("CES6561140003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "BUSNSSEDU_6114")

TCHNCLEDU_6115_WAG <- bls_api("CES6561150003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "TCHNCLEDU_6115")

OTHERSEDU_6116_WAG <- bls_api("CES6561160003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERSEDU_6116")

SUPPRTEDU_6117_WAG <- bls_api("CES6561170003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SUPPRTEDU_6117")

#Healthcare

PHYSCHEAL_6211_WAG <- bls_api("CES6562110003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PHYSCHEAL_6211")

DENTIHEAL_6212_WAG <- bls_api("CES6562120003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "DENTIHEAL_6212")

OTHERHEAL_6213_WAG <- bls_api("CES6562130003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERHEAL_6213")

OUTPAHEAL_6214_WAG <- bls_api("CES6562140003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OUTPAHEAL_6214")

MLABSHEAL_6215_WAG <- bls_api("CES6562150003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MLABSHEAL_6215")

HOMESHEAL_6216_WAG <- bls_api("CES6562160003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "HOMESHEAL_6216")

OTHERHEAL_6219_WAG <- bls_api("CES6562190003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "OTHERHEAL_6219")

GENRLHEAL_6221_WAG <- bls_api("CES6562210003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "GENRLHEAL_6221")

PSYCHHEAL_6222_WAG <- bls_api("CES6562220003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SUPPRTEDU_6117")

SPECLHEAL_6223_WAG <- bls_api("CES6562230003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SPECLHEAL_6223")

NURSEHEAL_623_WAG <- bls_api("CES6562300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "NURSEHEAL_623")

SOCALHEAL_624_WAG <- bls_api("CES6562400003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SOCALHEAL_624")

#Art, Entertainment, Recreation

PERFRMART_711_WAG <- bls_api("CES7071100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PERFRMART_711")

MUSEUMART_712_WAG <- bls_api("CES7071200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "MUSEUMART_712")

AMUSEMART_713_WAG <- bls_api("CES7071300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "AMUSEMART_713")

#Accomodation and Food Services

ACCMODATE_721_WAG <- bls_api("CES7072100003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ACCMODATE_721")

SPECLFOOD_7223_WAG <- bls_api("CES7072230003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SPECLFOOD_7223")

DRINKFOOD_7224_WAG <- bls_api("CES7072240003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "DRINKFOOD_7224")

FULLSFOOD_722511_WAG <- bls_api("CES7072251103", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "FULLSFOOD_722511")

FULLSFOOD_722513_WAG <- bls_api("CES7072251303", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "FULLSFOOD_722513")

FULLSFOOD_722514_WAG <- bls_api("CES7072251403", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "FULLSFOOD_722514")

SNACKFOOD_722515_WAG <- bls_api("CES7072251503", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "SNACKFOOD_722515")

#Other Services

REPAIRSRV_811_WAG <- bls_api("CES8081200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "REPAIRSRV_811")

PERSNLSRV_812_WAG <- bls_api("CES8081200003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "PERSNLSRV_812")

ORGNSMSRV_813_WAG <- bls_api("CES8081300003", startyear = 2005, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  transmute(date,value,seriesID,name = "ORGNSMSRV_813")

BULK_WAG <- rbind(OILANDGAS_211_WAG,
                  MININGXOG_212_WAG,
                  MININGSUP_213_WAG,
                  
                  UTILITIES_221_WAG,
                  
                  CONSBLDNG_236_WAG,
                  CONSHVYCV_237_WAG,
                  CONSSPECL_238_WAG,
                  
                  MANUFOODS_311_WAG,
                  #MANUDKTBL_312_WAG,
                  MANUTXTIL_313_WAG,
                  MANUTXPRD_314_WAG,
                  MANUAPPRL_315_WAG,
                  MANUWOODS_321_WAG,
                  MANUPAPER_322_WAG,
                  MANUPRINT_323_WAG,
                  #MANUPETRO_324_WAG,
                  MANUCHEMI_325_WAG,
                  MANUPLAST_326_WAG,
                  MANUNOMET_327_WAG,
                  MANUPRIMT_331_WAG,
                  MANUFBMET_332_WAG,
                  MANUMACHN_333_WAG,
                  MANUCOMPU_334_WAG,
                  MANUELECT_335_WAG,
                  MANUTRANS_336_WAG,
                  MANUFURNI_337_WAG,
                  MANUMISCE_339_WAG,
                  
                  WHOLEMCHD_423_WAG,
                  WHOLEMCHN_424_WAG,
                  WHOLEMCHT_425_WAG,
                  
                  RETAILVCL_441_WAG,
                  RETAILBLD_444_WAG,
                  RETAILFOD_445_WAG,
                  RETAILFUR_449_WAG,
                  RETAILGEN_455_WAG,
                  RETAILHEL_456_WAG,
                  RETAILGAS_457_WAG,
                  RETAILCLO_458_WAG,
                  RETAILSPO_459_WAG,
                  
                  AIRPTRANS_481_WAG,
                  #RAILTRANS_482_WAG,
                  #WATRTRANS_483_WAG,
                  TRCKTRANS_484_WAG,
                  TRANTRANS_485_WAG,
                  #PIPETRANS_486_WAG,
                  #SCENTRANS_487_WAG,
                  SUPPTRANS_488_WAG,
                  COURTRANS_492_WAG,
                  
                  WARESTORE_493_WAG,
                  MOVIEINFO_512_WAG,
                  PUBLSINFO_513_WAG,
                  BROADINFO_516_WAG,
                  TELECINFO_517_WAG,
                  COMPUINFO_518_WAG,
                  WEBSEINFO_519_WAG,
                  
                  CREDITFIN_522_WAG,
                  SECUREFIN_523_WAG,
                  INSUREFIN_524_WAG,
                  REALESTAT_531_WAG,
                  RENTALEAS_532_WAG,
                  #LESSORNFN_533_WAG,
                  
                  LEGALPROF_5411_WAG,
                  ACCNTPROF_5412_WAG,
                  ARCHIPROF_5413_WAG,
                  SPECLPROF_5414_WAG,
                  COMPTPROF_5415_WAG,
                  MNGMTPROF_5416_WAG,
                  SCIENPROF_5417_WAG,
                  ADVRTPROF_5418_WAG,
                  OTHERPROF_5419_WAG,
                  
                  MANAGEMNT_551_WAG,
                  
                  OFFICADMN_5611_WAG,
                  OFFICADMN_5612_WAG,
                  OFFICADMN_5613_WAG,
                  OFFICADMN_5614_WAG,
                  OFFICADMN_5615_WAG,
                  OFFICADMN_5616_WAG,
                  OFFICADMN_5617_WAG,
                  OFFICADMN_5619_WAG,
                  
                  WSTEMGMNT_562_WAG,
                  
                  #ELEMNTEDU_6111_WAG,
                  #JUNIOREDU_6113_WAG,
                  #BUSNSSEDU_6114_WAG,
                  #TCHNCLEDU_6115_WAG,
                  #OTHERSEDU_6116_WAG,
                  #SUPPRTEDU_6117_WAG,
                  
                  PHYSCHEAL_6211_WAG,
                  DENTIHEAL_6212_WAG,
                  OTHERHEAL_6213_WAG,
                  OUTPAHEAL_6214_WAG,
                  MLABSHEAL_6215_WAG,
                  HOMESHEAL_6216_WAG,
                  OTHERHEAL_6219_WAG,
                  GENRLHEAL_6221_WAG,
                  PSYCHHEAL_6222_WAG,
                  SPECLHEAL_6223_WAG,
                  
                  NURSEHEAL_623_WAG,
                  SOCALHEAL_624_WAG,
                  
                  PERFRMART_711_WAG,
                  MUSEUMART_712_WAG,
                  AMUSEMART_713_WAG,
                  ACCMODATE_721_WAG,
                  SPECLFOOD_7223_WAG,
                  DRINKFOOD_7224_WAG,
                  FULLSFOOD_722511_WAG,
                  FULLSFOOD_722513_WAG,
                  FULLSFOOD_722514_WAG,
                  SNACKFOOD_722515_WAG,
                  
                  REPAIRSRV_811_WAG,
                  PERSNLSRV_812_WAG,
                  ORGNSMSRV_813_WAG
)

BULK_WAG <- BULK_WAG %>%
  group_by(date) %>%
  filter(n() == 98)

BULK_WAG_EMP_MERGE <- BULK_WAG %>%
  inner_join(BULK_EMP, by = c("date","name")) %>%
  mutate(year = year(date)) %>%
  group_by(year,name) %>%
  summarize(
    WAG = mean(value.x, na.rm = TRUE),
    EMP = mean(value.y, na.rm = TRUE),
    name = name
  ) %>%
  unique() %>%
  ungroup()

BULK_WAG_EMP_BOT_25 <- BULK_WAG_EMP_MERGE %>%
  filter(year == 2019) %>%
  arrange(WAG) %>%
  mutate(cum_weight = cumsum(EMP)/ sum(EMP)) %>%
  filter(cum_weight <= 0.25)

BULK_WAG_EMP_TOP_25 <- BULK_WAG_EMP_MERGE %>%
  filter(year == 2019) %>%
  arrange(WAG) %>%
  mutate(cum_weight = cumsum(EMP)/ sum(EMP)) %>%
  filter(cum_weight >= 0.743)

EMP_AGG <- BULK_EMP %>%
  group_by(date) %>%
  summarise(value = sum(value))

EMP_BOT_25 <- BULK_EMP %>%
  filter(name %in% BULK_WAG_EMP_BOT_25$name) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  merge(., EMP_AGG, by = "date") %>%
  transmute(date,EMP = value.x, EMP_PCT = value.x/value.y)

EMP_TOP_25 <- BULK_EMP %>%
  filter(name %in% BULK_WAG_EMP_TOP_25$name) %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  merge(., EMP_AGG, by = "date") %>%
  transmute(date,EMP = value.x, EMP_PCT = value.x/value.y)

EMP_GROWTH_WAG_LEVEL_GRAPH <- ggplot() + 
  #geom_line(data=filter(EMP_BOT_25, date >= as.Date("2005-01-01")), aes(x=date,y= value,color= "Bottom 25"), size = 1.25) +
  geom_line(data=filter(EMP_BOT_25, date >= as.Date("2015-01-01")), aes(x=date,y= EMP/EMP[61]*100,color= "Low Wage Industries (Bottom 25%)"), size = 1.25) +
  geom_line(data=filter(EMP_TOP_25, date >= as.Date("2015-01-01")), aes(x=date,y= EMP/EMP[61]*100,color= "High Wage Industries (Top 25%)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,110), breaks = c(60,70,80,90,100,110), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Job Growth by Pre-COVID Wage Level") +
  labs(caption = "Graph created by @JosephPolitano using BLS data at 3/4 Digit NAICS Levels",subtitle = "Jobs Have Grown Most in America's Highest-Wage Industries, and Slower in Its Low-Wage Ones") +
  theme_apricitas + theme(legend.position = c(.25,.89)) +
  scale_color_manual(name= "Employment Level, Jan 2020 = 100",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("High Wage Industries (Top 25%)","Low Wage Industries (Bottom 25%)")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 65-(.3*45), ymax = 65) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_GROWTH_WAG_LEVEL_GRAPH, "Employment Growth by Wage Level.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

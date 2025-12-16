AL_JOLTS_HIRE <- bls_api("JTS000000010000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alabama")
AK_JOLTS_HIRE <- bls_api("JTS000000020000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Alaska")
AZ_JOLTS_HIRE <- bls_api("JTS000000040000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arizona")
AR_JOLTS_HIRE <- bls_api("JTS000000050000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Arkansas")
CA_JOLTS_HIRE <- bls_api("JTS000000060000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "California")
CO_JOLTS_HIRE <- bls_api("JTS000000080000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Colorado")
CT_JOLTS_HIRE <- bls_api("JTS000000090000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Connecticut")
DE_JOLTS_HIRE <- bls_api("JTS000000100000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Delaware")
DC_JOLTS_HIRE <- bls_api("JTS000000110000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "District of Columbia")
FL_JOLTS_HIRE <- bls_api("JTS000000120000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Florida")
GA_JOLTS_HIRE <- bls_api("JTS000000130000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Georgia")
HI_JOLTS_HIRE <- bls_api("JTS000000150000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Hawaii")
ID_JOLTS_HIRE <- bls_api("JTS000000160000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Idaho")
IL_JOLTS_HIRE <- bls_api("JTS000000170000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Illinois")
IN_JOLTS_HIRE <- bls_api("JTS000000180000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Indiana")
IA_JOLTS_HIRE <- bls_api("JTS000000190000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Iowa")
KS_JOLTS_HIRE <- bls_api("JTS000000200000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kansas")
KY_JOLTS_HIRE <- bls_api("JTS000000210000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Kentucky")
LA_JOLTS_HIRE <- bls_api("JTS000000220000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Louisiana")
ME_JOLTS_HIRE <- bls_api("JTS000000230000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maine")
MD_JOLTS_HIRE <- bls_api("JTS000000240000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Maryland")
MA_JOLTS_HIRE <- bls_api("JTS000000250000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Massachusetts")
MI_JOLTS_HIRE <- bls_api("JTS000000260000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Michigan")
MN_JOLTS_HIRE <- bls_api("JTS000000270000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Minnesota")
MS_JOLTS_HIRE <- bls_api("JTS000000280000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Mississippi")
MO_JOLTS_HIRE <- bls_api("JTS000000290000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Missouri")
MT_JOLTS_HIRE <- bls_api("JTS000000300000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Montana")
NE_JOLTS_HIRE <- bls_api("JTS000000310000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nebraska")
NV_JOLTS_HIRE <- bls_api("JTS000000320000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Nevada")
NH_JOLTS_HIRE <- bls_api("JTS000000330000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Hampshire")
NJ_JOLTS_HIRE <- bls_api("JTS000000340000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Jersey")
NM_JOLTS_HIRE <- bls_api("JTS000000350000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New Mexico")
NY_JOLTS_HIRE <- bls_api("JTS000000360000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "New York")
NC_JOLTS_HIRE <- bls_api("JTS000000370000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Carolina")
ND_JOLTS_HIRE <- bls_api("JTS000000380000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "North Dakota")
OH_JOLTS_HIRE <- bls_api("JTS000000390000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Ohio")
OK_JOLTS_HIRE <- bls_api("JTS000000400000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oklahoma")
OR_JOLTS_HIRE <- bls_api("JTS000000410000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Oregon")
PA_JOLTS_HIRE <- bls_api("JTS000000420000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Pennsylvania")
RI_JOLTS_HIRE <- bls_api("JTS000000440000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Rhode Island")
SC_JOLTS_HIRE <- bls_api("JTS000000450000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Carolina")
SD_JOLTS_HIRE <- bls_api("JTS000000460000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "South Dakota")
TN_JOLTS_HIRE <- bls_api("JTS000000470000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Tennessee")
TX_JOLTS_HIRE <- bls_api("JTS000000480000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Texas")
UT_JOLTS_HIRE <- bls_api("JTS000000490000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Utah")
VT_JOLTS_HIRE <- bls_api("JTS000000500000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Vermont")
VA_JOLTS_HIRE <- bls_api("JTS000000510000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Virginia")
WA_JOLTS_HIRE <- bls_api("JTS000000530000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Washington")
WV_JOLTS_HIRE <- bls_api("JTS000000540000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "West Virginia")
WI_JOLTS_HIRE <- bls_api("JTS000000550000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wisconsin")
WY_JOLTS_HIRE <- bls_api("JTS000000560000000HIR", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  arrange(date) %>%
  mutate(FIPS = as.numeric(substr(seriesID, 6, nchar(seriesID) - 10))) %>%
  mutate(name = "Wyoming")





JOLTS_HIRE <- rbind(AL_JOLTS_HIRE, AK_JOLTS_HIRE, AZ_JOLTS_HIRE, AR_JOLTS_HIRE, CA_JOLTS_HIRE, CO_JOLTS_HIRE, CT_JOLTS_HIRE, DE_JOLTS_HIRE, DC_JOLTS_HIRE, FL_JOLTS_HIRE, GA_JOLTS_HIRE, HI_JOLTS_HIRE, ID_JOLTS_HIRE, IL_JOLTS_HIRE, IN_JOLTS_HIRE, IA_JOLTS_HIRE, KS_JOLTS_HIRE, KY_JOLTS_HIRE, LA_JOLTS_HIRE, ME_JOLTS_HIRE, MD_JOLTS_HIRE, MA_JOLTS_HIRE, MI_JOLTS_HIRE, MN_JOLTS_HIRE, MS_JOLTS_HIRE, MO_JOLTS_HIRE, MT_JOLTS_HIRE, NE_JOLTS_HIRE, NV_JOLTS_HIRE, NH_JOLTS_HIRE, NJ_JOLTS_HIRE, NM_JOLTS_HIRE, NY_JOLTS_HIRE, NC_JOLTS_HIRE, ND_JOLTS_HIRE, OH_JOLTS_HIRE, OK_JOLTS_HIRE, OR_JOLTS_HIRE, PA_JOLTS_HIRE, RI_JOLTS_HIRE, SC_JOLTS_HIRE, SD_JOLTS_HIRE, TN_JOLTS_HIRE, TX_JOLTS_HIRE, UT_JOLTS_HIRE, VT_JOLTS_HIRE, VA_JOLTS_HIRE, WA_JOLTS_HIRE, WV_JOLTS_HIRE, WI_JOLTS_HIRE, WY_JOLTS_HIRE) %>%
  select(date, value, name, seriesID) %>%
  filter(date == max(date))

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)


states_JOLTS_HIRE <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states_JOLTS_HIRE <- states_JOLTS_HIRE %>%
  mutate(name = state_name)

states_JOLTS_HIRE <- left_join(states_JOLTS_HIRE, JOLTS_HIRE, by = "name") %>%
  drop_na()

states_territories_centroids_JOLTS_HIRE <- get_urbn_map("territories_states", sf = TRUE) %>%
  filter(state_fips != 69 & state_fips != 60 & state_fips != 66 & state_fips != 72 & state_fips != 78) %>% #ex guam, northern mariana islands, PR, guam, and American Samoa
  st_as_sf() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(long = X, lat = Y) %>% 
  bind_cols(states_JOLTS_HIRE, .) %>%
  st_centroid()

states_territories_labls_JOLTS_HIRE <- get_urbn_labels(map = "territories") %>%
  left_join(states_JOLTS_HIRE, by = "state_abbv") %>%
  select(-geometry) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)




JOLTS_STATE_RAINBOW <- states_JOLTS_HIRE  %>%
  ggplot(aes(fill = value/100)) +
  geom_sf(color = NA) +
  geom_sf(data = states_JOLTS_HIRE, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  #scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F","#AFEEEE","#AED581", "#00A99D","#3083DC"), #Commenting out old color scheme
  scale_fill_gradientn(colors = c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC"),label = scales::percent_format(accuracy = .1),breaks = c(0,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.055,0.06,0.065,0.07,0.075,0.08), expand = c(0,0)) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("NH")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 380000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("VT")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 150000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("MA")), 
    aes(x = 1600000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"),color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 100000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("RI")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 50000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("CT")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -125000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("NJ")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -130000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("DE")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("MD")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -390000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("DC")), 
    aes(x = 2700000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -590000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("HI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = -75000,nudge_x = -200000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("PR")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_label(
    data = filter(states_territories_centroids_JOLTS_HIRE, state_abbv %in% c("VI")), 
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), 
    size = 3.5, 
    hjust = 0.5,
    nudge_y = 0,nudge_x = 400000, # adjust these values as needed
    #segment.color = 'white',
    fontface = "bold",
    lineheight = 0.75,
    show.legend = FALSE
  ) +
  geom_text(data = filter(states_territories_labls_JOLTS_HIRE, !state_abbv %in% c("VI","PR","HI","VT","RI","CT","MA","NJ","NH","DC","DE","MD","LA","KY","WV","MP","AS","GU","FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 3, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_JOLTS_HIRE, state_abbv %in% c("FL","IN","TN")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.5, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_JOLTS_HIRE, state_abbv %in% c("LA","KY")), aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  geom_text(data = filter(states_territories_labls_JOLTS_HIRE, state_abbv %in% c("WV")), aes(x = st_coordinates(geometry)[,1]-25000, y = st_coordinates(geometry)[,2], label = paste0(state_abbv, "\n", ifelse(value >= 0, " ", ""), sprintf("%.1f", round(value, 1)), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), size = 2.25, check_overlap = TRUE,fontface = "bold",lineheight = 0.75) +
  ggtitle(paste("                State Hiring Rate,", format(ymd(states_JOLTS_HIRE$date[1]), "%B %Y"))) +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(dpi = "retina",plot = JOLTS_STATE_RAINBOW, "JOLTS State Rainbow.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

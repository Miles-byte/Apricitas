pacman::p_load(zctaCrosswalk,zipcodeR,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,openxlsx,sf,tidycensus,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

HOMEOWN_FREE_CLEAR <- get_acs(
  geography = "state",
  #variables = DP04,
  table = "DP04",
  cache_table = TRUE,
  year = 2021,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs1",
  show_call = TRUE
)

HOMEOWN_FREE_CLEAR_PCT <- HOMEOWN_FREE_CLEAR %>%
  select(GEOID, NAME, DP04_0092PE)

states <- get_urbn_map("territories_states", sf = TRUE) %>%
  st_as_sf()

states <- states %>%
  mutate(NAME = state_name)

states <- merge(states, HOMEOWN_FREE_CLEAR_PCT, by = "NAME") %>%
  mutate(PCT_CUT = cut(DP04_0092PE, breaks = c(-Inf,30,35,40,45,Inf), labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")))

HOMEOWN_FREE_CLEAR_PCT_MAP <- ggplot() +
  geom_sf(data = states, aes(fill = PCT_CUT), color = NA) +
  geom_sf(data = states, color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    na.value = "grey50", 
                    guide = "legend", 
                    labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  ggtitle("    Percent of Homeowners Without a Mortgage") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Census ACS 1-Year data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

ggsave(dpi = "retina",plot = HOMEOWN_FREE_CLEAR_PCT_MAP, "HOMEOWN_PCT_SHARE_STATE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

MORTGAGE_VALUE_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Have%20Mortgage%20Rates%20Frozen%20the%20Housing%20Market/MORTGAGE_VALUE_DATA.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  setNames(c("date","No Mortgage","<20%","20-39%","40-59%","60-79%","80-89%","90-99%","100%+","Unreported")) %>%
  rowwise() %>% 
  mutate(Total_Excluding_Unreported = sum(c_across(`<20%`:`100%+`))) %>%
  mutate(across(`<20%`:`100%+`, ~ .x + (.x / Total_Excluding_Unreported) * Unreported)) %>%
  ungroup() %>% 
  rowwise() %>%
  mutate(New_Total = sum(c_across(`No Mortgage`:`100%+`))) %>%
  mutate(across(`No Mortgage`:`100%+`, ~ .x / New_Total * 100)) %>%
  select(date, `No Mortgage`:`100%+`) %>%
  pivot_longer(cols = `No Mortgage`:`100%+`, names_to = "name", values_to = "value") %>%
  mutate(name = factor(name, levels = rev(c("No Mortgage","<20%","20-39%","40-59%","60-79%","80-89%","90-99%","100%+"))))

MORTGAGE_VALUE_DATA_graph <- ggplot(data = MORTGAGE_VALUE_DATA, aes(x = date, y = value/100, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("% of Homeowners") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.25,.5,.75,1), limits = c(0,1), expand = c(0,0)) +
  ggtitle("Distribution of Homeowners\nBy Mortgage % of Home Value") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "42% of Homeowners Have No Mortgage Balance Remaining as of 2021") +
  theme_apricitas + theme(legend.position = "right") +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Mortgage Loan\n% of Home Value",values = c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MORTGAGE_VALUE_DATA_graph, "Mortgage Value Data Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HOMEOWN_FREE_CLEAR_COUNTY <- get_acs(
  geography = "county",
  #variables = DP04,
  table = "DP04",
  cache_table = TRUE,
  year = 2021,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE
)

HOMEOWN_FREE_CLEAR_COUNTY <- HOMEOWN_FREE_CLEAR_COUNTY %>%
  transmute(GEOID, county = NAME, value = DP04_0092PE)

state_lookup <- data.frame(
  abbreviation = c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 
                   'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 
                   'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 
                   'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 
                   'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY'),
  name = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 
           'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 
           'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 
           'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 
           'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 
           'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 
           'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 
           'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 
           'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
)



REDFIN_COUNTY_BULK <- read.csv("C:/Users/josep/Documents/COUNTY_HOUSING_DATA_08_15_23.csv")

REDFIN_COUNTY_INVENTORY <- REDFIN_COUNTY_BULK %>%
  filter(period_begin %in% c("6/1/2021","6/1/2023") & property_type_id == "-1") %>%
  select(period_begin, table_id, region, inventory, inventory_yoy) %>%
  group_by(region) %>%
  arrange(region) %>%
  mutate(inventory_2yo2y = (inventory[2]-inventory[1])/inventory[1]) %>%
  filter(period_begin == "6/1/2023") %>%
  ungroup() %>%
  transmute(name = region, inventory_yoy, inventory_2yo2y) %>%
  separate(name, into = c("County", "State"), sep = ", ") %>%
  left_join(state_lookup, by = c("State" = "abbreviation")) %>%
  unite(county, County, name, sep = ", ", remove = TRUE) %>%
  select(-State) %>%
  mutate(county = replace(county, county == "Alexandria, Virginia","Alexandria city, Virginia"),
         county = replace(county, county == "King & Queen County, Virginia","King and Queen County, Virginia"),
         county = replace(county, county == "District of Columbia, NA","District of Columbia, District of Columbia"),
         county = replace(county, county == "Anchorage Borough, Alaska","Anchorage Municipality, Alaska"),
         county = replace(county, county == "Baltimore City County, Maryland","Baltimore city, Maryland"),
         county = replace(county, county == "Winchester, Virginia","Winchester city, Virginia"),
         county = replace(county, county == "Waynesboro, Virginia","Waynesboro city, Virginia"),
         county = replace(county, county == "Williamsburg, Virginia","Williamsburg city, Virginia"),
         county = replace(county, county == "Virginia Beach, Virginia","Virginia Beach city, Virginia"),
         county = replace(county, county == "Suffolk, Virginia","Suffolk city, Virginia"),
         county = replace(county, county == "Bristol, Virginia","Bristol city, Virginia"),
         county = replace(county, county == "Staunton, Virginia","Staunton city, Virginia"),
         county = replace(county, county == "St. Louis City County, Missouri","St. Louis city, Missouri"),
         county = replace(county, county == "Salem, Virginia","Salem city, Virginia"),
         county = replace(county, county == "Buena Vista, Virginia","Buena Vista city, Virginia"),
         county = replace(county, county == "Charlottesville, Virginia","Charlottesville city, Virginia"),
         county = replace(county, county == "Chesapeake, Virginia","Chesapeake city, Virginia"),
         county = replace(county, county == "Colonial Heights, Virginia","Colonial Heights city, Virginia"),
         county = replace(county, county == "Covington, Virginia","Covington city, Virginia"),
         county = replace(county, county == "Danville, Virginia","Danville city, Virginia"),
         county = replace(county, county == "Emporia, Virginia","Emporia city, Virginia"),
         county = replace(county, county == "Fairfax City County, Virginia","Fairfax city, Virginia"),
         county = replace(county, county == "Roanoke City County, Virginia","Roanoke city, Virginia"),
         county = replace(county, county == "Richmond City County, Virginia","Richmond city, Virginia"),
         county = replace(county, county == "Radford, Virginia","Radford city, Virginia"),
         county = replace(county, county == "Portsmouth, Virginia","Portsmouth city, Virginia"),
         county = replace(county, county == "Poquoson, Virginia","Poquoson city, Virginia"),
         county = replace(county, county == "Falls Church, Virginia","Falls Church city, Virginia"),
         county = replace(county, county == "Franklin City County, Virginia","Franklin city, Virginia"),
         county = replace(county, county == "Fredericksburg, Virginia","Fredericksburg city, Virginia"),
         county = replace(county, county == "Galax, Virginia","Galax city, Virginia"),
         county = replace(county, county == "Hampton, Virginia","Hampton city, Virginia"),
         county = replace(county, county == "Harrisonburg, Virginia","Harrisonburg city, Virginia"),
         county = replace(county, county == "Hopewell, Virginia","Hopewell city, Virginia"),
         county = replace(county, county == "Lewis & Clark County, Montana","Lewis and Clark County, Montana"),
         county = replace(county, county == "Lexington, Virginia","Lexington city, Virginia"),
         county = replace(county, county == "Lynchburg, Virginia","Lynchburg city, Virginia"),
         county = replace(county, county == "Manassas Park, Virginia","Manassas Park city, Virginia"),
         county = replace(county, county == "Manassas, Virginia","Manassas city, Virginia"),
         county = replace(county, county == "Martinsville, Virginia","Martinsville city, Virginia"),
         county = replace(county, county == "Norfolk, Virginia","Norfolk city, Virginia"),
         county = replace(county, county == "Norton, Virginia","Norton city, Virginia"),
         county = replace(county, county == "Petersburg, Virginia","Petersburg city, Virginia"),
         county = replace(county, county == "Newport News, Virginia","Newport News city, Virginia"),
         county = replace(county, county == "Dona Ana County, New Mexico","Doña Ana County, New Mexico"),
  )

COUNTY_FREE_CLEAR_MERGE <- full_join(HOMEOWN_FREE_CLEAR_COUNTY,REDFIN_COUNTY_INVENTORY, by = "county")

COUNTY_FREE_CLEAR_REG_graph <- ggplot() + #plotting traditional Unemployment/PCE Inflation curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_point(data=COUNTY_FREE_CLEAR_MERGE, aes(x=value/100,y=inventory_yoy, color= "1-year Change in Inventory"), size = 0.01)+
  stat_smooth(data=COUNTY_FREE_CLEAR_MERGE,method = "lm", aes(x=value/100,y=inventory_yoy, color= "1-year Change in Inventory"), size = 1.25) +
  geom_point(data=COUNTY_FREE_CLEAR_MERGE, aes(x=value/100,y=inventory_2yo2y, color= "2-year Change in Inventory"), size = 0.01)+
  stat_smooth(data=COUNTY_FREE_CLEAR_MERGE,method = "lm", aes(x=value/100,y=inventory_2yo2y, color= "2-year Change in Inventory"), size = 1.25) +
  ylab("Change in For-Sale\nHousing Inventory, %") +
  xlab("Share of Homeowners Without a Mortgage, %") +
  #geom_text_repel(data = FDIC_DATA_TICKER, aes(y = CUMUL_ABNORMAL, x = DEPUNA/DEPDOM, label = TICKER), hjust=0) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.125,.9), expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.75,.750), expand = c(0,0), breaks = c(-.75,-.5,-.25,0,0.25,0.5,0.75)) +
  ggtitle("Change in For-Sale Inventory vs\nShare of Homeowners Without Mortgages") +
  labs(caption = "Graph created by @JosephPolitano using Redfin and Census ACS 5-Year data", subtitle = "The Relationship Between Inventory Growth and Free-and Clear Ownership is Still Weak") +
  theme_apricitas + theme(legend.position = "top") +
  theme(axis.title.x = element_text(size = 20, hjust = 0.5),
        axis.title.y = element_text(size = 15, vjust = 0.5),
        plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D"))+
  guides(size = "none") + 
  annotation_custom(apricitas_logo_rast, xmin = .175-(.1861*.775), xmax = 0.175-(0.049*.775), ymin = -0.75-(.3*1.5), ymax = -0.75) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = COUNTY_FREE_CLEAR_REG_graph, "County Free Clear.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing  

HOME_PRICE_UNCERTAINTY_SCE_DATA <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY-SCE-Data.xlsx?sc_lang=en",10) %>%
  setNames(c("date","uncertainty")) %>%
  slice(-1,-2) %>%
  transmute(date = as.Date(paste0(date,"01"), "%Y%m%d"), uncertainty = as.numeric(uncertainty))

HOME_PRICE_EXPECTATIONS_SCE_DATA <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY-SCE-Data.xlsx?sc_lang=en",8) %>%
  setNames(c("date","median","25th_percentile","75th_percentile","median_1yr_point","median_3yr_point")) %>%
  slice(-1,-2) %>%
  mutate(date = as.Date(paste0(date,"01"), "%Y%m%d")) %>%
  mutate(across(where(is.character), as.numeric))

HOME_PRICE_UNCERTAINTY_SCE_DATA_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = HOME_PRICE_UNCERTAINTY_SCE_DATA, aes(x = date, y = uncertainty/100, color = "Median One-Year Ahead Home Price Change Uncertainty\nFRBNY Survey of Consumer Expectations"), size = 1.25) +
  xlab("Date") +
  ylab("Uncertainty in Home Price Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,.04,.05,.06), limits = c(0,.05), expand = c(0,0)) +
  ggtitle("Uncertainty About Home Prices is High") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data", subtitle = "Home Price Uncertainty is Declining But Still Elevated Amidst Higher Rates") +
  theme_apricitas + theme(legend.position = c(.5,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*0.05), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HOME_PRICE_UNCERTAINTY_SCE_DATA_graph, "Home Price Uncertainty.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HOME_PRICE_EXPECTATIONS_SCE_DATA_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = HOME_PRICE_EXPECTATIONS_SCE_DATA, aes(x = date, y = median/100, color = "Median One-Year Ahead Home Price Change Expectations\nFRBNY Survey of Consumer Expectations"), size = 1.25) +
  xlab("Date") +
  ylab("Expected Home Price Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,.04,.05,.06), limits = c(0,.065), expand = c(0,0)) +
  ggtitle("Home Price Expectations Have Normalized") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data", subtitle = "Home Price Growth Expectations are Recovering Even Amidst Higher Mortgage Rates") +
  theme_apricitas + theme(legend.position = c(.37,.79)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*0.06), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = HOME_PRICE_EXPECTATIONS_SCE_DATA_graph, "Home Price Expectations.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ANNUAL_SCE_HOUSING_OWNER_TENURE <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY_SCE_Housing_chartdata.xlsx?sc_lang=en",30) %>%
  setNames(c("date","10+ Years","6-10 Years","2-5 Years","<2 Years")) %>%
  slice(-1,-2) %>%
  mutate(date = as.Date(paste0(date,"01"), "%Y%m%d")) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  pivot_longer(cols = -date) %>%
  mutate(name = factor(name, levels = c("<2 Years","2-5 Years","6-10 Years","10+ Years")))

ANNUAL_SCE_HOUSING_OWNER_TENURE_graph <- ggplot(data = ANNUAL_SCE_HOUSING_OWNER_TENURE, aes(x = date, y = value/100, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("% of Homeowners") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.25,.5,.75,1), limits = c(0,1.01), expand = c(0,0)) +
  ggtitle("Homeowners' Expected Tenure Has Fallen") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data", subtitle = "Compared to Pre-Pandemic, Tenure Expectations for Homeowners Have Declined Significantly") +
  theme_apricitas + theme(legend.position = "right") +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Expected Tenure",values = c("#9A348E","#00A99D","#EE6055","#FFE98F","#FF8E72","#6A4C93","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*1), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ANNUAL_SCE_HOUSING_OWNER_TENURE_graph, "Annual Housing SCE Tenure Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

ANNUAL_SCE_HOUSING_PROB_MOVE <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY_SCE_Housing_chartdata.xlsx?sc_lang=en",15) %>%
  setNames(c("date","1yr_<50","1yr_>50","1yr<BA","1yr>BA","1yr<60k","1yr>60k","1yrWest","1yrMidwest","1yrSouth","1yrNortheast","1yrOwner","1yrRenter",
             "date2","3yr_<50","3yr_>50","3yr<BA","3yr>BA","3yr<60k","3yr>60k","3yrWest","3yrMidwest","3yrSouth","3yrNortheast","3yrOwner","3yrRenter")) %>%
  slice(-1,-2,-3) %>%
  mutate(date = as.Date(paste0(date,"01"), "%Y%m%d")) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  select(-date2)

ANNUAL_SCE_HOUSING_PROB_MOVE_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = ANNUAL_SCE_HOUSING_PROB_MOVE, aes(x = date, y = `1yrOwner`/100, color = "Next Year"), size = 1.25) +
  geom_line(data = ANNUAL_SCE_HOUSING_PROB_MOVE, aes(x = date, y = `3yrOwner`/100, color = "Next 3 Years"), size = 1.25) +
  xlab("Date") +
  ylab("Average Probability of Moving, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.1,0.15,0.2), limits = c(0,.225), expand = c(0,0)) +
  ggtitle("Homeowners' Probability of Moving") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data", subtitle = "Homeowners Rate Their Short-Term Probability of Moving as Historically Low") +
  theme_apricitas + theme(legend.position = c(.37,.29)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Average Moving Probability, Homeowners",values = c("#FFE98F","#00A99D","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055"), breaks = c("Next Year","Next 3 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*0.225), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ANNUAL_SCE_HOUSING_PROB_MOVE_graph, "Homeowner Moving Probabilities.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

REDFIN_NATIONAL_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Have%20Mortgage%20Rates%20Frozen%20the%20Housing%20Market/REDFIN_NATIONAL_DATA.csv") %>%
  mutate(date = as.Date(period_begin,"%m/%d/%Y")) %>%
  filter(is_seasonally_adjusted == "t",property_type_id == "-1") %>%
  select(date, inventory)

REDFIN_NATIONAL_DATA_INVENTORY_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = REDFIN_NATIONAL_DATA, aes(x = date, y = `inventory`/1000000, color = "All Homes For Sale, Redfin Estimate"), size = 1.25) +
  xlab("Date") +
  ylab("Housing Inventory, Millions") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5, suffix = "M"), breaks = c(0,0.5,1,1.5,2), limits = c(0,2.25), expand = c(0,0)) +
  ggtitle("Housing Inventory is Extremely Low") +
  labs(caption = "Graph created by @JosephPolitano using Redfin data", subtitle = "Rising Interest Rates Have Brought Housing Inventory to Near 10 Year Lows") +
  theme_apricitas + theme(legend.position = c(.37,.29)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 0-(.3*2.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = REDFIN_NATIONAL_DATA_INVENTORY_graph, "Redfin National Inventory Data graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

FHFA_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Have%20Mortgage%20Rates%20Frozen%20the%20Housing%20Market/FHFA_NMDB_MORTGAGE_STATISTICS.csv") %>%
  filter(MARKET == "All Mortgages" & SERIESID == "AVE_INTRATE" & GEOID == "USA") %>%
  transmute(date = as.Date(as.yearqtr(PERIOD, "%YQ%q")), value = VALUE1)

FIXED_30YR_MORTGAGE <- fredr(series_id = "MORTGAGE30US", observation_start = as.Date("2013-01-01"))

FIXED_30YR_FHFA_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = FHFA_DATA, aes(x = date+45, y = value/100, color = "Average Interest Rate on Outstanding Mortgages"), size = 1.25) +
  geom_line(data = FIXED_30YR_MORTGAGE, aes(x = date, y = value/100, color = "Average Interest Rate on New 30-Year Mortgage"), size = 1.25) +
  xlab("Date") +
  ylab("Interest Rate, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.025,0.05,0.075), limits = c(0,.075), expand = c(0,0)) +
  ggtitle("Low Mortgage Rates as Golden Handcuffs?") +
  labs(caption = "Graph created by @JosephPolitano using FHFA and Freddie Mac data", subtitle = "Interest Rates on New 30-Year Mortgages Have Surged Above Rates for Average Existing Borrowers") +
  theme_apricitas + theme(legend.position = c(.37,.29)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = 0-(.3*0.075), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIXED_30YR_FHFA_graph, "Fixed 30yr FHFA graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HOMEOWN_FREE_CLEAR_ZIP <- get_acs(
  geography = "zcta",
  variables = "DP04_0092PE",
  #table = "DP04",
  cache_table = TRUE,
  year = 2021,
  output = "wide",
  key = Sys.getenv("CENSUS_KEY"),
  moe_level = 90,
  survey = "acs5",
  show_call = TRUE,
)

ZIP <- zctas(year = 2020)
options(tigris_use_cache = TRUE)

ZIP_ACS_MERGE <- left_join(ZIP, HOMEOWN_FREE_CLEAR_ZIP, by = c("GEOID20" = "GEOID")) %>%
  transmute(GEOID20, value = DP04_0092PE, geometry) %>%
  mutate(value_cut = cut(value, breaks = c(-Inf,30,35,40,45,Inf), labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")))


# ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
# temp <- tempfile()
# download.file(ZipCodeSourceFile , temp)
# ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
# unlink(temp)
# names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
#                     "AdminName1", "AdminCode1", "AdminName2", "AdminCode2", 
#                     "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
# ZipCodes$zip <- str_pad(as.character(ZipCodes$zip), width = 5, pad = "0")
# 
# ZipCodes <- ZipCodes %>%
#   filter(!duplicated(zip))

states <- states(2020, resolution = "500k", cb = TRUE)

PCT_DC <- ggplot() +
  geom_sf(data = subset(ZIP_ACS_MERGE, GEOID20 %in% get_zctas_by_state(c("DC","MD","VA"))& !is.na(value_cut)), aes(fill = value_cut, color = value_cut)) +
  geom_sf(data = subset(states, STUSPS %in% c("DC","MD","VA")), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    guide = "legend", 
                    labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                     guide = NULL, 
                     labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  ggtitle("    Percent of Homeowners Without a Mortgage") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = PCT_DC, "PCT DC graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

PCT_WEST <- ggplot() +
  geom_sf(data = subset(ZIP_ACS_MERGE, GEOID20 %in% get_zctas_by_state(c("CA","OR","WA","ID","UT","NV","NM","AZ","CO","MT","WY"))& !is.na(value_cut)), aes(fill = value_cut, color = value_cut)) +
  geom_sf(data = subset(states, STUSPS %in% c("CA","OR","WA","ID","UT","NV","NM","AZ","CO","MT","WY")), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    guide = "legend", 
                    labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                     guide = NULL, 
                     labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  ggtitle("    Percent of Homeowners Without a Mortgage") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = PCT_WEST, "PCT WEST.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PCT_NORTHEAST <- ggplot() +
  geom_sf(data = subset(ZIP_ACS_MERGE, GEOID20 %in% get_zctas_by_state(c("ME","NH","VT","RI","NJ","NY","PA","MA","CT","PA","DC","MD","VA","WV"))& !is.na(value_cut)), aes(fill = value_cut, color = value_cut)) +
  geom_sf(data = subset(states, STUSPS %in% c("ME","NH","VT","RI","NJ","NY","PA","MA","CT","PA","DC","MD","VA","WV")), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    guide = "legend", 
                    labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                     guide = NULL, 
                     labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  ggtitle("    Percent of Homeowners Without a Mortgage") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = PCT_NORTHEAST, "PCT NORTHEAST.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PCT_SOUTH <- ggplot() +
  geom_sf(data = subset(ZIP_ACS_MERGE, GEOID20 %in% get_zctas_by_state(c("NC","SC","GA","AL","MS","FL","LA","TX","TN","AR","OK"))& !is.na(value_cut)), aes(fill = value_cut, color = value_cut)) +
  geom_sf(data = subset(states, STUSPS %in% c("NC","SC","GA","AL","MS","FL","LA","TX","TN","AR","OK")), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    guide = "legend", 
                    labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                     guide = NULL, 
                     labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  ggtitle("    Percent of Homeowners Without a Mortgage") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = PCT_SOUTH, "PCT SOUTH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PCT_MIDWEST <- ggplot() +
  geom_sf(data = subset(ZIP_ACS_MERGE, GEOID20 %in% get_zctas_by_state(c("OH","MI","WI","MN","IA","MO","KS","NE","SD","ND","IL","KY","IN"))& !is.na(value_cut)), aes(fill = value_cut, color = value_cut)) +
  geom_sf(data = subset(states, STUSPS %in% c("OH","MI","WI","MN","IA","MO","KS","NE","SD","ND","IL","KY","IN")), color = "black", fill = NA, lwd = 0.65) + # Black borders for states
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") + #albers projection
  scale_fill_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                    guide = "legend",
                    breaks = c("<30%", "30-35%", "35-40%", "40-45%", ">45%"),
                    labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  scale_color_manual(values = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D"),
                     guide = NULL,
                     breaks = c("<30%", "30-35%", "35-40%", "40-45%", ">45%"),
                     labels = c("<30%", "30-35%", "35-40%", "40-45%", ">45%")) +
  ggtitle("    Percent of Homeowners Without a Mortgage") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
  theme(plot.title = element_text(size = 24,hjust = 0.5))

ggsave(dpi = "retina",plot = PCT_MIDWEST, "PCT MIDWEST.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

#ANNUAL SURVEY MOVING EXPECTATIONS
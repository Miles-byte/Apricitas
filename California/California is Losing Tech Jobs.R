pacman::p_load(ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

CA_ALL_JOBS_NEW <- fredr("CANA", observation_start = as.Date("2022-01-01"))

CA_ALL_JOBS_OLD <- fredr("CANA", observation_start = as.Date("2022-01-01"), realtime_start = as.Date("2024-01-23"),realtime_end = as.Date("2024-01-23"))

CA_INFO_JOBS_NEW <- fredr("SMS06000005000000001", observation_start = as.Date("2022-01-01"))

CA_INFO_JOBS_OLD <- fredr("SMS06000005000000001", observation_start = as.Date("2022-01-01"), realtime_start = as.Date("2024-01-23"),realtime_end = as.Date("2024-01-23"))

CA_PROF_JOBS_NEW <- fredr("CAPBSV", observation_start = as.Date("2022-01-01"))

CA_PROF_JOBS_OLD <- fredr("CAPBSV", observation_start = as.Date("2022-01-01"), realtime_start = as.Date("2024-01-23"),realtime_end = as.Date("2024-01-23"))


CA_INFO_PROF_NEW <- merge(CA_INFO_JOBS_NEW,CA_PROF_JOBS_NEW, by = "date") %>%
  transmute(date, value = value.x+value.y)

CA_INFO_PROF_OLD <- merge(CA_INFO_JOBS_OLD,CA_PROF_JOBS_OLD, by = "date") %>%
  transmute(date, value = value.x+value.y)

CA_JOB_REVISIONS_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= CA_INFO_PROF_NEW, aes(x=date,y= value-value[1], color= "Professional, Business, and Information Sectors"), size = 1.25) +
  geom_line(data= CA_INFO_PROF_OLD, aes(x=date,y= value-value[1], color= "Professional, Business, and Information Sectors"), size = 0.75, linetype = "dashed") +
  geom_line(data= CA_ALL_JOBS_NEW, aes(x=date,y= value-value[1], color= "All Sectors"), size = 1.25) +
  geom_line(data= CA_ALL_JOBS_OLD, aes(x=date,y= value-value[1], color= "All Sectors"), size = 0.75, linetype = "dashed") +
  xlab("Date") +
  ylab("California Job Growth Since 2022") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-200,0,200,400,600,800), limits = c(-200,950), expand = c(0,0)) +
  ggtitle("California Job Growth Revisions") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "CA Job Growth Was Heavily Revised Down, Especially in Professional, Business, and Information") +
  theme_apricitas + theme(legend.position = c(.3,.89)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Dashed = Old, Solid = New Revised",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -200-(.3*1150), ymax = -200) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CA_JOB_REVISIONS_graph, "CA Job Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_DATA_PROCESSING <- bls_api("CEU5051800001", startyear = 1990, registrationKey = "BLS_KEY") %>% #data processing employment
  rbind(bls_api("CEU5051800001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Computing Infrastructure, Data Processing, Web Hosting, & Related") 

US_SOFTWARE_PUBLISHERS <- bls_api("CEU5051320001", startyear = 1990, registrationKey = "BLS_KEY") %>% #software employment
  rbind(bls_api("CEU5051320001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Software Publishers") 

US_SEARCH_PORTALS <- bls_api("CEU5051900001", startyear = 1990, registrationKey = "BLS_KEY") %>% #internet employment
  rbind(bls_api("CEU5051900001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Web Search Portals, Libraries, and Other Information Services")

US_MEDIA_SOCIAL <- bls_api("CEU5051620001", startyear = 1990, registrationKey = "BLS_KEY") %>% #internet employment
  rbind(bls_api("CEU5051620001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Streaming Services, Social Networks, & Related")

US_COMPUTER_SYSTEM_DESIGN <- bls_api("CEU6054150001", startyear = 1990, registrationKey = "BLS_KEY") %>% #internet employment
  rbind(bls_api("CEU6054150001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Computer Systems Design Services & Related")



CA_DATA_PROCESSING <- bls_api("SMU06000005051800001", startyear = 1990, registrationKey = "BLS_KEY") %>% #software employment
  rbind(bls_api("SMU06000005051800001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Computing Infrastructure, Data Processing, Web Hosting, & Related")

CA_SOFTWARE_PUBLISHERS <- bls_api("SMU06000005051320001", startyear = 1990, registrationKey = "BLS_KEY") %>% #software employment
  rbind(bls_api("SMU06000005051320001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Software Publishers")

CA_SEARCH_PORTALS <- bls_api("SMU06000005051900001", startyear = 1990, registrationKey = "BLS_KEY") %>% #software employment
  rbind(bls_api("SMU06000005051900001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Web Search Portals, Libraries, and Other Information Services")

CA_MEDIA_SOCIAL <- bls_api("SMU06000005051620001", startyear = 1990, registrationKey = "BLS_KEY") %>% #software employment
  rbind(bls_api("SMU06000005051620001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Streaming Services, Social Networks, & Related")

CA_COMPUTER_SYSTEM_DESIGN <- bls_api("SMU06000006054150001", startyear = 1990, registrationKey = "BLS_KEY") %>% #software employment
  rbind(bls_api("SMU06000006054150001", startyear = 2010, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Computer Systems Design Services & Related") 

CA_US_DATA_PROCESSING <- merge(CA_DATA_PROCESSING, US_DATA_PROCESSING, by = "date") %>%
  transmute(date, value = value.x/value.y)

CA_US_SOFTWARE_PUBLISHERS <- merge(CA_SOFTWARE_PUBLISHERS, US_SOFTWARE_PUBLISHERS, by = "date") %>%
  transmute(date, value = value.x/value.y)

CA_US_SEARCH_PORTALS <- merge(CA_SEARCH_PORTALS, US_SEARCH_PORTALS, by = "date") %>%
  transmute(date, value = value.x/value.y)

CA_US_MEDIA_SOCIAL <- merge(CA_MEDIA_SOCIAL, US_MEDIA_SOCIAL, by = "date") %>%
  transmute(date, value = value.x/value.y)

CA_US_COMPUTER_SYSTEM_DESIGN <- merge(CA_COMPUTER_SYSTEM_DESIGN, US_COMPUTER_SYSTEM_DESIGN, by = "date") %>%
  transmute(date, value = value.x/value.y)

CA_US_TECH_SHARE_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(CA_US_DATA_PROCESSING, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "Computing Infrastructure,\nData Processing,\nWeb Hosting, & Related"), size = 1.25) +
  geom_line(data= filter(CA_US_SOFTWARE_PUBLISHERS, date > as.Date("2010-01-01")), aes(x=date,y= value, color= "Software Publishers"), size = 1.25) +
  geom_line(data= filter(CA_US_SEARCH_PORTALS, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "Web Search Portals, Libraries,\n& Related"), size = 1.25) +
  geom_line(data= filter(CA_US_MEDIA_SOCIAL, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "Streaming Services, Social Networks,\n& Related"), size = 1.25) +
  geom_line(data= filter(CA_US_COMPUTER_SYSTEM_DESIGN, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "Computer Systems Design,\nCustom Programming, & Related"), size = 1.25) +
  xlab("Date") +
  ylab("Share of US Tech Jobs in California, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.1,.2,.3,.4,.5), limits = c(0,.45), expand = c(0,0)) +
  ggtitle("Share of US Tech Jobs in California, %") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "California's Share of Tech Employment Has Fallen After Rising Significantly in the 2010s") +
  theme_apricitas + theme(legend.position = "right") +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055","#FFE98F")), breaks = c("Software Publishers","Computer Systems Design,\nCustom Programming, & Related","Computing Infrastructure,\nData Processing,\nWeb Hosting, & Related","Web Search Portals, Libraries,\n& Related","Streaming Services, Social Networks,\n& Related")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2003-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2003-01-01"))), ymin = 0-(.3*.45), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  theme(legend.text =  element_text(size = 12, color = "white")) +
  theme(legend.key.height = unit(1.5, "cm"), # Adjusts the height of the legend keys
  legend.spacing.y = unit(1.5, "cm")) +
  #guides(color = guide_legend(ncol = 2)) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CA_US_TECH_SHARE_graph, "CA US Tech Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CA_TECH_TOTAL <- rbind(CA_DATA_PROCESSING, CA_SOFTWARE_PUBLISHERS, CA_SEARCH_PORTALS, CA_MEDIA_SOCIAL, CA_COMPUTER_SYSTEM_DESIGN) %>%
  group_by(date) %>%
  summarise(CA = sum(value, na.rm = TRUE))

US_TECH_TOTAL <- rbind(US_DATA_PROCESSING, US_SOFTWARE_PUBLISHERS, US_SEARCH_PORTALS, US_MEDIA_SOCIAL, US_COMPUTER_SYSTEM_DESIGN) %>%
  group_by(date) %>%
  summarise(US = sum(value, na.rm = TRUE))

CA_US_TECH <- merge(CA_TECH_TOTAL, US_TECH_TOTAL, by = "date") %>%
  transmute(date, value = CA/US)

CA_TECH_JOB_SHARE_graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data= filter(CA_US_TECH, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "Percent of All US Tech Jobs Located in California"), size = 1.25) +
  xlab("Date") +
  ylab("Percent of US Tech Jobs") +
  annotate("text",label = "NOTE: Tech Included Software Publishers, Computer Systems Design, Computing Infrastructure,\nData Processing, Web Hosting, Web Search Portals, Social Media, and Streaming Services", hjust = 0, x = as.Date("2011-07-01"), y =.1485, color = "white", size = 4, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(.15,.16,.17,.18,.19), limits = c(.145,.19), expand = c(0,0)) +
  ggtitle("Tech Jobs are Leaving California") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "California's Share of Total Tech-Sector Employment Has Fallen Significantly Since 2020") +
  theme_apricitas + theme(legend.position = c(.32,.96)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = .145-(.3*.045), ymax = .145) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CA_TECH_JOB_SHARE_graph, "CA Tech Job Share.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SF_INFO <- bls_api("SMU06418605000000001", startyear = 2009, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06418605000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Information—San Francisco") 

SJ_INFO <- bls_api("SMU06419405000000001", startyear = 2009, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06419405000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Information—San Jose")

SF_SJ_INFO_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data= filter(SF_INFO, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "San Francisco Metro Area"), size = 1.25) +
  geom_line(data= filter(SJ_INFO, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "San Jose Metro Area"), size = 1.25) +
  xlab("Date") +
  ylab("Employees, Thousands, Not SeasonallY Adjusted") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,20,40,60,80,100,120,140,160), limits = c(0,170), expand = c(0,0)) +
  ggtitle("Information Sector Jobs in the Bay Area") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Bay Area Has Started Losing Information-Sector Jobs Since Mid-2022") +
  theme_apricitas + theme(legend.position = c(.3,.89)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "All Employees, Information",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*170), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SF_SJ_INFO_Graph, "SF SJ Info Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SF_INFO_GROWTH <- SF_INFO %>%
  mutate(value = value-lag(value,12)) 

SJ_INFO_GROWTH <- SJ_INFO %>%
  mutate(value = value-lag(value,12))

SF_SJ_INFO_GROWTH_Graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(SJ_INFO_GROWTH, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "San Jose Metro Area"), size = 1.25) +
  geom_line(data= filter(SF_INFO_GROWTH, date >= as.Date("2010-01-01")), aes(x=date,y= value, color= "San Francisco Metro Area"), size = 1.25) +
  xlab("Date") +
  ylab("Year-on-Year Growth, Employees, Thousands") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-20,-10,0,10,20), limits = c(-20,20), expand = c(0,0)) +
  ggtitle("Information Sector Job Growth in the Bay") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Bay Area Has Started Losing Information-Sector Jobs Since Mid-2022") +
  theme_apricitas + theme(legend.position = c(.20,.86)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "All Employees, Information\nYear-on-Year Change",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*170), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SF_SJ_INFO_GROWTH_Graph, "SF SJ Info Growth Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


RGDP_CA <- fredr(series_id = "CARQGSP", observation_start = as.Date("2019-01-01"), units = "pc1") %>%
  mutate(category = "Real GDP", state = "California")
RGDP_US <- fredr(series_id = "GDPC1", observation_start = as.Date("2019-01-01"), units = "pc1") %>%
  mutate(category = "Real GDP", state = "United States")

INFO_GDP_CA <- fredr("CAINFORQGSP", observation_start = as.Date("2019-01-01"), units = "pc1") %>%
  mutate(category = "Real Information Value Added", state = "California")
INFO_GDP_US <- fredr("RVAI", observation_start = as.Date("2019-01-01"), units = "pc1") %>%
  mutate(category = "Real Information Value Added", state = "United States")

INFO_RGDP_CA_US_RBIND <- rbind(RGDP_CA,RGDP_US,INFO_GDP_CA,INFO_GDP_US)
  

CALIFORNIA_GDP_INFO_FACET_Graph <- ggplot(INFO_RGDP_CA_US_RBIND, aes(x = date, y = value/100, color = state)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(size = 1.25) +
  facet_wrap(~ category) +
  xlab("Date") +
  ylab("Real Growth, Year-over-Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.10,0,.10,.20), limits = c(-.10,.25), expand = c(0,0)) +
  ggtitle("California and the Tech-cession") +
  labs(caption = "Graph created by @JosephPolitano using BEA data", subtitle = "California GDP Growth Turned Negative in 2022, Led by A Drop in Information") +
  theme_apricitas + theme(legend.position = "right", legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("United States","California")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.10-(.3*.35), ymax = -0.10) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm"))


ggsave(dpi = "retina",plot = CALIFORNIA_GDP_INFO_FACET_Graph, "CALIFORNIA GDP Info Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

CPI_RENT_US <- bls_api("CUUR0000SEHA", startyear = 2010, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06419405000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "United States") %>%
  mutate(value = value/lag(value,12))

CPI_RENT_SF <- bls_api("CUURS49BSEHA", startyear = 2010, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06419405000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "San Francisco") %>%
  mutate(value = value/lag(value,12))

CPI_RENT_LA <- bls_api("CUURS49ASEHA", startyear = 2010, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06419405000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Los Angeles") %>%
  mutate(value = value/lag(value,12))

CPI_RENT_SD <- bls_api("CUURS49ESEHA", startyear = 2010, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06419405000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "San Diego") %>%
  mutate(value = value/lag(value,12))

CPI_RENT_IE <- bls_api("CUURS49CSEHA", startyear = 2010, registrationKey = "BLS_KEY") %>% #software employment
  #rbind(bls_api("SMU06419405000000001", startyear = 2020, registrationKey = "BLS_KEY") %>% select(-latest)) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  arrange(date) %>%
  mutate(series_id = "Inland Empire") %>%
  mutate(value = value/lag(value,12))

CA_RENT_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(CPI_RENT_US, date >= as.Date("2015-01-01")), aes(x=date,y= value-1, color= "United States"), size = 2.25) +
  geom_line(data= filter(CPI_RENT_SF, date >= as.Date("2015-01-01")), aes(x=date,y= value-1, color= "San Francisco"), size = 1.25) +
  geom_line(data= filter(CPI_RENT_LA, date >= as.Date("2015-01-01")), aes(x=date,y= value-1, color= "Los Angeles"), size = 1.25) +
  geom_line(data= filter(CPI_RENT_SD, date >= as.Date("2015-01-01")), aes(x=date,y= value-1, color= "San Diego"), size = 1.25) +
  geom_line(data= filter(CPI_RENT_IE, date >= as.Date("2015-01-01")), aes(x=date,y= value-1, color= "Riverside-San Bernardino"), size = 1.25) +
  xlab("Date") +
  ylab("Percent Growth, Year-on-year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.02,.04,.06,.08,.1,.12), limits = c(-0.01,.13), expand = c(0,0)) +
  ggtitle("Rent Growth Across California") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "SF and LA Have Seen Much Slower Rent Growth than the US Average Since the Pandemic") +
  theme_apricitas + theme(legend.position = c(.35,.825), legend.key.height = unit(0,"cm"), plot.title = element_text(size = 27)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "CPI: Rent of Primary Residence, US vs CA Metro Areas",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")), breaks = c("United States","San Francisco","Los Angeles","San Diego","Riverside-San Bernardino"), guide=guide_legend(override.aes=list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = -0.01-(.3*.14), ymax = -0.01) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CA_RENT_graph, "CA Rent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


US_DATA_PROCESSING_IND <- bls_api("CES5051800001", startyear = 2020, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computing Infrastructure, Data Processing, Web Hosting, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

US_SOFTWARE_PUBLISHERS_IND <- bls_api("CES5051320001", startyear = 2020, registrationKey = "BLS_KEY") %>% #software employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Software Publishers") %>%
  mutate(value = (value-value[nrow(.)]))

US_SEARCH_PORTALS_IND <- bls_api("CES5051900001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Web Search Portals, Libraries, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

US_MEDIA_SOCIAL_IND <- bls_api("CES5051620001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Streaming Services, Social Networks, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

US_COMPUTER_SYSTEM_DESIGN_IND <- bls_api("CES6054150001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computer Systems Design, Custom Programming, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

US_TECH_EMPLOY_GROWTH_IND <- rbind(US_DATA_PROCESSING_IND,US_MEDIA_SOCIAL_IND,US_SEARCH_PORTALS_IND,US_SOFTWARE_PUBLISHERS_IND,US_COMPUTER_SYSTEM_DESIGN_IND) %>%
  group_by(date) %>%
  filter(n() > 4) %>%
  mutate(series_id = factor(series_id,levels = rev(c("Software Publishers","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design, Custom Programming, & Related","Web Search Portals, Libraries, & Related","Streaming Services, Social Networks, & Related"))))

TECH_EMPLOY_GROWTH_INDSUM <- TECH_EMPLOY_GROWTH_IND %>%
  group_by(date) %>%
  summarise(sum_value = sum(value, na.rm = TRUE))

TECH_EMPLOY_GROWTH_IND_graph <- ggplot(data = US_TECH_EMPLOY_GROWTH_IND, aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,200,400,600,800), limits = c(-75,800), expand = c(0,0)) +
  ggtitle("US Tech Sector Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Employment in the Tech Sector Stagnated in 2023, But Has Not Declined") +
  theme_apricitas + theme(legend.position = c(0.35,0.83), legend.key.size = unit(0.5,"cm"), legend.spacing.y = unit(0, "cm")) +
  scale_fill_manual(name= "Change in Employment since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#3083DC","#6A4C93","#A7ACD9"), breaks = c("Software Publishers","Computer Systems Design, Custom Programming, & Related","Computing Infrastructure, Data Processing, Web Hosting, & Related","Web Search Portals, Libraries, & Related","Streaming Services, Social Networks, & Related")) +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -75-(.3*875), ymax = -75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TECH_EMPLOY_GROWTH_IND_graph, "Tech Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


CA_DATA_PROCESSING_IND <- bls_api("SMU06000005051800001", startyear = 2020, registrationKey = "BLS_KEY") %>% #data processing employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computing Infrastructure, Data Processing, Web Hosting, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

CA_SOFTWARE_PUBLISHERS_IND <- bls_api("SMU06000005051320001", startyear = 2020, registrationKey = "BLS_KEY") %>% #software employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Software Publishers") %>%
  mutate(value = (value-value[nrow(.)]))

CA_SEARCH_PORTALS_IND <- bls_api("SMU06000005051900001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Web Search Portals, Libraries, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

CA_MEDIA_SOCIAL_IND <- bls_api("SMU06000005051620001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Streaming Services, Social Networks, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

CA_COMPUTER_SYSTEM_DESIGN_IND <- bls_api("SMU06000006054150001", startyear = 2020, registrationKey = "BLS_KEY") %>% #internet employment
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value, seriesID) %>%
  mutate(series_id = "Computer Systems Design, Custom Programming, & Related") %>%
  mutate(value = (value-value[nrow(.)]))

CA_TECH_EMPLOY_GROWTH_IND <- rbind(CA_DATA_PROCESSING_IND,CA_MEDIA_SOCIAL_IND,CA_SEARCH_PORTALS_IND,CA_SOFTWARE_PUBLISHERS_IND,CA_COMPUTER_SYSTEM_DESIGN_IND) %>%
  group_by(date) %>%
  filter(n() > 4) %>%
  mutate(series_id = factor(series_id,levels = rev(c("Software Publishers","Computing Infrastructure, Data Processing, Web Hosting, & Related","Computer Systems Design, Custom Programming, & Related","Web Search Portals, Libraries, & Related","Streaming Services, Social Networks, & Related"))))

CA_TECH_EMPLOY_GROWTH_IND_graph <- ggplot(data = CA_TECH_EMPLOY_GROWTH_IND, aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Change Since Jan 2020, Thousands of Jobs, NSA") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-50,-25,0,25,50,75,100), limits = c(-30,100), expand = c(0,0)) +
  ggtitle("California Tech Sector Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "CA Tech Jobs Have Stagnated, With Search, Streaming, and Computer Systems Design Dropping") +
  theme_apricitas + theme(legend.position = c(0.35,0.83), legend.key.size = unit(0.5,"cm"), legend.spacing.y = unit(0, "cm")) +
  scale_fill_manual(name= "Change in Employment since Jan 2020",values = c("#FFE98F","#EE6055","#00A99D","#9A348E","#3083DC","#6A4C93","#A7ACD9"), breaks = c("Software Publishers","Computer Systems Design, Custom Programming, & Related","Computing Infrastructure, Data Processing, Web Hosting, & Related","Web Search Portals, Libraries, & Related","Streaming Services, Social Networks, & Related")) +
  theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*(today()-as.Date("2020-01-01"))), xmax = as.Date("2020-01-01")-(0.049*(today()-as.Date("2020-01-01"))), ymin = -30-(.3*130), ymax = -30) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CA_TECH_EMPLOY_GROWTH_IND_graph, "CA Tech Employ Growth IND.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

Census_API_List <- listCensusApis()

metadata <- listCensusMetadata("timeseries/eits/qtax")

QTAX_Data <- getCensus(
  name = "timeseries/eits/qtax",
  #region = "CA",
  vars = c("CELL_VALUE","CATEGORY_CODE","SEASONALLY_ADJ","DATA_TYPE_CODE"),
  time = paste("from 2016 to", format(Sys.Date(), "%Y")),
  region = "state:06",
  DATA_TYPE_CODE = "T40",
  DATA_TYPE_CODE = "T41"
) %>%
  transmute(name = DATA_TYPE_CODE, date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(CELL_VALUE)) %>%
  pivot_wider() %>%
  setNames(c("date","indiv","corp")) %>%
  arrange(date) %>%
  mutate(roll_indiv = c(0,0,0,rollmean(indiv,4)), roll_corp = c(0,0,0,rollmean(corp,k = 4)))

CA_TAX_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data= filter(QTAX_Data,date>=as.Date("2017-01-01")), aes(x=date,y= indiv*4/1000, color= "Individual Income Taxes (incl. Capital Gains)"), size = 0.75, linetype = "dashed") +
  geom_line(data= filter(QTAX_Data,date>=as.Date("2017-01-01")), aes(x=date,y= roll_indiv*4/1000, color= "Individual Income Taxes (incl. Capital Gains)"), size = 1.25) +
  geom_line(data= filter(QTAX_Data,date>=as.Date("2017-01-01")), aes(x=date,y= corp*4/1000, color= "Corporate Net Income Taxes"), size = 0.75, linetype = "dashed") +
  geom_line(data= filter(QTAX_Data,date>=as.Date("2017-01-01")), aes(x=date,y= roll_corp*4/1000, color= "Corporate Net Income Taxes"), size = 1.25) +
  xlab("Date") +
  ylab("Dollars, Billions") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), limits = c(0,200), breaks = c(0,25,50,75,100,125,150,175,200), expand = c(0,0)) +
  ggtitle("California State Tax Receipts") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "CA State Tax Revenue Has Contracted Since the Tech-cession of 2022") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Dashed = Quarterly, Annualized Solid = 4Q Rolling Average",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F")),breaks = c("Individual Income Taxes (incl. Capital Gains)","Corporate Net Income Taxes")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*200), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CA_TAX_Graph, "CA Tax Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

metadata <- listCensusMetadata("timeseries/eits/qfr")

QFR_Data <- getCensus(
  name = "timeseries/eits/qfr",
  #region = "CA",
  vars = c("cell_value","category_code","data_type_code","seasonally_adj", "time_slot_id"),
  time = paste("from 2018 to", format(Sys.Date(), "%Y")),
  data_type_code = 219, #Net Property, Plant, and Equipment
  #category_code = 511, #Publishing ex Internet
  #category_code = 519 #Other Information
) %>%
  filter(category_code %in% c(511,519)) %>%
  transmute(name = DATA_TYPE_CODE, date = as.Date(as.yearqtr(time, "%Y-Q%q")), value = as.numeric(CELL_VALUE)) %>%
  pivot_wider() %>%
  setNames(c("date","indiv","corp")) %>%
  arrange(date) %>%
  mutate(roll_indiv = c(0,0,0,rollmean(indiv,4)), roll_corp = c(0,0,0,rollmean(corp,k = 4)))


MOVIES_CA <- fredr("SMU06000005051200001SA", observation_start = as.Date("2010-01-01"))

MOVIES_US <- fredr("CES5051200001", observation_start = as.Date("2010-01-01")) 

MOVIES_US_EX_CA <- merge(MOVIES_CA,MOVIES_US, by = "date") %>%
  transmute(date, value = value.y-value.x)

MOVIES_US_EX_CA_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= MOVIES_CA, aes(x=date,y= value, color= "California"), size = 1.25) +
  geom_line(data= MOVIES_US_EX_CA, aes(x=date,y= value, color= "US Ex-California"), size = 1.25) +
  xlab("Date") +
  ylab("Employment, Thousands") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,350), breaks = c(0,100,200,300), expand = c(0,0)) +
  ggtitle("California Movie Industry Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Hollywood Has Lost Jobs As Movie Production Employment Elsewhere in the US Rises") +
  theme_apricitas + theme(legend.position = c(.3,.89)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "All Employees, Motion Picture and Sound Recording",values = rev(c("#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#EE6055","#00A99D","#FFE98F"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 0-(.3*350), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MOVIES_US_EX_CA_graph, "Movies US ex CA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
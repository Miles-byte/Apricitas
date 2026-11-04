pacman::p_load(bea.R,janitor,sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


PCE_UNDERLYING_DETAIL_NOMINAL_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20405',
  'Frequency' = 'A',
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_UNDERLYING_DETAIL_NOMINAL <- beaGet(PCE_UNDERLYING_DETAIL_NOMINAL_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1960-01-01"), length.out = nrow(.), by = "1 year"))) %>%
  clean_names()


PCE_AUDIO <- PCE_UNDERLYING_DETAIL_NOMINAL %>%
  select(date,u20405_dpcerc_1_personal_consumption_expenditures_current_dollars_level_6,u20405_drtdrc_45_audio_discs_tapes_vinyl_and_permanent_digital_downloads_current_dollars_level_6,u20405_dligrc_215_live_entertainment_excluding_sports_current_dollars_level_6,u20405_la000232_225_audio_streaming_and_radio_services_including_satellite_radio_current_dollars_level_6) %>%
  setNames(c("date","PCE","Audio Discs, Tapes, Vinyl, & Permanent Downloads","Tickets to Live Entertainment","Audio Streaming")) %>%
  transmute(date, mutate(across(where(is.numeric) & !all_of("PCE"), ~ . / PCE))) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Audio Discs, Tapes, Vinyl, & Permanent Downloads","Audio Streaming","Tickets to Live Entertainment"))))


PCE_AUDIO_GRAPH <- ggplot() + #plotting components of manufacturing construction
  geom_bar(data = filter(PCE_AUDIO), aes(x = date, y = value, fill = name), color = NA, width = 370, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),limits = c(0,0.0055), breaks = c(0,0.001,0.002,0.003,0.004,0.005), expand = c(0,0)) +
  ylab("% of Consumer Spending") +
  ggtitle("Music, % of US Consumer Spending") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Direct Music Sales Cratered After the Year 2000, and the Industry Was Only Kept Alive by Concerts") +
  theme_apricitas + theme(legend.position = c(0.32,0.91), legend.key.size = unit(0.5,"cm")) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*(today()-as.Date("1960-01-01"))), xmax = as.Date("1960-01-01")-(0.049*(today()-as.Date("1960-01-01"))), ymin = 0-(.3*0.0055), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_AUDIO_GRAPH, "PCE Audio Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


PCE_VIDEO <- PCE_UNDERLYING_DETAIL_NOMINAL %>%
  select(date,u20405_dpcerc_1_personal_consumption_expenditures_current_dollars_level_6,u20405_dmovrc_214_motion_picture_theaters_current_dollars_level_6,u20405_dctvrc_219_cable_satellite_and_other_live_television_services_current_dollars_level_6,u20405_la000233_224_video_streaming_and_rental_current_dollars_level_6,u20405_doverc_46_video_discs_tapes_and_permanent_digital_downloads_current_dollars_level_6) %>%
  setNames(c("date","PCE","Movie Theater Tickets","Cable, Satellite, & Other Live TV","Video Streaming & Rental","Video Discs, Tapes, & Permanent Downloads")) %>%
  transmute(date, mutate(across(where(is.numeric) & !all_of("PCE"), ~ . / PCE))) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Movie Theater Tickets","Cable, Satellite, & Other Live TV","Video Streaming & Rental","Video Discs, Tapes, & Permanent Downloads"))))


PCE_VIDEO_GRAPH <- ggplot() + #plotting components of manufacturing construction
  geom_bar(data = filter(PCE_VIDEO), aes(x = date, y = value, fill = name), color = NA, width = 370, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),limits = c(0,0.012), breaks = c(0,0.002,0.004,0.006,0.008,0.01,0.012), expand = c(0,0)) +
  ylab("% of Consumer Spending") +
  ggtitle("Video, % of US Consumer Spending") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Video Sales Fell After 2016, With Streaming Unable to Compensate for Cable & Movie Losses") +
  theme_apricitas + theme(legend.position = c(0.32,0.89), legend.key.size = unit(0.5,"cm")) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*(today()-as.Date("1960-01-01"))), xmax = as.Date("1960-01-01")-(0.049*(today()-as.Date("1960-01-01"))), ymin = 0-(.3*0.012), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PCE_VIDEO_GRAPH, "PCE Video Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



NEWS_IND <- bls_api("CES5051311001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051311001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Newspapers") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

PERIODICALS_IND <- bls_api("CES5051312001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051312001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Magazines & Periodicals") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

BOOKS_IND <- bls_api("CES5051313001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051313001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Books") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

TOTAL_PUBLISHING_RBIND <- rbind(NEWS_IND,PERIODICALS_IND,BOOKS_IND) %>%
  transmute(date, value, name = factor(series_id, levels = rev(c("Newspapers","Magazines & Periodicals","Books"))))

TOTAL_PUBLISHING_GRAPH <- ggplot() + #plotting components of manufacturing construction
  geom_bar(data = filter(TOTAL_PUBLISHING_RBIND), aes(x = date, y = value, fill = name), color = NA, width = 33, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"),limits = c(0,750), breaks = c(0,200,400,600), expand = c(0,0)) +
  ylab("All Employees") +
  ggtitle("All Employees, Written Media") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Media Employment Has Cratered in the Digital Age") +
  theme_apricitas + theme(legend.position = c(0.75,0.89)) +
  scale_fill_manual(name= NULL,values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#3083DC","#A7ACD9","#6A4C93","#FF8E72")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*(today()-as.Date("1990-01-01"))), xmax = as.Date("1990-01-01")-(0.049*(today()-as.Date("1990-01-01"))), ymin = 0-(.3*750), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TOTAL_PUBLISHING_GRAPH, "Total Publishing Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing









MOVIE_YOY <- bls_api("CEU5051200001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU5051200001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Motion Picture & Sound Recording") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

PUBLISH_YOY <- bls_api("CEU5051310001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU5051310001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Newspapers, Magazine, & Book Publishing") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

RADIO_TV_YOY <- bls_api("CEU5051610001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU5051610001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Radio & TV Broadcasting") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

MEDIA_STREAMING_YOY <- bls_api("CEU5051620001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU5051620001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Streaming & Social Media") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

GRAPHIC_DESIGN_YOY <- bls_api("CEU6054143001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU6054143001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Graphic Design") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

PERFORMING_ARTS_YOY <- bls_api("CEU7071110001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU7071110001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Performing Arts") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

INDEPENDENT_ARTISTS_YOY <- bls_api("CEU7071150001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU7071150001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Employees of Independent Artists") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

MUSEUMS_YOY <- bls_api("CEU7071211001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CEU7071211001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Museums") %>%
  arrange(date) %>%
  mutate(value = (value-lag(value,12))) %>%
  drop_na()

TOTAL_ARTS_RBIND <- rbind(MOVIE_YOY,PUBLISH_YOY,RADIO_TV_YOY,MEDIA_STREAMING_YOY,GRAPHIC_DESIGN_YOY,PERFORMING_ARTS_YOY,INDEPENDENT_ARTISTS_YOY,MUSEUMS_YOY) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Motion Picture & Sound Recording","Newspapers, Magazine, & Book Publishing","Radio & TV Broadcasting","Streaming & Social Media","Graphic Design","Performing Arts","Employees of Independent Artists","Museums")))) %>%
  group_by(date) %>%
  filter(n() == 8) %>%
  ungroup()


TOTAL_ARTS_YOY <- TOTAL_ARTS_RBIND %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(series_id = "test")


TOTAL_ARTS_YOY_graph <- ggplot(data = filter(TOTAL_ARTS_RBIND, date >= as.Date("2022-08-01")), aes(x = date, y = value, fill = series_id)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(TOTAL_ARTS_YOY, date >= as.Date("2022-08-01")), aes(x=date, y = value, color = "Total Creative Class Employment Growth"), size = 2) +
  xlab("Date") +
  ylab("Year-on-Year Change, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-100,-50,0,50,100,150,200,400,600,800), limits = c(-110,150), expand = c(0,0)) +
  ggtitle("Year-on-Year Change in US Creative Class Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "American Blue Collar Employmentis Falling, With Losses in Newspapers, Books, TV, and Movies") +
  theme_apricitas + theme(legend.position = c(.475,.78)) + theme(plot.title = element_text(size = 23), legend.margin=margin(0,0,-5,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.4, "cm"),legend.key.height = unit(0, "cm"), legend.text = element_text(size = 13)) +
  #scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72", breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#3083DC","#A7ACD9","#6A4C93","#9A348E","#D28E20","#FF8E72","#EE6055"), breaks = c("Motion Picture & Sound Recording","Newspapers, Magazine, & Book Publishing","Radio & TV Broadcasting","Streaming & Social Media","Graphic Design","Performing Arts","Employees of Independent Artists","Museums")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-08-01")-(.1861*(today()-as.Date("2022-08-01"))), xmax = as.Date("2022-08-01")-(0.049*(today()-as.Date("2022-08-01"))), ymin = -110-(.3*260), ymax = -110) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TOTAL_ARTS_YOY_graph, "Total Arts Yoy Growth Yoy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")










MOVIE_IND <- bls_api("CES5051200001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051200001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Motion Picture & Sound Recording") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

PUBLISH_IND <- bls_api("CES5051310001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051310001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Newspapers, Magazine, & Book Publishing") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

RADIO_TV_IND <- bls_api("CES5051610001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051610001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Radio & TV Broadcasting") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

MEDIA_STREAMING_IND <- bls_api("CES5051620001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051620001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Streaming & Social Media") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

GRAPHIC_DESIGN_IND <- bls_api("CES6054143001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES6054143001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Graphic Design") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

PERFORMING_ARTS_IND <- bls_api("CES7071110001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES7071110001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Performing Arts") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

INDEPENDENT_ARTISTS_IND <- bls_api("CES7071150001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES7071150001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Employees of Independent Artists") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()

MUSEUMS_IND <- bls_api("CES7071211001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES7071211001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Museums") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()


TOTAL_ARTS_IND_RBIND <- rbind(MOVIE_IND,PUBLISH_IND,RADIO_TV_IND,MEDIA_STREAMING_IND,GRAPHIC_DESIGN_IND,PERFORMING_ARTS_IND,INDEPENDENT_ARTISTS_IND,MUSEUMS_IND) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Motion Picture & Sound Recording","Newspapers, Magazine, & Book Publishing","Radio & TV Broadcasting","Streaming & Social Media","Graphic Design","Performing Arts","Employees of Independent Artists","Museums")))) %>%
  group_by(date) %>%
  filter(n() == 8)

TOTAL_ARTS_IND <- TOTAL_ARTS_IND_RBIND %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(series_id = "test")



TOTAL_ARTS_IND_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(TOTAL_ARTS_IND, date >= as.Date("2010-01-01")), aes(x=date, y = value/1000, color = "US Total Arts Employment\n(Movies, Newspapers, Magazines, Books, Radio, TV,\nStreaming, Social Media, Graphic Design, Performing Arts,\nMuseums, & Employees of Independent Artists)"), size = 1.25) +
  xlab("Date") +
  ylab("All Employees, Millions") +
  scale_y_continuous(labels = scales::number_format(accuracy = .1, suffix = "M"), breaks = c(1,1.25,1.5,1.75), limits = c(1,1.75), expand = c(0,0)) +
  ggtitle("US Arts Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "US Arts Employment is Down More Than 200k From Their Mid-2022 Peaks") +
  theme_apricitas + theme(legend.position = c(.4,.875)) +
  #scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72", breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))) +
  scale_color_manual(name = NULL, values = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 1-(.3*0.8), ymax = 1) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TOTAL_ARTS_IND_graph, "Total Arts Ind Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


MOTION_PICTURE_VIDEO_IND <- bls_api("CES5051211001", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% #data processing employment
  select(-any_of("latest")) %>%
  rbind(bls_api("CES5051211001", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(date, value) %>%
  mutate(series_id = "Motion Picture and Video Employment") %>%
  arrange(date) %>%
  #mutate(value = (value-lag(value,12))) %>%
  drop_na()


MOTION_PICTURE_VIDEO_IND_graph <- ggplot() + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(MOTION_PICTURE_VIDEO_IND, date >= as.Date("2010-01-01")), aes(x=date, y = value, color = "All Employees: Motion Picture & Video Production"), size = 1.25) +
  xlab("Date") +
  ylab("All Employees, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(100,200,300), limits = c(100,300), expand = c(0,0)) +
  ggtitle("US Movie & TV Production Employment") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Hollywood Employment Has Cratered Over the Last Three Years, Down More than 30%") +
  theme_apricitas + theme(legend.position = c(.35,.875)) +
  #scale_fill_manual(name= "Residential Construction Employment, Change Since Jan 2020",values = c("#FFE98F","#6A4C93","#00A99D","#A7ACD9","#9A348E","#3083DC","#D28E20","#FF8E72", breaks = c("Metals & Nonmetallic Minerals","Transportation Equipment (Cars/Planes/Etc)","Food","Chemicals, Petroleum, & Plastic","Computer, Electronics, & Electrical","Machinery","Wood, Paper, & Printing","Other"))) +
  scale_color_manual(name = NULL, values = "#FFE98F") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2010-01-01")-(.1861*(today()-as.Date("2010-01-01"))), xmax = as.Date("2010-01-01")-(0.049*(today()-as.Date("2010-01-01"))), ymin = 100-(.3*200), ymax = 100) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MOTION_PICTURE_VIDEO_IND_graph, "Motion Picture Arts Ind Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ORL_RBIND <- rbind(MOVIE_IND,PUBLISH_IND,RADIO_TV_IND,MEDIA_STREAMING_IND,GRAPHIC_DESIGN_IND) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Motion Picture & Sound Recording","Newspapers, Magazine, & Book Publishing","Radio & TV Broadcasting","Streaming & Social Media","Graphic Design","Performing Arts","Employees of Independent Artists","Museums")))) %>%
  group_by(date) %>%
  filter(n() == 5) %>%
  group_by(series_id) %>%
  filter(date >= as.Date("2023-01-01")) %>%
  mutate(value = value/value[1]-1) %>%
  mutate(category = "Digital Arts")


IRL_RBIND <- rbind(PERFORMING_ARTS_IND,INDEPENDENT_ARTISTS_IND,MUSEUMS_IND) %>%
  mutate(series_id = factor(series_id, levels = rev(c("Performing Arts","Employees of Independent Artists","Museums")))) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  group_by(series_id) %>%
  filter(date >= as.Date("2023-01-01")) %>%
  mutate(value = value/value[1]-1) %>%
  mutate(category = "Live Arts")

IRL_ORL_RBIND <- rbind(ORL_RBIND,IRL_RBIND)

pacman::p_load(ggnewscale)


IRL_ORL_AGG_GRAPH <- ggplot() +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = IRL_ORL_RBIND, aes(x = date, y = value, color = series_id), size = 1.25) + 
  facet_wrap(~ category) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.75,1.25), expand = c(0,0)) +
  ylab("Percent Growth Since ChatGPT Launch 2023") +
  ggtitle("Cumulative Job Growth Since Jan 2023, %") +
  labs(caption = "Graph created by @JosephPolitano", subtitle = "The 'Mountain Lions' of Utah, Nevada, & Idaho Have Led the US in Job Growth, Especially in Construction") +
  theme_apricitas + theme(legend.position = c(.15,.67), plot.title = element_text(size = 27)) + 
  theme(strip.text = element_text(size = 17, color = "white", face = "bold")) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055")) +
  new_scale_color()

scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#A7ACD9","#6A4C93","#9A348E","#D28E20","#FF8E72")) +
  theme(plot.title.position = "panel")

ggsave(dpi = "retina", plot = LION_EMP_AGG_GRAPH, "LION_EMP_AGG_GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



IRL_ORL_AGG_GRAPH <- ggplot() +
  theme_apricitas +
  #annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0, color = "white", linewidth = .5) +
  # first group
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.25,.25), expand = c(0,0)) +
  ylab("Percent Growth Since Jan 2023") +
  ggtitle("Cumulative Job Growth Since ChatGPT Launch") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = filter(IRL_ORL_RBIND, category == "Live Arts"),
            aes(x = date, y = value, color = series_id), linewidth = 1.25) +
  scale_color_manual(name = "Live Arts",
                     values = c("#FFE98F","#00A99D","#EE6055")) +
  new_scale_color() +
  # second group
  geom_line(data = filter(IRL_ORL_RBIND, category == "Digital Arts"),
            aes(x = date, y = value, color = series_id), linewidth = 1.25) +
  scale_color_manual(name = NULL,
                     values = c("#3083DC","#A7ACD9","#6A4C93","#9A348E","#D28E20","#FF8E72")) +
  facet_wrap(~ category) +
  theme_apricitas +  theme(plot.title = element_text(size = 27)) + 
  theme(strip.text = element_text(size = 17, color = "white", face = "bold")) +
  theme(legend.position = c(.27,.78), legend.key.size = unit(0.4,"cm"), legend.text = element_text(size = 14), legend.spacing.y = unit(-0.4,"cm")) +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina", plot = IRL_ORL_AGG_GRAPH, "IRL ORL Agg Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PCE_UNDERLYING_DETAIL_QUANTITY_SPECS <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIUnderlyingDetail',
  'TableName' = 'U20403',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 1960, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

PCE_UNDERLYING_DETAIL_QUANTITY <- beaGet(PCE_UNDERLYING_DETAIL_QUANTITY_SPECS, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("1960-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

PCE_UNDERLYING_DETAIL_QUANTITY$u20403_drbkra_60_recreational_books_part_of_90_fisher_quantity_index_level_0


US_REAL_BOOK_SALES_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=filter(PCE_UNDERLYING_DETAIL_QUANTITY, date>= as.Date("1995-01-01")), aes(x=date,y= u20403_drbkra_60_recreational_books_part_of_90_fisher_quantity_index_level_0*.22280,color= "Real Sales, Recreational Books"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Billions of 2017 Dollars") +
  ggtitle("US Book Sales Have Rebounded to Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "Book Sales Were Declining in the 2010s, But Surprisingly Rebounded Amidst COVID & TikTok") +
  theme_apricitas + theme(legend.position = c(.5,.80), plot.title = element_text(size = 25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-01-01")-(.1861*(today()-as.Date("1995-01-01"))), xmax = as.Date("1995-01-01")-(0.049*(today()-as.Date("1995-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = US_REAL_BOOK_SALES_Graph, "US Real Book Sales Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

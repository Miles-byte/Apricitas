UNION_LVL_AGG <- bls_api("LUU0203161800", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0203161800", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0203161800", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

UNION_LVL_PRI <- bls_api("LUU0203182000", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0203182000", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0203182000", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

UNION_LVL_GOV <- bls_api("LUU0204922600", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204922600", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204922600", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))


EMPMT_LVL_AGG <- bls_api("LUU0204466800", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204466800", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204466800", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

EMPMT_LVL_PRI <- bls_api("LUU0204466700", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204466700", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204466700", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

EMPMT_LVL_GOV <- bls_api("LUU0202851800", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0202851800", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0202851800", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))



UNION_RAT_AGG <- merge(UNION_LVL_AGG,EMPMT_LVL_AGG, by = "date") %>%
  transmute(date, value = value.x/value.y)

UNION_RAT_PRI <- merge(UNION_LVL_PRI,EMPMT_LVL_PRI, by = "date") %>%
  transmute(date, value = value.x/value.y)

UNION_RAT_GOV <- merge(UNION_LVL_GOV,EMPMT_LVL_GOV, by = "date") %>%
  transmute(date, value = value.x/value.y)


UNION_RATE_GRAPH <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNION_RAT_AGG, aes(x=date,y= value,color= "Total Unionization Rate"), size = 1.25) +
  geom_line(data=UNION_RAT_PRI, aes(x=date,y= value,color= "Private-Sector Unionization Rate"), size = 1.25) +
  #geom_line(data=UNION_RAT_GOV, aes(x=date,y= value,color= "Public-Sector Unionization Rate"), size = 1.25) +
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.21), breaks = c(0,0.05,0.1,0.15,0.2), expand = c(0,0)) +
  ggtitle("US Unionization Rates") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "America's Unionization Rate Continues Falling") +
  theme_apricitas + theme(legend.position = c(.45,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"),breaks = c("Total Unionization Rate","Private-Sector Unionization Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1983-01-01")-(.1861*(today()-as.Date("1983-01-01"))), xmax = as.Date("1983-01-01")-(0.049*(today()-as.Date("1983-01-01"))), ymin = 0-(.3*.21), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNION_RATE_GRAPH, "UNION RATE GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



UNION_LVL_FED <- bls_api("LUU0204923100", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204923100", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204923100", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

UNION_LVL_STA <- bls_api("LUU0204923600", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204923600", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204923600", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

UNION_LVL_LOC <- bls_api("LUU0204924100", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204924100", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204924100", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))


EMPMT_LVL_FED <- bls_api("LUU0204923000", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204923000", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204923000", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

EMPMT_LVL_STA <- bls_api("LUU0204923500", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204923500", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204923500", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))

EMPMT_LVL_LOC <- bls_api("LUU0204924000", startyear = 1983, registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(bls_api("LUU0204924000", startyear = 2003, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  rbind(bls_api("LUU0204924000", startyear = 2023, registrationKey = Sys.getenv("BLS_KEY")) %>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))


UNION_RAT_FED <- merge(UNION_LVL_FED,EMPMT_LVL_FED, by = "date") %>%
  transmute(date, value = value.x/value.y)

UNION_RAT_STA <- merge(UNION_LVL_STA,EMPMT_LVL_STA, by = "date") %>%
  transmute(date, value = value.x/value.y)

UNION_RAT_LOC <- merge(UNION_LVL_LOC,EMPMT_LVL_LOC, by = "date") %>%
  transmute(date, value = value.x/value.y)


UNION_RATE_GOV_GRAPH <- ggplot() + #plotting u1 unemployment rate
  geom_line(data=UNION_RAT_GOV, aes(x=date,y= value,color= "Total Government Unionization Rate"), size = 2.25) +
  geom_line(data=UNION_RAT_FED, aes(x=date,y= value,color= "Federal Government Unionization Rate"), size = 1.25) +
  geom_line(data=UNION_RAT_STA, aes(x=date,y= value,color= "State Government Unionization Rate"), size = 1.25) +
  geom_line(data=UNION_RAT_LOC, aes(x=date,y= value,color= "Local Government Unionization Rate"), size = 1.25) +
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.51), breaks = c(0,0.1,0.2,0.3,0.4,0.5), expand = c(0,0)) +
  ggtitle("US Public-Sector Unionization Rates") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "America's Unionization Rate Continues Falling") +
  theme_apricitas + theme(legend.position = c(.35,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"),breaks = c("Total Government Unionization Rate","Local Government Unionization Rate","State Government Unionization Rate","Federal Government Unionization Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1983-01-01")-(.1861*(today()-as.Date("1983-01-01"))), xmax = as.Date("1983-01-01")-(0.049*(today()-as.Date("1983-01-01"))), ymin = 0-(.3*.51), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = UNION_RATE_GOV_GRAPH, "UNION RATE GOV GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

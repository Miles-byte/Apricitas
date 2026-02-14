PPIPCT <- bls_api("WPUFD4", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data
PPILFEPCT <- bls_api("WPUFD49104", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12)) %>%
  subset(date >= as.Date("2019-01-01"))

PPILFETPCT <- bls_api("WPUFD49116", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12)) %>%
  subset(date >= as.Date("2019-01-01"))


PPIPCT_MOM <- bls_api("WPSFD4", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data
PPILFEPCT_MOM <- bls_api("WPSFD49104", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1)) %>%
  subset(date >= as.Date("2019-01-01"))
PPILFETPCT_MOM <- bls_api("WPSFD49116", startyear = 2017, endyear = 2025, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,1))/lead(value,1)) %>%
  subset(date >= as.Date("2019-01-01"))



sprintf(
  "NEW: PPI Inflation rose to %s%% year-on-year, increasing %s%% month-on-month\n\nCore CPI inflation rose %s%% year-on-year, growing %s%% month-on-month",
  formatC(PPIPCT %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1),
  formatC(PPIPCT_MOM %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1),
  formatC(PPILFEPCT %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1),
  formatC(PPILFEPCT_MOM %>% arrange(desc(date)) %>% slice(1) %>% pull(value)*100, format = "f", digits = 1)
)

PPIPCT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=PPILFETPCT, aes(x=date,y= value,color= "Producer Price Index Less Food, Energy, & Retailer/Wholesaler Margins"), size = 1.25) +
  geom_line(data=PPILFEPCT, aes(x=date,y= value,color= "Producer Price Index Less Food & Energy"), size = 1.25) +
  geom_line(data=PPIPCT, aes(x=date,y= value,color= "Producer Price Index"), size = 1.25) +
  # annotate("vline", x= as.Date("2022-08-01"), xintercept= as.Date("2022-08-01"), color = "white", size = 1.25, linetype = "dashed") +
  # annotate("text",label = "Inflation Reduction Act Signed", x= as.Date("2021-09-01"), y = 0.0075, color = "white", size = 5.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.02,0.15), breaks = c(0,0.03,0.06,0.09,0.12,0.15), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("The Inflation Situation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Producer Price Index","Producer Price Index Less Food & Energy","Producer Price Index Less Food, Energy, & Retailer/Wholesaler Margins")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.02-(.3*0.17), ymax = -0.02) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PPIPCT_Graph, "PPI PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

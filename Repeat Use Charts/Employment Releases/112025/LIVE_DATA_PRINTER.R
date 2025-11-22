pacman::p_load(ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)


Payrolls_latest_data <- bls_api("CES0000000001", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest) %>%
  arrange(date) %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(diff(value),2))

Payrolls_formatted_change <- ifelse(Payrolls_latest_data$change_from_prior_month >= 0,
                                paste0("+", Payrolls_latest_data$change_from_prior_month),
                                Payrolls_latest_data$change_from_prior_month)

UNRATE_latest_data <- bls_api("LNS14000000", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest) %>%
  arrange(date) %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(diff(value),2))

UNRATE_formatted_change <- ifelse(UNRATE_latest_data$change_from_prior_month >= 0,
                                    paste0("+", UNRATE_latest_data$change_from_prior_month),
                                    UNRATE_latest_data$change_from_prior_month)

output_string <- paste0("NEW JOBS DATA:\n\nNon-farm Payrolls: ", Payrolls_formatted_change, "k")
output_string <- paste0(output_string, "\nUnemployment Rate: ", UNRATE_latest_data$latest_value,"%", " (",UNRATE_formatted_change,"%)")

EPOP_1990 <- bls_api("LNS12300060", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_2010 <- bls_api("LNS12300060", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPop <- rbind(EPOP_1990,EPOP_2010) %>%
  arrange(date)

EPop_latest_data <- EPop %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(diff(value),2))

# Find the date of the closest prior value that's higher
EPop_date_of_higher_value <- if (EPop_latest_data$change_from_prior_month > 0) {
  EPop %>%
    filter(date < EPop_latest_data$latest_date, value >= EPop_latest_data$latest_value) %>%
    summarise(date_of_higher_value = max(date)) %>%
    pull() %>%
    as.Date(origin = "1970-01-01") %>%
    format("%B %Y")
} else {
  NA
}

EPop_formatted_change <- ifelse(EPop_latest_data$change_from_prior_month >= 0,
                           paste0("+", EPop_latest_data$change_from_prior_month),
                           EPop_latest_data$change_from_prior_month)


# Concatenate the string
output_string <- paste0(output_string,"\nPrime Age (25-54) Employment-Population Ratio: ", EPop_latest_data$latest_value,
                        "% (", EPop_formatted_change,"%)")

# Add the "Highest since" section if change is positive
if (!is.na(EPop_date_of_higher_value)) {
  output_string <- paste0(output_string, ", Highest since ", date_of_higher_value,"!!)")
}

AHE_latest_data <- bls_api("CES0500000003", startyear = 2020, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest) %>%
  arrange(date) %>%
  tail(2) %>%
  summarise(latest_date = tail(date, 1),
            latest_value = tail(value, 1),
            change_from_prior_month = round(100 * diff(value) / head(value, 1), 1))

AHE_formatted_change <- ifelse(AHE_latest_data$change_from_prior_month >= 0,
                                  paste0("+", AHE_latest_data$change_from_prior_month),
                                  AHE_latest_data$change_from_prior_month)


output_string <- paste0(output_string, "\nAverage Hourly Earnings: ",AHE_formatted_change,"%")


EPOP_WOM_1990 <- bls_api("LNS12300062", startyear = 1990, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
EPOP_WOM_2010 <- bls_api("LNS12300062", startyear = 2010, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  select(-latest)

EPOP_WOM <- rbind(EPOP_WOM_1990,EPOP_WOM_2010) %>%
  arrange(desc(value)) %>%
  rev()

cat(paste0("DATE VERIFICATION:\n",Payrolls_latest_data$latest_date,"\n",UNRATE_latest_data$latest_date,"\n",EPop_latest_data$latest_date,"\n",AHE_latest_data$latest_date))
cat(paste0("Highest Women's EPOP: ",EPOP_WOM$date[1]))
# Print the result
cat(output_string)



PAYEMS <- bls_api("CES0000000001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  transmute(date, PAYEMS = value-lag(value,1))

UNRATE <- bls_api("LNS14000000", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, UNRATE = value)

EPOP <-  bls_api("LNS12300060", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, EPOP = value)

AHE <-  bls_api("CES0500000003", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, AHE = (value-lag(value,1))/lag(value,1))

AHE <- fredr(
  series_id = "CES0500000003",
  observation_start = as.Date("2018-01-01")
) %>%
  transmute(date, AHE = (value-lag(value,1))/lag(value,1))

PAYEMS_Graph <- ggplot(filter(PAYEMS, date >= as.Date("2021-01-01")), aes(x = date, y = PAYEMS)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_bar(stat = "identity", fill = "#FFE98F", color = NA, show.legend = FALSE) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::comma_format(suffix = "k"),limits = c(-50,1000), breaks = c(0,250,500,750,1000), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Nonfarm Payrolls Growth, Monthly") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm")) #reducing plot margins makes the ggarrange look better

UNRATE_Graph <- ggplot() +
  geom_line(data= filter(UNRATE, date >= as.Date("2021-01-01")), aes(x=date,y= UNRATE/100, color = "layoffs"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.07), breaks = c(0,.01,.02,.03,.04,.05,0.06,0.07), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Unemployment Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

EPOP_Graph <- ggplot() +
  geom_line(data= filter(EPOP, date >= as.Date("2021-01-01")), aes(x=date,y= EPOP/100, color = "EPOP"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.76,0.82), breaks = c(0.76,0.78,0.80,.82), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Prime Age (25-54) Employment Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm")) #reducing plot margins makes the ggarrange look better

AHE_Graph <- ggplot(filter(AHE, date >= as.Date("2021-01-01")), aes(x = date, y = AHE)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_bar(stat = "identity", fill = "#FFE98F", color = NA, show.legend = FALSE) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(-0.001,0.0075), breaks = c(0,0.002,0.004,0.006), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Avg. Hourly Earnings Growth, Pvt., Monthly") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 13, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm"))

#arranging the SEP graphs into one items and adding background and border colors to match the theme
JOLTS_ARRANGE_GRAPH <- ggarrange(HIRES_Graph, LAYOFFS_Graph, QUITS_Graph, JOB_OPENINGS_Graph,  ncol = 2, nrow = 2, heights = 20, widths = 10, common.legend = TRUE, legend = "none") + bgcolor("#252A32") + border("#252A32")

text <- c("Jobs  Data",fontface = "bold")

# Create a text grob
tgrob <- text_grob(expression(bold("                                  US Jobs Data")),size = 29, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-.15,-.15,-0.15,-.15),"cm"))  
blank <- ""
blankgrob <- text_grob(blank,size = 20)
plot_blank <- as_ggplot(blankgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))
JOBS_ARRANGE_TITLE_GRAPH <- ggarrange(plot_0,plot_blank,PAYEMS_Graph, UNRATE_Graph, EPOP_Graph, AHE_Graph,  ncol = 2, nrow = 3, heights = c(5,20,20), widths = 10, common.legend = TRUE, legend = "none") + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = JOBS_ARRANGE_TITLE_GRAPH, "JOBS ARRANGE GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

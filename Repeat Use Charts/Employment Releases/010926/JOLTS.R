pacman::p_load(ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

PAYEMS <- bls_api("CES0000000001", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  transmute(date, payrolls = value)

HIRES <- bls_api("JTS000000000000000HIL", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, hires = value)

LAYOFFS <- bls_api("JTS000000000000000LDL", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, layoffs = value)

QUITS <- bls_api("JTS000000000000000QUL", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, quits = value)

JOB_OPENINGS <- bls_api("JTS000000000000000JOL", startyear = 2018, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  transmute(date, openings = value)

JOLTS_DATA <- PAYEMS %>%
  full_join(HIRES, by = "date") %>%
  full_join(LAYOFFS, by = "date") %>%
  full_join(QUITS, by = "date") %>%
  full_join(JOB_OPENINGS, by = "date") %>%
  arrange(date) %>%
  tidyr::fill(payrolls, .direction = "down") %>%
  transmute(date, hiring_rate = hires/payrolls, layoff_rate = layoffs/payrolls, quit_rate = quits/payrolls, openings_rate = openings/(openings+payrolls)) %>%
  filter(date >= as.Date("2021-01-01"))



HIRES_Graph <- ggplot() +
  geom_line(data= JOLTS_DATA, aes(x=date,y= hiring_rate, color = "hires"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.03,0.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Hiring Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 18, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm")) #reducing plot margins makes the ggarrange look better

LAYOFFS_Graph <- ggplot() +
  geom_line(data= JOLTS_DATA, aes(x=date,y= layoff_rate, color = "layoffs"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.02), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Layoff Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 18, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

QUITS_Graph <- ggplot() +
  geom_line(data= JOLTS_DATA, aes(x=date,y= quit_rate, color = "quits"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.015,0.035), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Quit Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 18, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,1,0.15,0.4),"cm")) #reducing plot margins makes the ggarrange look better

JOB_OPENINGS_Graph <- ggplot() +
  geom_line(data= JOLTS_DATA, aes(x=date,y= openings_rate, color = "hires"), size = 1.25) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0.04,0.08), breaks = c(0,.02,.04,.06,.08), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Job Openings Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 18, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

#arranging the SEP graphs into one items and adding background and border colors to match the theme
JOLTS_ARRANGE_GRAPH <- ggarrange(HIRES_Graph, LAYOFFS_Graph, QUITS_Graph, JOB_OPENINGS_Graph,  ncol = 2, nrow = 2, heights = 20, widths = 10, common.legend = TRUE, legend = "none") + bgcolor("#252A32") + border("#252A32")

text <- c("JOLTS Data",fontface = "bold")

# Create a text grob
tgrob <- text_grob(expression(bold("                                  US Job Openings & Labor Turnover Data")),size = 29, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-.15,-.15,-0.15,-.15),"cm"))  
blank <- ""
blankgrob <- text_grob(blank,size = 20)
plot_blank <- as_ggplot(blankgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))
JOLTS_ARRANGE_TITLE_GRAPH <- ggarrange(plot_0,plot_blank,HIRES_Graph, LAYOFFS_Graph, QUITS_Graph, JOB_OPENINGS_Graph,  ncol = 2, nrow = 3, heights = c(5,20,20), widths = 10, common.legend = TRUE, legend = "none") + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = JOLTS_ARRANGE_TITLE_GRAPH, "JOLTS ARRANGE GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cols <- c("hiring_rate","layoff_rate","quit_rate","openings_rate")


latest_two <- JOLTS_DATA %>%
  arrange(date) %>%
  select(date, all_of(cols)) %>%
  filter(if_all(all_of(cols), ~ !is.na(.))) %>%
  slice_tail(n = 2)

cur  <- latest_two %>% slice_tail(n = 1)
prev <- latest_two %>% slice_head(n = 1)

dir_word <- function(cur, prev) {
  case_when(
    cur > prev ~ "rose to ",
    cur < prev ~ "fell to ",
    TRUE       ~ "was unchanged at "
  )
}

sentence <- paste0(
  "The hiring rate ", dir_word(cur$hiring_rate, prev$hiring_rate), scales::percent(cur$hiring_rate, accuracy = 0.1),
  ", layoffs ",      dir_word(cur$layoff_rate, prev$layoff_rate), scales::percent(cur$layoff_rate, accuracy = 0.1),
  ", quits ",        dir_word(cur$quit_rate, prev$quit_rate),     scales::percent(cur$quit_rate, accuracy = 0.1),
  ", and job openings ", dir_word(cur$openings_rate, prev$openings_rate), scales::percent(cur$openings_rate, accuracy = 0.1)
)

sentence

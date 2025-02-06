pacman::p_load(read_xl,bea.R,janitor,tidyverse,ggpubr,purrr,sf,seasonal,tigris,maps,readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)



read_excel_from_url <- function(url) {
  temp_file <- tempfile(fileext = ".xls")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  read_excel(temp_file)
}

#NEED TO CHANGE "12_24" NUMBERS FOR EACH NEW QUARTER
SEAS_QUARTERLY_GDP_BULK <- read_excel_from_url("https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_desest_12_24.xls")

SEAS_QUARTERLY_GDP <- SEAS_QUARTERLY_GDP_BULK %>%
  setNames(c("year","date","GDP","Imports","Private Consumption","Public Consumption","GFCF","Exports")) %>%
  select(-year) %>%
  drop_na() %>%
  slice(-1) %>%
  mutate(date = seq.Date(from = as.Date("2004-01-01"), by = "3 months", length.out = nrow(.))) %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))
          
SEAS_QUARTERLY_GDP_graph <- ggplot() + #plotting real battery shipments
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=filter(SEAS_QUARTERLY_GDP, date >= as.Date("2014-01-01")), aes(x=date,y= GDP/GDP[24]*100,color="Argentina Real GDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(80,110), expand = c(0,0)) +
  ylab("Real GDP, Q4 2019 = 100") +
  ggtitle("Argentine GDP Has Stagnated for Years") +
  labs(caption = "Graph created by @JosephPolitano using INDEC Data",subtitle = "Argentina's GDP Has Seen Functionally Zero Growth Since the Early 2010s") +
  theme_apricitas + theme(legend.position = c(.37,.965)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 80-(.3*30), ymax = 80) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SEAS_QUARTERLY_GDP_graph, "Seas Quarterly GDP graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


MONTHLY_ACTIVITY_INDICATOR_BULK <- read_excel_from_url("https://www.indec.gob.ar/ftp/cuadros/economia/sh_emae_mensual_base2004.xls")

MONTHLY_ACTIVITY_INDICATOR <- MONTHLY_ACTIVITY_INDICATOR_BULK %>%
  setNames(c("year","date","Original","Yoy_Growth","Seasonally_Adjusted","Mom_Growth","Trend","Mom_Growth_Trend")) %>%
  slice(-1:-3) %>%
  select(-year) %>%
  #drop_na() %>%
  mutate(date = seq.Date(from = as.Date("2004-01-01"), by = "1 months", length.out = nrow(.))) %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(date >= as.Date("2014-01-01"))

MONTHLY_ACTIVITY_INDICATOR_graph <- ggplot() + #plotting real battery shipments
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data=filter(MONTHLY_ACTIVITY_INDICATOR, date >= as.Date("2014-01-01")), aes(x=date,y= Seasonally_Adjusted/Seasonally_Adjusted[71]*100,color="Argentina Monthly Real Economic Activity Indicator"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(70,120), expand = c(0,0)) +
  ylab("Real GDP, Nov 2019 = 100") +
  ggtitle("Argentine GDP Has Stagnated for Years") +
  labs(caption = "Graph created by @JosephPolitano using INDEC Data",subtitle = "Argentina's GDP Has Seen Functionally Zero Growth Since the Early 2010s") +
  theme_apricitas + theme(legend.position = c(.37,.965)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 70-(.3*45), ymax = 70) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MONTHLY_ACTIVITY_INDICATOR_graph, "Monthly Activity Indicator graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

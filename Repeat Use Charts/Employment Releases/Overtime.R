pacman::p_load(dplyr,seasonal,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

OVERTIME_MANUFACTURING <- fredr(series_id = "CES3000000004")
OVERTIME_NONDURABLE <- fredr(series_id = "CES3200000004")
OVERTIME_FOOD <- bls_api(seriesid = "CES3231100004", startyear = 2006, endyear = 2015, registrationKey =  Sys.getenv("bd82b930b5444bd3954730ad80f639cc")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
OVERTIME_FOOD_2 <- bls_api(seriesid = "CES3231100004", startyear = 2016, endyear = 2023, registrationKey =  Sys.getenv("bd82b930b5444bd3954730ad80f639cc")) %>%
  select(-latest) %>% 
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))
OVERTIME_FOOD <- rbind(OVERTIME_FOOD,OVERTIME_FOOD_2)

Overtime <- ggplot() + #plotting Wage Growth
  geom_line(data=OVERTIME_MANUFACTURING, aes(x=date,y= value,color= "All Manufacturing"), size = 1.25) +
  geom_line(data=OVERTIME_NONDURABLE, aes(x=date,y= value,color= "Nondurable Goods Manufacturing"), size = 1.25) +
  #geom_line(data=OVERTIME_FOOD, aes(x=date,y= value,color= "Food Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.5),limits = c(2,4), expand = c(0,0)) +
  ylab("Weekly Overtime Hours") +
  ggtitle("Under-Time") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Overtime Hours at Manufacturing Firms are Declining, Especially in Nondurable/Food Manufacturing") +
  theme_apricitas + theme(legend.position = c(.52,.22)) +
  scale_color_manual(name= "Average Weekly Overtime Hours",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("All Manufacturing","Nondurable Goods Manufacturing","Food Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-03-01")-(.1861*(today()-as.Date("2006-03-01"))), xmax = as.Date("2006-03-01")-(0.049*(today()-as.Date("2006-03-01"))), ymin = 2-(.3*2), ymax = 2) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Overtime, "Overtime.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

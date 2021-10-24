pacman::p_load(plm,transformr,stringi,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Museum_Employees <- fredr(series_id = c("CES7071200001"), observation_start = as.Date("2015-01-01")) #downloading employment in museums and other cultural institutions from FRED
Prime_Age_Population <- fredr(series_id = c("LFWA25TTUSM647N"), observation_start = as.Date("2015-01-01")) #downloading prime age employment data from FRED
Museum_Prime_Age_Merge <- merge(Museum_Employees, Prime_Age_Population, by = "date")

Museum_Employees_Graph <- ggplot() + 
  geom_line(data = Museum_Prime_Age_Merge, aes(x=date, y =value.x*1000/value.y, color = "Museums, Historical Sites, and Similar Institutions Prime Age Employment-Population Ratio"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.025), limits = c(0,.002), breaks = c(0,.001,.002), expand = c(0,0)) +
  scale_x_date() +
  ylab("Prime Age Employment-Population Ratio") +
  ggtitle("That Belongs in a Museum!") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Employment at Museums and Historical Institutions has Yet to Recover") +
  theme_apricitas + theme(legend.position = c(.50,.8)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*2435), xmax = as.Date("2015-01-01")-(0.049*2435), ymin = 0-(.3*0.002), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Museum_Employees_Graph, "Museum Employees.png", type = "cairo-png")  #saving a graph of employment-population ratio in museums


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
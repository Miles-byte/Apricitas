pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

TWOYR <- fredr(series_id = "DGS2",observation_start = as.Date("2019-11-04")) #downloading 2yr yields
TWOYR <- drop_na(TWOYR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

TWOYR_Graph <- ggplot() + 
  geom_line(data = TWOYR, aes(x = date, y = value/100, color = "Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(0,.02), breaks = c(0,.005,.01,.01,.015,.02), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("The Fed's Pivot") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Yield on 2YR Treasury Notes Has Rapidly Returned to Pre-Pandemic Levels") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-11-04")-(.1861*780), xmax = as.Date("2019-11-04")-(0.049*780), ymin = 0-(.3*.02), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TWOYR_Graph, "Two Year Treasury Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
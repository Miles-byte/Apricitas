pacman::p_load(cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

T10Y2Y2018 <- fredr(series_id = "T10Y2Y",observation_start = as.Date("2018-01-01")) #downloading 2s10s
T10Y2Y <- fredr(series_id = "T10Y2Y",observation_start = as.Date("2018-01-01")) #downloading 10s2s
T10Y2Y2018 <- drop_na(T10Y2Y2018)
T10Y2Y <- drop_na(T10Y2Y)

T10Y2Y2018_Graph <- ggplot() + #yield curve
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = T10Y2Y2018, aes(x = date, y = value/100, color = "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(-0.0025,0.0175), breaks = c(0,0.005,0.01,0.015), expand = c(0,0)) +
  ylab("Yield, %") +
  ggtitle("A Flattening Yield Curve") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "The Yield Curve is Flattening as the Fed Tightens Policy") +
  theme_apricitas + theme(legend.position = c(.50,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1440), xmax = as.Date("2018-01-01")-(0.049*1440), ymin = -0.0025-(.3*0.02), ymax = -0.0025) +
  coord_cartesian(clip = "off")




ggsave(dpi = "retina",plot = T10Y2Y2018_Graph, "T10Y2Y2018.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
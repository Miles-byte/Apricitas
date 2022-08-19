pacman::p_load(eia,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

SPR_LEVEL <- eia_series("PET.WCSSTUS1.W", start = "2019")
SPR_LEVEL <- as.data.frame(SPR_LEVEL$data)

FFR_FUTURES_MEGA_MERGE_7_5 <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/FFR_FUTURES.csv") %>%
  mutate(Contract = stri_sub(Contract, 8, 14)) %>%
  mutate(Contract = gsub("'","20",Contract)) %>%
  mutate(Contract = as.Date(as.yearmon(Contract)))

SPR_LEVEL_Graph <- ggplot() + #plotting US SPR Crude Oil Stocks
  geom_line(data=SPR_LEVEL, aes(x=date,y= value/1000, color= "Stocks of Crude Oil in the Strategic Petroleum Reserve"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = " MMbbl", accuracy = 1), limits = c(0,675),breaks = c(0,150,300,450,600), expand = c(0,0)) +
  ylab("Mbbl") +
  ggtitle("Breaking the Emergency Glass") +
  labs(caption = "Graph created by @JosephPolitano using EIA data",subtitle = "The Drawdown in the Strategic Petroleum Reserve is Historicly Large") +
  theme_apricitas + theme(legend.position = c(.45,.78)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*675), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")



ggsave(dpi = "retina",plot = CPI_PCEPI_PCT_Graph, "CPI PCEPI PCT Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
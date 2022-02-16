pacman::p_load(Quandl,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

TWOYR <- fredr(series_id = "DGS2",observation_start = as.Date("2019-11-04")) #downloading 2yr yields
TWOYR <- drop_na(TWOYR)

CPIRent <- fredr(series_id = "CUSR0000SEHA",observation_start = as.Date("2019-01-01"), units = "pc1")
CPIServicesLessRent <- fredr(series_id = "CUSR0000SASL2RS",observation_start = as.Date("2019-01-01"), units = "pc1")

ECIWAG <- fredr(series_id = "ECIWAG",observation_start = as.Date("2019-01-01"), units = "pc1")

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
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-11-04")-(.1861*780), xmax = as.Date("2019-11-04")-(0.049*780), ymin = 0-(.3*.02), ymax = 0) +
  coord_cartesian(clip = "off")

Rent_LessRent_Graph <- ggplot() + 
  geom_line(data = CPIRent, aes(x = date, y = value/100, color = "CPI: Rent of Primary Residences"), size = 1.25) +
  geom_line(data = CPIServicesLessRent, aes(x = date, y = value/100, color = "CPI: Services Less Rent of Shelter"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,0.05), expand = c(0,0)) +
  ylab("Change from Year Ago, %") +
  ggtitle("The Fed's Pivot") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is Becoming More Broad Based as Prices for Rent and Other Services Jump") +
  theme_apricitas + theme(legend.position = c(.50,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1100), xmax = as.Date("2019-01-01")-(0.049*1100), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

Wage_Graph <- ggplot() + 
  geom_line(data = ECIWAG, aes(x = date, y = value/100, color = "Employment Cost Index: Wages and Salaries: Private Industry Workers"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,0.05), expand = c(0,0)) +
  ylab("Change from Year Ago, %") +
  ggtitle("Wages and Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Fed is Coy About it, but they are Likely Worried by the Pace of Wage Growth") +
  theme_apricitas + theme(legend.position = c(.50,.32)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-02-01")-(.1861*1100), xmax = as.Date("2019-01-15")-(0.049*1100), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TWOYR_Graph, "Two Year Treasury Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Rent_LessRent_Graph, "Rent and Services Less Rent.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = Wage_Graph, "Wage Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
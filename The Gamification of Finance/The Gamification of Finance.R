pacman::p_load(cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

GME <- tq_get("GME", from = "2020-01-01")#importing stock market data
AMC <- tq_get("AMC", from = "2020-01-01")
TSLA <- tq_get("TSLA", from = "2015-01-01")

SPACS <- data.frame(year = seq(from = 2011, to = 2021), SPAC = c(16,9,10,12,20,13,34,46,59,248,613))

TSLA_Graph <- ggplot() + 
  geom_line(data = TSLA, aes(x = date, y = (adjusted-43.9)/43.9, color = "TSLA"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, big.mark = ","),limits = c(-1,30), expand = c(0,0)) +
  ylab("Total Return Since Jan 2015") +
  ggtitle("Elon Markets Hypothesis") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo Finance data",subtitle = "Tesla's Price Skyrocketed Throughout 2020") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*2583), xmax = as.Date("2015-01-01")-(0.049*2583), ymin = -1-(.3*31), ymax = -1) +
  coord_cartesian(clip = "off")

Meme_Graph <- ggplot() + 
  geom_line(data = GME, aes(x = date, y = (close-6.31)/6.31, color = "GME"), size = 1.25) +
  geom_line(data = AMC, aes(x = date, y = (close-7.46)/7.46, color = "AMC"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, big.mark = ","),limits = c(-1,60), expand = c(0,0)) +
  ylab("Price Return Since Jan 2020") +
  ggtitle("Moonrise") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo Finance data",subtitle = "Gamestop and AMC's Prices Jumped up and Stayed up") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-01-01")-(.1861*720), xmax = as.Date("2020-01-01")-(0.049*720), ymin = -1-(.3*61), ymax = -1) +
  coord_cartesian(clip = "off")

SPAC_Graph <- ggplot() + 
  geom_line(data = SPACS, aes(x = year, y = SPAC,color = "SPACs Per Year"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(0,650), expand = c(0,0)) +
  ylab("Number of SPAC") +
  ggtitle("Special Purpose, Multi-Use") +
  labs(caption = "Graph created by @JosephPolitano using SPACAnalytics data",subtitle = "2021 Saw A Record Number of SPAC IPOs") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 2011-(.1861*10), xmax = 2011-(0.049*10), ymin = 0-(.3*650), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TSLA_Graph, "TSLA.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Meme_Graph, "Meme.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = SPAC_Graph, "SPAC.png", type = "cairo-png") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
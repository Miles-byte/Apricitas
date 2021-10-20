pacman::p_load(plm,transformr,stringi,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

USYIELDCURVE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Yield%20Curve%20is%20a%20Policy%20Choice/USTREASURY-YIELD.csv")

colnames(USYIELDCURVE) <- c("Date",1/12,2/12,3/12,6/12,1,2,3,5,7,10,20,30) 
USYIELDCURVE <- pivot_longer(USYIELDCURVE, cols = 2:13)

colnames(USYIELDCURVE) <- c("Date","Maturity","Value") 
USYIELDCURVE$Maturity <- as.numeric(USYIELDCURVE$Maturity)
USYIELDCURVE$Date <- as.Date(USYIELDCURVE$Date)
USYIELDCURVE <- USYIELDCURVE[USYIELDCURVE$Date >= "2006-02-09" & USYIELDCURVE$Date != "2017-04-14",]

USYIELDCURVE_GRAPH <- ggplot(USYIELDCURVE, aes(x = Maturity ,y = Value/100 ,fill = Date, color = "#00A99D")) +
  geom_line(size = 1.25) +
  geom_point() +
  theme_clean() +
  theme_apricitas +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.055), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,30)) +
  xlab("Maturity, Years") +
  ylab("Yield, %") +
  theme_apricitas +
  ggtitle("U.S. Yield Curve 2006-2021") +
  labs(caption = "Graph created by @JosephPolitano using data from NASDAQ") +
  annotation_custom(apricitas_logo_rast, xmin = 0-(.1861*30), xmax = 0-(0.049*30), ymin = 0-(.3*0.055), ymax = 0) +
  scale_color_manual(values = c("#FFE98F")) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")

USYIELDCURVE_ANIMATED <- USYIELDCURVE_GRAPH + transition_time(Date) + labs(subtitle = 'Date: {frame_time}',size = 2)
animate(USYIELDCURVE_ANIMATED, height = 1140, width = 1824, fps = 45, duration = 25, end_pause = 100, res = 200) #increasing resolution alonside height and width
animate(USYIELDCURVE_ANIMATED, height = 570, width = 912, fps = 15, duration = 10, end_pause = 30, res = 100) #increasing resolution alonside height and width

anim_save("US Yield Curve Animated.gif")


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
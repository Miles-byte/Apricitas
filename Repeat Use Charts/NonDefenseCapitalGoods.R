pacman::p_load(plm,transformr,stringi,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

NEWORDER <- fredr(series_id = "NEWORDER", observation_start = as.Date("1992-02-01")) #downloading new nondefense capital goods ex aircraft data from FRED

PCEPI <- fredr(series_id = c("PCEPI")) #downloading PCEPI data from FRED
REAL_NEWORDER_MERGE <- merge(NEWORDER, PCEPI, by = "date", All = FALSE) #merge capital goods and PCEPI data

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing Apricitas Logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


Real_Neworder_Graph <- ggplot() + #Plotting GDP Growth Rates
  geom_line(data=REAL_NEWORDER_MERGE, aes(x=date, y= value.x/(value.y*10) , color="Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft, 2012 Dollars"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),limits = c(50,90), breaks = c(50,60,70,80,90), expand = c(0,0)) +
  #scale_x_date(limits = c(as.Date("1992-01-01"),as.Date("2021-07-01"))) +
  ylab("2012 Dollars") +
  ggtitle("The Music of Machinery") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Manufacturers' Investment is Increasing, but Remains Low by Historical Standards") +
  theme_apricitas + theme(legend.position = c(.45,.98)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*10836), xmax = as.Date("1992-01-01")-(0.049*10836), ymin = 50-(.3*40), ymax = 50) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = Real_Neworder_Graph, "New Orders Capital Goods.png", type = "cairo-png") #Saving Image of New Manufacturers Orders 


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

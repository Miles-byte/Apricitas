pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


BEA_PAYROLL_INDEX <- fredr(series_id = "NA000275Q",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

QCEW_PAYROLL_INDEX <- bls_api("ENUUS00030010", startyear = 2019) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2019-01-01"), as.Date("2022-01-01"), "3 months")) %>%
  mutate(value= value/22451792.77)

CES_PAYROLL_PRIVATE_INDEX <- fredr(series_id = "CEU0500000017",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL)

QCEW_PAYROLL_PRIVATE_INDEX <- bls_api("ENUUS00030510", startyear = 2019) %>% #QCEW data
  .[order(nrow(.):1),] %>%
  mutate(date = seq(as.Date("2019-01-01"), as.Date("2022-01-01"), "3 months")) %>%
  mutate(value= value/19299130.62)



BEA_QCEW_Graph <- ggplot() + #plotting permanent and temporary job losers
  #geom_line(data=CES_PAYROLL_INDEX, aes(x=date,y= value/1.42,color= "CES Private Payroll (NSA)"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_INDEX, aes(x=date,y= value,color= "QCEW Payrolls (NSA)"), size = 1.25)+ 
  geom_line(data=BEA_PAYROLL_INDEX, aes(x=date,y= value/23898.83,color= "BEA Payrolls (NSA)"), size = 1.25)+ 
  xlab("Date") +
  ylab("Index, Jan/Q1 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("NSA Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Why is there a Discrepancy Between NSA BEA/QCEW data for 2021?") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

QCEW_CES_PrivateGraph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CES_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value/1.42,color= "CES Private Payroll (NSA)"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value,color= "QCEW Private Payrolls (NSA)"), size = 1.25)+ 
  annotate("vline", x = as.Date("2019-01-01"), xintercept = as.Date("2019-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2021-01-01"), xintercept = as.Date("2021-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2022-01-01"), xintercept = as.Date("2022-01-01"), color = "white", size = .5, linetype = "dashed") +
  xlab("Date") +
  ylab("Index, Jan/Q1 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("CES/QCEW Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "No Major Discrepancy Between CES/QCEW Data") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

BEA_QCEW_CES_PrivateGraph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=CES_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value/1.42,color= "CES Private Payroll (NSA)"), size = 1.25)+ 
  geom_line(data=QCEW_PAYROLL_PRIVATE_INDEX, aes(x=date,y= value,color= "QCEW Private Payrolls (NSA)"), size = 1.25)+ 
  geom_line(data=BEA_PAYROLL_INDEX, aes(x=date,y= value/23898.83,color= "BEA Payrolls (NSA)"), size = 1.25)+ 
  annotate("vline", x = as.Date("2019-01-01"), xintercept = as.Date("2019-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2021-01-01"), xintercept = as.Date("2021-01-01"), color = "white", size = .5, linetype = "dashed") +
  annotate("vline", x = as.Date("2022-01-01"), xintercept = as.Date("2022-01-01"), color = "white", size = .5, linetype = "dashed") +
  xlab("Date") +
  ylab("Index, Jan/Q1 2020 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(80,90,100,110,120,130), limits = c(80,130), expand = c(0,0)) +
  ggtitle("BEA/CES/QCEW Discrepancy") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "No Major Discrepancy Between CES/QCEW Data") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_QCEW_Graph, "BEA QCEW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = QCEW_CES_PrivateGraph, "QCEW CES Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = BEA_QCEW_CES_PrivateGraph, "BEA QCEW CES Private.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

US_MANUFACTURING_PRODUCTIVITY <- fredr("PRS30006163")

US_MANUFACTURING_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= US_MANUFACTURING_PRODUCTIVITY, aes(x=date,y=value/value[73]*100,color= "US Manufacturing Sector Labor Productivity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(45,125), expand = c(0,0)) +
  ylab("Index, Q1 2005 = 100") +
  ggtitle("US Manufacturing's Productivity Problem") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "American Manufacturing Productivity Remains Stuck at Pre-Great Recession Levels") +
  theme_apricitas + theme(legend.position = c(.30,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 45-(.3*(80)), ymax = 45) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_MANUFACTURING_PRODUCTIVITY_GRAPH, "US Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

US_TOTAL_PRODUCTIVITY <- fredr("OPHNFB", observation_start = as.Date("1987-01-01"))

US_MANUFACTURING_VS_TOTAL_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= US_MANUFACTURING_PRODUCTIVITY, aes(x=date,y=value/value[73]*100,color= "US Manufacturing Sector Labor Productivity"), size = 1.25) +
  geom_line(data= US_TOTAL_PRODUCTIVITY, aes(x=date,y=value/value[73]*100,color= "US Total Nonfarm Labor Productivity"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(45,135), expand = c(0,0)) +
  ylab("Index, Q1 2005 = 100") +
  ggtitle("US Manufacturing's Productivity Problem") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Overall US Productivity Has Strengthened Post-COVID, Even as Manufacturing Productivity Falters") +
  theme_apricitas + theme(legend.position = c(.30,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 45-(.3*(90)), ymax = 45) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_MANUFACTURING_VS_TOTAL_PRODUCTIVITY_GRAPH, "US Manufacturing Productivity vs Total Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

COMPUTE_ELEC_PRODUCTIVITY <- bls_api("IPUEN334___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN334___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

COMPUTE_PERIPH_PRODUCTIVITY <- bls_api("IPUEN3341__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3341__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

SEMI_ELEC_COMPONENT_PRODUCTIVITY <- bls_api("IPUEN3344__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3344__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

COMPUTE_ELEC_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= COMPUTE_PERIPH_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Computers & Related Peripherals"), size = 1.25) +
  geom_line(data= SEMI_ELEC_COMPONENT_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Semiconductors & Electronic Components"), size = 1.25) +
  geom_line(data= COMPUTE_ELEC_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "All Computer & Electronic Products"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,225), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Electronics Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "American Electronics Manufacturing Productivity Remains Stuck at Pre-Great Recession Levels") +
  theme_apricitas + theme(legend.position = c(.275,.84)) +
  scale_color_manual(name= "Labor Productivity, 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 0-(.3*(225)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COMPUTE_ELEC_PRODUCTIVITY_GRAPH, "Computer Electronics Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MOTOR_VEHICLE_PRODUCTIVITY <- bls_api("IPUEN3361__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3361__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

VEHICLE_BODY_TRAILER_PRODUCTIVITY <- bls_api("IPUEN3362__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3362__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

VEHICLE_PARTS_PRODUCTIVITY <- bls_api("IPUEN3363__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3363__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

VEHICLE_PARTS_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= MOTOR_VEHICLE_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Motor Vehicle Manufacturing & Final Assembly"), size = 1.25) +
  geom_line(data= VEHICLE_BODY_TRAILER_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Vehicle Body & Trailer Manufacturing"), size = 1.25) +
  geom_line(data= VEHICLE_PARTS_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Vehicle Parts Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(45,135), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Vehicle Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Productivity in Vehicle Bodies & Final Assembly Has Stagnated, But in Parts it has Slowly Increased") +
  theme_apricitas + theme(legend.position = c(.29,.84)) +
  scale_color_manual(name= "Labor Productivity, 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), guide = guide_legend(override.aes = list(lwd = c(1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 45-(.3*(90)), ymax = 45) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VEHICLE_PARTS_PRODUCTIVITY_GRAPH, "Vehicle Parts Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PRIMARY_METAL <- bls_api("IPUEN331___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN331___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

FABRICATED_METAL <- bls_api("IPUEN332___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN332___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

METALS_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= PRIMARY_METAL, aes(x=date,y=value/value[19]*100,color= "Primary Metal Manufacturing\n(e.g. Mills and Foundries)"), size = 1.25) +
  geom_line(data= FABRICATED_METAL, aes(x=date,y=value/value[19]*100,color= "Fabricated Metal Manufacturing\n(e.g. Forges and Machine Shops)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(55,125), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Metals Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Despite Intense Levels of Trade Protection, Productivity in the US Metals Industry Has Stagnated") +
  theme_apricitas + theme(legend.position = c(.25,.82)) +
  scale_color_manual(name= "Labor Productivity, 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Primary Metal Manufacturing\n(e.g. Mills and Foundries)","Fabricated Metal Manufacturing\n(e.g. Forges and Machine Shops)"),guide = guide_legend(override.aes = list(lwd = c(1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 55-(.3*(70)), ymax = 55) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = METALS_PRODUCTIVITY_GRAPH, "Metals Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#SURVIVAL RATE
#GROWTH RATE DATA

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
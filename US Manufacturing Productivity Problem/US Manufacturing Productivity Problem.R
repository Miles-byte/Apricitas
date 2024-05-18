pacman::p_load(readabs,rsdmx,censusapi,estatapi,seasonal,openxlsx,readxl,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,tools,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

US_MANUFACTURING_PRODUCTIVITY <- fredr("OPHMFG", units = "lin") %>%
  drop_na()

US_MANUFACTURING_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= US_MANUFACTURING_PRODUCTIVITY, aes(x=date,y=value/value[73]*100,color= "US Manufacturing Sector Labor Productivity\n(Output per Hour Worked)"), size = 1.25) +
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
  ggtitle("Breaking Down US Productivity Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Overall US Productivity Has Strengthened Post-COVID, Even as Manufacturing Productivity Falters") +
  theme_apricitas + theme(legend.position = c(.35,.89)) +
  scale_color_manual(name= "Labor Productivity (Output per Hour Worked), 2005 = 100",values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
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
  geom_line(data= COMPUTE_ELEC_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Total Computer & Electronic Products"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,240), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Electronics Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "American Electronics Manufacturing Productivity Remains Stuck at Pre-Great Recession Levels") +
  theme_apricitas + theme(legend.position = c(.33,.84)) +
  scale_color_manual(name= "Labor Productivity (Output per Hour Worked), 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total Computer & Electronic Products","Computers & Related Peripherals","Semiconductors & Electronic Components"),guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 0-(.3*(240)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = COMPUTE_ELEC_PRODUCTIVITY_GRAPH, "Computer Electronics Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

VEHICLE_TOTAL_PRODUCTIVITY <- bls_api("MPU5360062", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5360062", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

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
  geom_line(data= MOTOR_VEHICLE_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Vehicle Final Assembly Manufacturing"), size = 1.25) +
  geom_line(data= VEHICLE_BODY_TRAILER_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Vehicle Body & Trailer Manufacturing"), size = 1.25) +
  geom_line(data= VEHICLE_PARTS_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Vehicle Parts Manufacturing"), size = 1.25) +
  geom_line(data= VEHICLE_TOTAL_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Total Motor Vehicle & Parts Manufacturing"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(45,140), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Vehicle Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Productivity in Vehicle Production—Including Assembly, Bodies/Trailers, & Parts—Has Stagnated") +
  theme_apricitas + theme(legend.position = c(.35,.84), legend.key.height = unit(0.4,"cm")) +
  scale_color_manual(name= "Labor Productivity (Output per Hour Worked), 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total Motor Vehicle & Parts Manufacturing","Vehicle Final Assembly Manufacturing","Vehicle Body & Trailer Manufacturing","Vehicle Parts Manufacturing"),guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 45-(.3*(95)), ymax = 45) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VEHICLE_PARTS_PRODUCTIVITY_GRAPH, "Vehicle Parts Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

ELEC_APPLIANCE_PRODUCTIVITY <- bls_api("IPUEN335___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN335___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

ELEC_EQUIP_PRODUCTIVITY <- bls_api("IPUEN3353__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3353__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

APPLIANCE_PRODUCTIVITY <- bls_api("IPUEN3352__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3352__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

ELEC_APPLIANCE_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= ELEC_EQUIP_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Household Appliance Manufacturing"), size = 1.25) +
  geom_line(data= APPLIANCE_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Electrical Equipment Manufacturing"), size = 1.25) +
  geom_line(data= ELEC_APPLIANCE_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Total Electrical Equipment, Appliance, & Related Manufacturing"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(40,120), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Electrical Equipment & Appliance\nManufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Productivity in Electrical Equipment and Appliance Manufacturing Has Stagnated Since 2005") +
  theme_apricitas + theme(legend.position = c(.4,.86), legend.key.height = unit(0.4,"cm")) +
  scale_color_manual(name= "Labor Productivity (Output Per Hour Worked), 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total Electrical Equipment, Appliance, & Related Manufacturing","Electrical Equipment Manufacturing","Household Appliance Manufacturing"),guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 40-(.3*(80)), ymax = 40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ELEC_APPLIANCE_PRODUCTIVITY_GRAPH, "Electrical Appliance Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CHEM_PRODUCTIVITY <- bls_api("IPUEN325___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN325___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

PHARM_MEDIC_PRODUCTIVITY <- bls_api("IPUEN3254__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3254__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

BASIC_CHEM_PRODUCTIVITY <- bls_api("IPUEN3251__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3251__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

AG_CHEM_PRODUCTIVITY <- bls_api("IPUEN3253__L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN3253__L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)


CHEM_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= BASIC_CHEM_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Basic Chemical Manufacturing"), size = 1.25) +
  geom_line(data= PHARM_MEDIC_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Pharmaceutical & Medicine Manufacturing"), size = 1.25) +
  geom_line(data= AG_CHEM_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Agricultural Chemical Manufacturing"), size = 1.25) +
  geom_line(data= CHEM_PRODUCTIVITY, aes(x=date,y=value/value[19]*100,color= "Total Chemical Manufacturing"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(40,135), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Chemical Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Productivity in Chemical Manufacturing Has Stagnated, with Pharma Producitivity Falling") +
  theme_apricitas + theme(legend.position = c(.33,.85), legend.key.height = unit(0.4,"cm")) +
  scale_color_manual(name= "Labor Productivity (Output Per Hour Worked), 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Total Chemical Manufacturing","Basic Chemical Manufacturing","Pharmaceutical & Medicine Manufacturing","Agricultural Chemical Manufacturing"),guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 40-(.3*(95)), ymax = 40) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHEM_PRODUCTIVITY_GRAPH, "Chem Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


PRIMARY_METAL <- bls_api("IPUEN331___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN331___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

FABRICATED_METAL <- bls_api("IPUEN332___L000000000", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("IPUEN332___L000000000", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

METALS_PRODUCTIVITY_GRAPH <- ggplot() + #plotting Total Manufacturing Productivity
  geom_line(data= FABRICATED_METAL, aes(x=date,y=value/value[19]*100,color= "Fabricated Metal Manufacturing\n(e.g. Forges and Machine Shops)"), size = 1.25) +
  geom_line(data= PRIMARY_METAL, aes(x=date,y=value/value[19]*100,color= "Primary Metal Manufacturing\n(e.g. Mills and Foundries)"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(55,125), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Metals Manufacturing Productivity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Despite Intense Levels of Trade Protection, Productivity in the US Metals Industry Has Stagnated") +
  theme_apricitas + theme(legend.position = c(.35,.82), legend.key.height = unit(1.3,"cm")) +
  scale_color_manual(name= "Labor Productivity (Output per Hour Worked), 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Primary Metal Manufacturing\n(e.g. Mills and Foundries)","Fabricated Metal Manufacturing\n(e.g. Forges and Machine Shops)"),guide = guide_legend(override.aes = list(lwd = c(1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 55-(.3*(70)), ymax = 55) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = METALS_PRODUCTIVITY_GRAPH, "Metals Manufacturing Productivity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CAPITAL_INTENSITY_TOTAL <- bls_api("MPU9900152", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU9900152", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

CAPITAL_INTENSITY_COMPUTER <- bls_api("MPU5250152", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5250152", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)
  
CAPITAL_INTENSITY_ELECTRICAL <- bls_api("MPU5300152", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5300152", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)
  
CAPITAL_INTENSITY_VEHICLES <- bls_api("MPU5360152", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5360152", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)
  
CAPITAL_INTENSITY_CHEMICALS <- bls_api("MPU5800152", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5800152", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

CAPITAL_INTENSITY_TOTAL_GRAPH <- ggplot() + #plotting Manufacturing Capital Intensity
  geom_line(data= CAPITAL_INTENSITY_COMPUTER, aes(x=date,y=value/value[19]*100,color= "Computers & Electronics Manufacturing"), size = 1.25) +
  geom_line(data= CAPITAL_INTENSITY_ELECTRICAL, aes(x=date,y=value/value[19]*100,color= "Electrical Equipment & Appliances Manufacturing"), size = 1.25) +
  geom_line(data= CAPITAL_INTENSITY_VEHICLES, aes(x=date,y=value/value[19]*100,color= "Vehicle & Parts Manufacturing"), size = 1.25) +
  geom_line(data= CAPITAL_INTENSITY_CHEMICALS, aes(x=date,y=value/value[19]*100,color= "Chemical Manufacturing"), size = 1.25) +
  geom_line(data= CAPITAL_INTENSITY_TOTAL, aes(x=date,y=value/value[19]*100,color= "All Manufacturing"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(75,125), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Manufacturing Capital Intensity") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Weak Investment in the 2010s Has Slowed Capital Intensity's Contribution to Productivity") +
  theme_apricitas + theme(legend.position = c(.375,.83), legend.key.height = unit(0.4,"cm")) +
  scale_color_manual(name= "Contribution of Capital Intensity to Labor Productivity, 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("All Manufacturing","Chemical Manufacturing","Computers & Electronics Manufacturing","Electrical Equipment & Appliances Manufacturing","Vehicle & Parts Manufacturing"),guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 75-(.3*(50)), ymax = 75) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CAPITAL_INTENSITY_TOTAL_GRAPH, "Capital Intensity Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

LABOR_COMPOSITION_TOTAL <- bls_api("MPU9900182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU9900182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

LABOR_COMPOSITION_COMPUTER <- bls_api("MPU5250182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5250182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

LABOR_COMPOSITION_ELECTRICAL <- bls_api("MPU5300182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5300182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

LABOR_COMPOSITION_VEHICLES <- bls_api("MPU5360182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5360182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

LABOR_COMPOSITION_CHEMICALS <- bls_api("MPU5800182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU5800182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

LABOR_COMPOSITION_TOTAL_GRAPH <- ggplot() + #plotting Manufacturing Capital Intensity
  geom_line(data= LABOR_COMPOSITION_COMPUTER, aes(x=date,y=value/value[19]*100,color= "Computers & Electronics Manufacturing"), size = 1.25) +
  geom_line(data= LABOR_COMPOSITION_ELECTRICAL, aes(x=date,y=value/value[19]*100,color= "Electrical Equipment & Appliances Manufacturing"), size = 1.25) +
  geom_line(data= LABOR_COMPOSITION_VEHICLES, aes(x=date,y=value/value[19]*100,color= "Vehicle & Parts Manufacturing"), size = 1.25) +
  geom_line(data= LABOR_COMPOSITION_CHEMICALS, aes(x=date,y=value/value[19]*100,color= "Chemical Manufacturing"), size = 1.25) +
  geom_line(data= LABOR_COMPOSITION_TOTAL, aes(x=date,y=value/value[19]*100,color= "All Manufacturing"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(93,103.5), expand = c(0,0)) +
  ylab("Index, 2005 = 100") +
  ggtitle("US Manufacturing Labor Composition") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "An Aging Workforce and Slowdown in Education Gains Have Weakened Productivity Growth") +
  theme_apricitas + theme(legend.position = c(.72,.28), legend.key.height = unit(0.4,"cm")) +
  scale_color_manual(name= "Contribution of Labor Composition\nto Labor Productivity, 2005 = 100",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("All Manufacturing","Chemical Manufacturing","Computers & Electronics Manufacturing","Electrical Equipment & Appliances Manufacturing","Vehicle & Parts Manufacturing"),guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1987-01-01")-(.1861*(today()-as.Date("1987-01-01"))), xmax = as.Date("1987-01-01")-(0.049*((today()-as.Date("1987-01-01")))), ymin = 93-(.3*(10.5)), ymax = 93) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LABOR_COMPOSITION_TOTAL_GRAPH, "Labor Composition Total Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


LABOR_PRODUCTIVITY_BAR_CHART_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/US%20Manufacturing%20Productivity%20Problem/LABOR_PRODUCTIVITY_BAR_CHART_DATA.csv") %>%
  transmute(Industry,Growth_87_05 = Growth_87_05-1,Growth_05_23 = Growth_05_23-1) %>%
  setNames(c("Industry","1987-2005","2005-2023")) %>%
  pivot_longer(-Industry) %>%
  mutate(Industry = gsub("Computer & Electronics Products","Computer & Electronics",Industry)) %>%
  mutate(Industry = gsub("Electrical Equipment & Appliances","Electrical & Appliances",Industry)) %>%
  mutate(Industry = gsub("Petroleum & Coal Products","Petroleum & Coal",Industry)) %>%
  mutate(Industry = gsub("Plastics & Rubber Products","Plastics & Rubber",Industry)) %>%
  mutate(Industry = gsub("Fabricated Metal Products","Fabricated Metal",Industry)) %>%
  mutate(Industry = factor(Industry, levels = rev(c("Computer & Electronics","Electrical & Appliances","Transportation Equipment","Petroleum & Coal","Machinery","Plastics & Rubber","Chemicals","Food","Fabricated Metal")))) %>%
  mutate(name = factor(name, levels = rev(c("1987-2005","2005-2023"))))

LABOR_PRODUCTIVITY_BAR_CHART_GRAPH <- ggplot(data = LABOR_PRODUCTIVITY_BAR_CHART_DATA, aes(x = Industry, y = value, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate(geom = "hline",y = 0,yintercept = 0, size = .25,color = "white") +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Industry") +
  ylab("Labor Productivity Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.15,9), expand = c(0,0)) +
  scale_x_discrete(breaks = c("Computer & Electronics","Electrical & Appliances","Transportation Equipment","Petroleum & Coal","Machinery","Plastics & Rubber","Chemicals","Food","Fabricated Metal")) +
  ggtitle("Manufacturing Labor Productivity Growth") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Electronics was the Largest Driver of Industry Productivity Growth Pre-05") +
  theme_apricitas + theme(legend.position = c(.6,.75), axis.text.y = element_text(size = 13), plot.title = element_text(size = 23), plot.margin= grid::unit(c(.25, .25, .25, 0), "in")) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Labor Productivity Growth\n(Output per Hour Worked)",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("1987-2005","2005-2023")) +
  coord_flip()

ggsave(dpi = "retina",plot = LABOR_PRODUCTIVITY_BAR_CHART_GRAPH, "Labor Productivity Bar Chart Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

PRODUCT_DISP_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/US%20Manufacturing%20Productivity%20Problem/PRODUCTIVITY_DISPERSION_DATA.csv") %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  filter(naics4 %in% c(3341,3344)) %>%
  mutate(across(where(is.character), as.numeric))

PRODUCT_DISP_DATA_GRAPH <- ggplot() + #plotting Dispersion
  geom_line(data= filter(PRODUCT_DISP_DATA, naics4 == 3341), aes(x=date,y=`d7525.`,color= "Computers & Related Peripherals"), size = 1.25) +
  geom_line(data= filter(PRODUCT_DISP_DATA, naics4 == 3344), aes(x=date,y=`d7525.`,color= "Semiconductors & Electronics Components"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,3.1), expand = c(0,0)) +
  ylab("Difference, Natural Log of Labor Productivity") +
  ggtitle("Dispersion of US Electronics Manufacturing\nLabor Productivity") +
  labs(caption = "Graph created by @JosephPolitano using US Census-BLS Dispersion Statistics on Productivity (DiSP) Data",subtitle = "The Dispersion in US Electronics Manufacturing Productivity Spiked Post-2000") +
  theme_apricitas + theme(legend.position = c(.72,.22)) +
  scale_color_manual(name= "Difference Between 75th and 25th Percentiles\nof the Natural Log of Establishment Labor Productivity",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1988-01-01")-(.1861*(today()-1400-as.Date("1988-01-01"))), xmax = as.Date("1988-01-01")-(0.049*((today()-1400-as.Date("1988-01-01")))), ymin = 0-(.3*(3.1)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = PRODUCT_DISP_DATA_GRAPH, "Product Disp Data Graph Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SHIPMENTS_TOTAL <- fredr("AMTMVS")

SHIPMENTS_COMPUTER <- fredr("A34SVS")

SHIPMENTS_COMPUTER_SHARE <- merge(SHIPMENTS_COMPUTER,SHIPMENTS_TOTAL, by = "date") %>%
  transmute(date, value = value.x/value.y)

SHIPMENTS_COMPUTER_SHARE_GRAPH <- ggplot() + #shipments share
  geom_line(data= SHIPMENTS_COMPUTER_SHARE, aes(x=date,y= value,color= "Computer & Electronics Manufacturing\nShare of All US Manufacturing Shipments"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.13), expand = c(0,0)) +
  ylab("Share of All Manufacturing Shipments") +
  ggtitle("The Decline of US Electronics Manufacturing") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "Electronics Manufacturing Became a Shrinking Share of American Industry Post-01 and Post-08") +
  theme_apricitas + theme(legend.position = c(.72,.75)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1992-01-01")-(.1861*(today()-as.Date("1992-01-01"))), xmax = as.Date("1992-01-01")-(0.049*((today()-as.Date("1992-01-01")))), ymin = 0-(.3*(.13)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = SHIPMENTS_COMPUTER_SHARE_GRAPH, "Shipments Computer Share Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FIRM_ENTRY_RATE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/US%20Manufacturing%20Productivity%20Problem/FIRM_ENTRY_RATE_DATA.csv") %>%
  transmute(firm, date = as.Date(paste0(year, "-01-01")), sector) %>%
  pivot_wider(names_from = sector, values_from = firm)

FIRM_ENTRY_RATE_Graph <- ggplot() + #shipments share
  geom_line(data= FIRM_ENTRY_RATE, aes(x=date,y= `335`/100,color= "Electrical Equipment & Appliance Manufacturing"), size = 1.25) +
  geom_line(data= FIRM_ENTRY_RATE, aes(x=date,y= `336`/100,color= "Transportation Equipment Manufacturing"), size = 1.25) +
  geom_line(data= FIRM_ENTRY_RATE, aes(x=date,y= `325`/100,color= "Chemical Manufacturing"), size = 1.25) +
  geom_line(data= FIRM_ENTRY_RATE, aes(x=date,y= `334`/100,color= "Computer & Electronics Manufacturing"), size = 1.25) +
  geom_line(data= FIRM_ENTRY_RATE, aes(x=date,y= `31-33`/100,color= "All Manufacturing"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.13), expand = c(0,0)) +
  ylab("Share of All Manufacturing Shipments") +
  ggtitle("Manufacturing Firm Entry Rate") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "The Rate of New US Manufacturing Firm Creation Has Declined Significant Since the 1980s") +
  theme_apricitas + theme(legend.position = c(.69,.87), legend.key.height = unit(0.4,"cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("All Manufacturing","Chemical Manufacturing","Computer & Electronics Manufacturing","Electrical Equipment & Appliance Manufacturing","Transportation Equipment Manufacturing"), guide = guide_legend(override.aes = list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1978-01-01")-(.1861*(today()-as.Date("1978-01-01"))), xmax = as.Date("1978-01-01")-(0.049*((today()-as.Date("1978-01-01")))), ymin = 0-(.3*(.13)), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FIRM_ENTRY_RATE_Graph, "Firm Entry Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


USE THIS FOR CONTRIBUTION CALCULATIONS
https://www.bls.gov/productivity/highlights/contributions-of-total-factor-productivity-major-industry-to-output.htm


BUS_LABOR_COMPOSITION <- bls_api("MPU4900182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU4900182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

MAN_LABOR_COMPOSITION <- bls_api("MPU9900182", startyear = 1987, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY")) %>%
  rbind(.,bls_api("MPU9900182", startyear = 2007, endyear = format(Sys.Date(), "%Y"), registrationKey = Sys.getenv("BLS_KEY"))%>% select(-latest)) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  arrange(date)

CAPITAL_INTENSITY_TOTAL_GRAPH <- ggplot() + #plotting Manufacturing Capital Intensity
  geom_line(data= BUS_LABOR_COMPOSITION, aes(x=date,y=value/value[19]*100,color= "Computers & Electronics Manufacturing"), size = 1.25) +
  geom_line(data= MAN_LABOR_COMPOSITION, aes(x=date,y=value/value[19]*100,color= "Electrical Equipment & Appliances Manufacturing"), size = 1.25)


#SURVIVAL RATE
#GROWTH RATE DATA
#SIZE DATA



p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
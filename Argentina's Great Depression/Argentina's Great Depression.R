pacman::p_load(cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

CPI <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Argentina's%20Great%20Depression/Argentina_CPI2.csv")
Labor <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Argentina's%20Great%20Depression/Labor.csv")
NER_RER <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Argentina's%20Great%20Depression/NER_RER.csv")
NGDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Argentina's%20Great%20Depression/NGDP2.csv")
Output_Gap <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Argentina's%20Great%20Depression/Output_Gap.csv")
RGDP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Argentina's%20Great%20Depression/RGDP.csv")

CPI$Date <- as.Date(CPI$Date)
Labor$Date <- as.Date(Labor$Date)
NER_RER$Date <- as.Date(NER_RER$Date)
NGDP$Date <- as.Date(NGDP$Date)
RGDP$Date <- as.Date(RGDP$Date)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


CPI_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 1) +
  geom_line(data = CPI, aes(x = Date, y = CPIgrowth, color = "CPI Inflation"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.021,.08), breaks = c(-0.02,0,0.02,0.04,.06,.08), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1994-01-01"),as.Date("2001-12-01"))) +
  ylab("CPI Inflation, %") +
  ggtitle("Deflationary Drop") +
  labs(caption = "Graph created by @MonetaristMaia using INDEC data",subtitle = "Argentina's Inflation Rate Went Negative During the Early 2000s") +
  theme_apricitas + theme(legend.position = c(.40,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1994-01-01")-(.1861*2891), xmax = as.Date("1994-01-01")-(0.049*2891), ymin = -0.021-(.3*.101), ymax = -0.021) +
  coord_cartesian(clip = "off")

RER_Graph <- ggplot() + 
  geom_line(data = NER_RER, aes(x = Date, y = RER, color = "Real Exchange Rate"), size = 1.25) +
  geom_line(data = NER_RER, aes(x = Date, y = RER_LatAm, color = "Real Exchange Rate, Latin America"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(65,100), breaks = c(70,80,90,100), expand = c(0,0)) +
  ylab("Real Exchange Rate") +
  ggtitle("A Real Problem") +
  labs(caption = "Graph created by @MonetaristMaia using INDEC data",subtitle = "Argentina's Real Exchange Rate Hampered Trade and Economic Activity During the Crisis") +
  theme_apricitas + theme(legend.position = c(.65,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1997-01-01")-(.1861*1795), xmax = as.Date("1997-01-01")-(0.049*1795), ymin = 65-(.3*35), ymax = 65) +
  coord_cartesian(clip = "off")

Output_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 1) +
  geom_line(data = Output_Gap, aes(x = Year, y = Output_Gap, color = "Output Gap, %"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.12,.04), breaks = c(-0.1,-.06,-.02,.02), expand = c(0,0)) +
  ylab("Output Gap, %") +
  ggtitle("Mind the Gap") +
  labs(caption = "Graph created by @MonetaristMaia using INDEC data",subtitle = "Argentina had a Massive Output Gap by 2001") +
  theme_apricitas + theme(legend.position = c(.60,.40)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 1990-(.1861*11), xmax = 1990-(0.049*11), ymin = -0.12-(.3*.16), ymax = -0.12) +
  coord_cartesian(clip = "off")

NGDP_RGDP_Graph <- ggplot() + 
  geom_line(data = NGDP, aes(x = Date, y = Nominal_GDP/1000, color = "NGDP"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(200,325), breaks = c(200,250,300), expand = c(0,0)) +
  ylab("Billions of Pesos") +
  ggtitle("Nominal Shock") +
  labs(caption = "Graph created by @MonetaristMaia using INDEC data",subtitle = "NGDP Stopped Growing After 1998") +
  theme_apricitas + theme(legend.position = c(.75,.80)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1993-04-01")-(.1861*2832), xmax = as.Date("1993-04-01")-(0.049*2832), ymin = 200-(.3*125), ymax = 200) +
  coord_cartesian(clip = "off")

Labor_Graph <- ggplot() + 
  geom_line(data = Labor, aes(x = Date, y = Unemployment/100, color = "Unemployment Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.25), breaks = c(0,.1,.2), expand = c(0,0)) +
  ylab("Unemployment Rate, %") +
  ggtitle("Love's Labor's Lost") +
  labs(caption = "Graph created by @MonetaristMaia using INDEC data",subtitle = "The Unemployment Rate Jumped Back up to 18% During the Crisis") +
  theme_apricitas + theme(legend.position = c(.60,.40)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1995-05-01")-(.1861*2345), xmax = as.Date("1995-05-01")-(0.049*2345), ymin = 0-(.3*.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Graph, "CPI.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = RER_Graph, "RER.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Output_Graph, "Output Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = NGDP_RGDP_Graph, "NGDP.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Labor_Graph, "Labor.png", type = "cairo-png") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
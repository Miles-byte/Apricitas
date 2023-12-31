pacman::p_load(ggpubr,openxlsx,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

REV_UNCERTAINTY <- fredr(series_id = "ATLSBUSRGUP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDP
EMP_UNCERTAINTY <- fredr(series_id = "ATLSBUEGUP",observation_start = as.Date("2017-01-01"),realtime_start = NULL, realtime_end = NULL) #Real GDI

EMP_REV_UNCERTAINTY_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = EMP_UNCERTAINTY, aes(x=date, y = value/100, color = "Employment Growth Uncertainty"), size = 1.25) + 
  geom_line(data = REV_UNCERTAINTY, aes(x=date, y = value/100, color = "Sales Revenue Growth Uncertainty"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.065), breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.06), expand = c(0,0)) +
  ylab("Uncertainty, %") +
  ggtitle("Businesses Are Feeling Less Uncertain") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data",subtitle = "Uncertainty in Businesses' Employment/Revenue Expectations Continues Falling From COVID Highs") +
  theme_apricitas + theme(legend.position = c(.75,.40)) +
  scale_color_manual(name= "Businesses' Uncertainty, Next 12M",values = c("#FFE98F","#00A99D"), breaks = c("Sales Revenue Growth Uncertainty","Employment Growth Uncertainty")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*0.065), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EMP_REV_UNCERTAINTY_Graph, "Emp Rev Uncertainty Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

INFLATION_UNCERTAINTY_DATA <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY-SCE-Data.xlsx?sc_lang=en", sheet = "Inflation uncertainty") %>%
  slice(-(1:2)) %>%
  setnames(c("date","1yruncertainty","3yruncertainty")) %>%
  mutate(date = seq.Date(from = as.Date("2013-06-01"), by = "1 months", length = nrow(.))) %>%
  mutate(across(where(is.character), ~ as.numeric(.))) %>%
  filter(date >= as.Date("2018-01-01"))

INFLATION_UNCERTAINTY_DATA_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = INFLATION_UNCERTAINTY_DATA, aes(x=date, y = `1yruncertainty`/100, color = "One-year Ahead"), size = 1.25) + 
  geom_line(data = INFLATION_UNCERTAINTY_DATA, aes(x=date, y = `3yruncertainty`/100, color = "Three-year Ahead"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.05), breaks = c(0,0.01,0.02,0.03,0.04,0.05), expand = c(0,0)) +
  ylab("Uncertainty, %") +
  ggtitle("Consumers Feel Less Uncertain About Inflation") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data",subtitle = "Households' Uncertainty About Inflation is Declining From its 2022 Highs") +
  theme_apricitas + theme(legend.position = c(.75,.40)) +
  scale_color_manual(name= "Consumers' Inflation Uncertainty\nFRBNY Survey of Consumer Expectations",values = c("#FFE98F","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.05), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = INFLATION_UNCERTAINTY_DATA_Graph, "Inflation Data Uncertainty Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


SEP_UNCERTAINTY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Federal%20Reserve%20Raises%20Interest%20Rates/SEP_UNCERTAINTY.csv") %>%
  mutate(date = as.Date(date))

SEP_UNCERTAINTY_GRAPH <- ggplot() + #plotting FOMC uncertainty
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=SEP_UNCERTAINTY, aes(x=date,y= GDP ,color= "GDP"), size = 1.25) +
  geom_line(data=SEP_UNCERTAINTY, aes(x=date,y= URATE ,color= "Unemployment"), size = 1.25) +
  geom_line(data=SEP_UNCERTAINTY, aes(x=date,y= CORE ,color= "Core PCE Inflation"), size = 1.25) +
  annotate("text", label = "Normal Uncertainty Levels ", x = as.Date("2009-01-01")+((today()-as.Date("2019-01-01"))/3), y = 0.0287, color = "white", alpha = 0.6, size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.1,1), breaks = c(0,0.25,0.5,0.75,1), expand = c(0,0)) +
  ylab("Net % Saying Higher Than Normal Uncertainty") +
  ggtitle("FOMC Uncertainty is High But Decreasing") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "A Smaller Majority of FOMC Participants Now Say Uncertainty is Above Normal") +
  theme_apricitas + theme(legend.position = c(.55,.80)) +
  scale_color_manual(name= "Net % of FOMC Participants Saying\nUncertainty is Above Normal For",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  theme(legend.key.width =  unit(.82, "cm"), legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-10-01")-(.1861*(today() - as.Date("2007-10-01"))), xmax = as.Date("2007-10-01")-(0.049*(today() - as.Date("2007-10-01"))), ymin = -.1-(.3*1.1), ymax = -.1) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SEP_UNCERTAINTY_GRAPH, "SEP UNCERTAINTY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIE_1YR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Inflation%20Expectations/ATL_BIE_1YR.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(bie = as.numeric(gsub("%", "", bie)))

BIE_Graph <- ggplot() + #plotting total quits
  geom_line(data=BIE_1YR, aes(x=date,y= bie,color= "Business Unit Cost Inflation Expectations: Next Year"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ggtitle("Inflation Expectations are Falling") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data", subtitle = "Business Unit Cost Inflation Expectations are Falling Significantly") +
  theme_apricitas + theme(legend.position = c(.47,.84)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9"), breaks = c("Business Unit Cost Inflation Expectations: Next Year","Business Unit Cost Inflation Expectations: Next 5-10 Years")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-10-21")-(.1861*(today()-as.Date("2011-10-21"))), xmax = as.Date("2011-10-21")-(0.049*(today()-as.Date("2011-10-21"))), ymin = 0-(.3*.05), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_Graph, "BIE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

SEPUNRATE2022 <- fredr(series_id = "UNRATEMD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2022-01-01")) %>% subset(date < as.Date("2026-01-01"))
SEPUNRATE2022$realtime_start <- as.character(SEPUNRATE2022$realtime_start) %>% { gsub("2023-09-20","September 2023",.) } %>% { gsub("2023-03-22","March 2023",.) } %>% { gsub("2022-03-16","March 2022",.) } %>% { gsub("2022-09-21","September 2022",.) } %>% { gsub("2022-06-15","June 2022",.) } %>% { gsub("2022-12-14","December 2022",.) }%>% { gsub("2023-06-14","June 2023",.) }  %>% factor(levels = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

MANUALUNRATE2022 <- data.frame(
  date = c(as.Date("2023-01-01"),as.Date("2024-01-01"),as.Date("2025-01-01")),
  series_id = c("UNRATEMD","UNRATEMD","UNRATEMD"),
  value = c(3.8,4.1,4.1),
  realtime_start = c("December 2023","December 2023","December 2023"),
  realtime_end = c(as.Date("9999-12-31"),as.Date("9999-12-31"),as.Date("9999-12-31")))

SEPUNRATE2022 <- rbind(SEPUNRATE2022, MANUALUNRATE2022)

SEPGDP2022 <- fredr(series_id = "GDPC1MD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2022-01-01")) %>% subset(date < as.Date("2026-01-01")) %>%
  add_row(date = as.Date("2025-01-01"), series_id = "GDPC1MD", value = 1.8, realtime_start = as.Date("2022-12-14"), realtime_end = as.Date("9999-01-01"))
SEPGDP2022$realtime_start <- as.character(SEPGDP2022$realtime_start) %>% { gsub("2023-12-13","December 2023",.) }%>% { gsub("2023-09-20","September 2023",.) } %>% { gsub("2023-03-22","March 2023",.) } %>% { gsub("2022-03-16","March 2022",.) } %>% { gsub("2022-09-21","September 2022",.) } %>% { gsub("2022-06-15","June 2022",.) } %>% { gsub("2022-12-14","December 2022",.) }%>% { gsub("2023-06-14","June 2023",.) }  %>% factor(levels = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023"))

MANUALGDP2022 <- data.frame(
  date = c(as.Date("2023-01-01"),as.Date("2024-01-01"),as.Date("2025-01-01")),
  series_id = c("UNRATEMD","UNRATEMD","UNRATEMD"),
  value = c(2.1,1.5,1.8),
  realtime_start = c("September 2023","September 2023","September 2023"),
  realtime_end = c(as.Date("9999-12-31"),as.Date("9999-12-31"),as.Date("9999-12-31")))

SEPGDP2022 <- rbind(SEPGDP2022, MANUALGDP2022)


SEPPCEPI2022 <- fredr(series_id = "PCECTPIMD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2022-01-01")) %>% subset(date < as.Date("2026-01-01"))
SEPPCEPI2022$realtime_start <- as.character(SEPPCEPI2022$realtime_start) %>% { gsub("2023-12-13","December 2023",.) }%>% { gsub("2023-09-20","September 2023",.) } %>% { gsub("2023-03-22","March 2023",.) } %>% { gsub("2022-03-16","March 2022",.) } %>% { gsub("2022-09-21","September 2022",.) } %>% { gsub("2022-06-15","June 2022",.) } %>% { gsub("2022-12-14","December 2022",.) }%>% { gsub("2023-06-14","June 2023",.) }  %>% factor(levels = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

MANUALPCEPI2022 <- data.frame(
  date = c(as.Date("2023-01-01"),as.Date("2024-01-01"),as.Date("2025-01-01")),
  series_id = c("UNRATEMD","UNRATEMD","UNRATEMD"),
  value = c(3.3,2.5,2.2),
  realtime_start = c("September 2023","September 2023","September 2023"),
  realtime_end = c(as.Date("9999-12-31"),as.Date("9999-12-31"),as.Date("9999-12-31")))

SEPPCEPI2022 <- rbind(SEPPCEPI2022, MANUALPCEPI2022)

SEPFFR2022 <- fredr(series_id = "FEDTARMD", realtime_start = as.Date("2022-03-16")) %>% subset(realtime_start > as.Date("2022-01-01")) %>% subset(date > as.Date("2022-01-01")) %>% subset(date < as.Date("2026-01-01"))
SEPFFR2022$realtime_start <- as.character(SEPFFR2022$realtime_start) %>% { gsub("2023-12-13","December 2023",.) }%>% { gsub("2023-09-20","September 2023",.) } %>% { gsub("2023-03-22","March 2023",.) } %>% { gsub("2022-03-16","March 2022",.) } %>% { gsub("2022-09-21","September 2022",.) } %>% { gsub("2022-06-15","June 2022",.) } %>% { gsub("2022-12-14","December 2022",.) }%>% { gsub("2023-06-14","June 2023",.) }  %>% factor(levels = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023"))#the brackets wrap and the period acts as a data market to get gsub to work with pipes

MANUALFFR2022 <- data.frame(
  date = c(as.Date("2023-01-01"),as.Date("2024-01-01"),as.Date("2025-01-01")),
  series_id = c("UNRATEMD","UNRATEMD","UNRATEMD"),
  value = c(5.6,5.1,3.9),
  realtime_start = c("December 2023","December 2023","December 2023"),
  realtime_end = c(as.Date("9999-12-31"),as.Date("9999-12-31"),as.Date("9999-12-31")))

SEPFFR2022 <- rbind(SEPFFR2022, MANUALFFR2022)



SEPUNRATE2022_Graph <- ggplot(data = SEPUNRATE2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.056), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Unemployment Rate") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) + #adding manual background to get ggarrange to work
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041","#FF8E72"), breaks = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

SEPGDP2022_Graph <- ggplot(data = SEPGDP2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.056), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Real GDP Growth") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32" ),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041","#FF8E72"), breaks = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

SEPPCEPI2022_Graph <- ggplot(data = SEPPCEPI2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.056), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Inflation (PCEPI)") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"),  plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041","#FF8E72"), breaks = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

SEPFFR2022_Graph <- ggplot(data = SEPFFR2022, aes(x = date, y = value/100, fill = realtime_start)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.056), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ylab(NULL) +
  ggtitle("Interest Rates (FFR)") +
  #labs(caption = "Graph created by @JosephPolitano using Federal Reserve data") +
  theme_apricitas + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#3083DC","#9A348E","#A7ACD9","#F5B041","#FF8E72"), breaks = c("March 2022","June 2022","September 2022","December 2022","March 2023","June 2023","September 2023","December 2023")) +
  #annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-10-15")-(.1861*2200), xmax = as.Date("2015-10-15")-(0.049*2200), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.margin=unit(c(0.15,0.15,0.15,0.15),"cm")) #reducing plot margins makes the ggarrange look better

#arranging the SEP graphs into one items and adding background and border colors to match the theme
SEP_ARRANGE_GRAPH <- ggarrange(SEPUNRATE2022_Graph, SEPPCEPI2022_Graph, SEPGDP2022_Graph, SEPFFR2022_Graph,  ncol = 2, nrow = 2, heights = 20, widths = 10, common.legend = TRUE, legend = "bottom") + bgcolor("#252A32") + border("#252A32")

text <- c("FOMC SEP Projections",fontface = "bold")

# Create a text grob
tgrob <- text_grob(expression(bold("FOMC SEP Projections")),size = 25, color = "white") 
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme_apricitas + theme(plot.margin = margin(0,0,0,0, "cm")) + theme(legend.position = "bottom", plot.title = element_text(size = 14, color = "white"), legend.background = element_rect(fill = "#252A32", colour = "#252A32"), plot.background = element_rect(fill = "#252A32", colour = "#252A32"), legend.key = element_rect(fill = "#252A32", colour = "#252A32")) +
  theme(plot.margin=unit(c(-0.15,-0.15,-0.15,-0.15),"cm"))  
blank <- ""
blankgrob <- text_grob(blank,size = 20)
plot_blank <- as_ggplot(blankgrob) + theme(plot.margin = margin(0,0,0,0, "cm"))
SEP_ARRAGE_TITLE_GRAPH <- ggarrange(plot_0,plot_blank,SEPUNRATE2022_Graph, SEPPCEPI2022_Graph, SEPGDP2022_Graph, SEPFFR2022_Graph,  ncol = 2, nrow = 3, heights = c(5,20,20), widths = 10, common.legend = TRUE, legend = "bottom") + bgcolor("#252A32") + border("#252A32")

ggsave(dpi = "retina",plot = SEP_ARRAGE_TITLE_GRAPH, "SEP ARRANGE TITLE.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


SEPGDP2023_Line <- fredr(series_id = "GDPC1MD", realtime_start = as.Date("2018-01-01")) %>% subset(realtime_start > as.Date("2018-01-01")) %>% subset(date > as.Date("2022-01-01")) %>% subset(date < as.Date("2026-01-01")) %>%
  add_row(date = as.Date("2025-01-01"), series_id = "GDPC1MD", value = 2.2, realtime_start = as.Date("2022-03-15"), realtime_end = as.Date("9999-01-01")) %>%
  filter(date == as.Date("2023-01-01"))
  
SEPGDP2023_Line_Graph <- ggplot() + #plotting total quits
  geom_line(data=SEPGDP2023_Line, aes(x=realtime_start,y= value/100,color= "FOMC Median 2023 Real GDP Growth Projection"), size = 1.25)+ 
  xlab("Date") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.03), breaks = c(0,.01,.02,.03,.04,.05), expand = c(0,0)) +
  ggtitle("US Growth: Defying Expectations") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data", subtitle = "2023 GDP Growth Projections Had to Be Revised Up Repeatedly as Recession Expectations Faded") +
  theme_apricitas + theme(legend.position = c(.5,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2020-09-16")-(.1861*(today()-as.Date("2020-09-16"))), xmax = as.Date("2020-09-16")-(0.049*(today()-as.Date("2020-09-16"))), ymin = 0-(.3*.03), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SEPGDP2023_Line_Graph, "SEP GDP 2023 Line.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") 

NY_SERVICES_DIFF <- fredr(series_id = "EMFDINA066MNFRBNY",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
NY_MANUFACT_DIFF <- fredr(series_id = "NEFDISA066MSFRBNY",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
TX_SERVICES_DIFF <- fredr(series_id = "TSSOSFEMPSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
TX_MANUFACT_DIFF <- fredr(series_id = "FNEMPSAMFRBDAL",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)
PHI_MANUFACT_DIFF <- fredr(series_id = "NEFDFSA066MSFRBPHI",observation_start = as.Date("2018-01-01"),realtime_start = NULL, realtime_end = NULL)


SERVICES_DIFF_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = NY_SERVICES_DIFF, aes(x = date, y = value, color = "NY Region Services"), size = 1.25) +
  geom_line(data = TX_SERVICES_DIFF, aes(x = date, y = value, color = "TX Services"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-40,60), breaks = c(-40,-20,0,20,40,60), expand = c(0,0)) +
  ylab("Diffusion Index, Positive Number Indicates Growth") +
  ggtitle("Services Hiring Plans") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Services Employment Growth Forecasts are Holding Up Better, Likely Due to Sectoral Rotations") +
  theme_apricitas + theme(legend.position = c(.72,.20)) +
  scale_color_manual(name= "Future Employment Diffusion Index",values = c("#FFE98F","#00A99D","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1900), xmax = as.Date("2018-01-01")-(0.049*1900), ymin = -40-(.3*100), ymax = -40) +
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = SERVICES_DIFF_Graph, "SERVICES DIFF Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

MANUFACT_DIFF_Graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = NY_MANUFACT_DIFF, aes(x = date, y = value, color = "NY Manufacturing"), size = 1.25) +
  geom_line(data = TX_MANUFACT_DIFF, aes(x = date, y = value, color = "TX Manufacturing"), size = 1.25) +
  geom_line(data = PHI_MANUFACT_DIFF, aes(x = date, y = value, color = "PHI Region Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(-40,60), breaks = c(-40,-20,0,20,40,60), expand = c(0,0)) +
  ylab("Diffusion Index, Positive Number Indicates Growth") +
  ggtitle("Manufacturing Hiring Plans") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Manufacturing Employment Growth Forecasts are Weakening") +
  theme_apricitas + theme(legend.position = c(.72,.20)) +
  scale_color_manual(name= "Future Employment Diffusion Index",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","RED"), breaks = c("NY Manufacturing","TX Manufacturing","PHI Region Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*1900), xmax = as.Date("2018-01-01")-(0.049*1900), ymin = -40-(.3*100), ymax = -40) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MANUFACT_DIFF_Graph, "MANUFACT DIFF Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HOME_PRICE_UNCERTAINTY_SCE_DATA <- read.xlsx("https://www.newyorkfed.org/medialibrary/Interactives/sce/sce/downloads/data/FRBNY-SCE-Data.xlsx?sc_lang=en",10) %>%
  setNames(c("date","uncertainty")) %>%
  slice(-1,-2) %>%
  transmute(date = as.Date(paste0(date,"01"), "%Y%m%d"), uncertainty = as.numeric(uncertainty))

HOME_PRICE_UNCERTAINTY_SCE_DATA_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = HOME_PRICE_UNCERTAINTY_SCE_DATA, aes(x = date, y = uncertainty/100, color = "Median One-Year Ahead Home Price Change Uncertainty\nFRBNY Survey of Consumer Expectations"), size = 1.25) +
  xlab("Date") +
  ylab("Uncertainty in Home Price Growth, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.01,0.02,0.03,.04,.05,.06), limits = c(0,.05), expand = c(0,0)) +
  ggtitle("Home Price Uncertainty is High But Falling") +
  labs(caption = "Graph created by @JosephPolitano using FRBNY data", subtitle = "Home Price Uncertainty is Declining But Still Elevated Amidst Higher Rates") +
  theme_apricitas + theme(legend.position = c(.5,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*0.05), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 27))

ggsave(dpi = "retina",plot = HOME_PRICE_UNCERTAINTY_SCE_DATA_graph, "Home Price Uncertainty.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

HOUSING_STARTS_DISPERSION <- read.xlsx("https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/dispersion_3.xlsx", sheet = "HOUSING") %>%
  slice(-(1:7)) %>%
  {setNames(., unlist(.[1, ]))} %>%
  slice(-(1:1)) %>%# Remove row 8 (which is now row 1)
  mutate(`Survey_Date(T)` = as.yearqtr(`Survey_Date(T)`, format = " %YQ%q")) %>% # Convert Survey_Date to yearqtr
  mutate(`Survey_Date(T)` = as.Date(`Survey_Date(T)`)) %>%
  filter(`Survey_Date(T)` >= as.Date("2018-01-01")) %>%# Convert from yearqtr to Date (start of the quarter)
  mutate(across(where(is.character), as.numeric))
  
HOUSING_STARTS_DISPERSION_graph <- ggplot() + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = HOUSING_STARTS_DISPERSION, aes(x = `Survey_Date(T)`, y = `HOUSING_D3(T+4)`/100, color = "Dispersion in Year-Ahead Housing Starts Forecasts\nSurvey of Professional Forecasters"), size = 1.25) +
  xlab("Date") +
  ylab("Uncertainty in Housing Starts, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.10,0.15,0.20), limits = c(0,.22), expand = c(0,0)) +
  ggtitle("Uncertainty about Housing Construction Has Fallen") +
  labs(caption = "Graph created by @JosephPolitano using Philly Fed data", subtitle = "Uncertainty About Housing Starts Has Fallen Significantly Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.5,.25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FF8E72","#6A4C93","#A7ACD9","#3083DC","#9A348E","#00A99D","#EE6055")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*0.22), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 23))

ggsave(dpi = "retina",plot = HOUSING_STARTS_DISPERSION_graph, "Housing Starts Uncertainty.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
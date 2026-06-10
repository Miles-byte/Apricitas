pacman::p_load(prismatic,ggpubr,ggrepel,tigris,purrr,forecast,imputeTS,tsibble,sf,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

install_github("keberwein/blscrapeR")
library(blscrapeR)

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


BEA_PR_GDP_CONTRIB_SPECS <- list(
  "UserID" = Sys.getenv("BEA_KEY"), # Set up API key
  "Method" = "GetData", # Method
  "datasetname" = "Regional", # Specify dataset
  "TableName" = "PRGDP1-4", # Specify table within the dataset
  "Frequency" = "A", # Specify the line code
  "LineCode" = "All", # Specify the line code
  "GeoFips" = "State", # Specify the geographical level
  "Year" =  paste(seq(from = 2013, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ",") # Specify the year
)

BEA_PR_GDP_CONTRIB <- beaGet(BEA_PR_GDP_CONTRIB_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  filter(Description %in% c("Personal consumption expenditures ", "Fixed investment ", "Change in private inventories ", "Net exports of goods and services ", "Government consumption expenditures and gross investment ")) %>%
  mutate(name = case_when(
    Description == "Personal consumption expenditures " ~ "Consumption",
    Description == "Fixed investment " ~ "Investment",
    Description == "Change in private inventories " ~ "Inventories",
    Description == "Net exports of goods and services " ~ "Net Exports",
    Description == "Government consumption expenditures and gross investment " ~ "Government",
    TRUE ~ Description
  )) %>%
  transmute(date = as.Date(paste0(TimePeriod,"-01-01")),
            value = DataValue,
            name = factor(name, levels = c("Consumption","Investment","Inventories","Net Exports","Government")))

BEA_PR_GDP <- beaGet(BEA_PR_GDP_CONTRIB_SPECS, iTableStyle = FALSE, asWide = FALSE) %>%
  filter(Description %in% c("Gross domestic product ")) %>%
  transmute(date = as.Date(paste0(TimePeriod,"-01-01")),
            value = DataValue)
            
  
GDPContribPR_Graph <- ggplot(BEA_PR_GDP_CONTRIB, aes(fill=name, x=date, y=value/100)) +
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_point(data = BEA_PR_GDP, aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.12,0.1), breaks = c(-0.1,-0.05,0,0.05,0.1), expand = c(0,0)) +
  ylab("Contributions, Percent, Annual") +
  ggtitle("Contributions to Puerto Rican RGDP Growth") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = "right") +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#3083DC","#EE6055","#A7ACD9","black"), breaks = c("Consumption","Investment","Inventories","Net Exports","Government")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(today()-as.Date("2013-01-01"))), ymin = -0.12-(.3*0.22), ymax = -0.12) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = GDPContribPR_Graph, "GDP Contributions Puerto Rico.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

pacman::p_load(stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

MULTIPLE_JOBHOLDERS <- fredr(series_id = "LNS12026619",observation_start = as.Date("2018-01-01")) #downloading Multiple Jobholders data
SELF_EMPLOYED <- fredr(series_id = "LNS12027714",observation_start = as.Date("2018-01-01")) #downloading Self Employed data
AGRICULTURAL <- fredr(series_id = "LNS12034560",observation_start = as.Date("2018-01-01")) #downloading Agricultural Workers


ggsave(dpi = "retina",plot = NordStream_Pipeline_Import_Graph, "Nord Stream Pipeline Imports.png", type = "cairo-png") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
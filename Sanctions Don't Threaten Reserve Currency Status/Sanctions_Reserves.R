pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

PPGDollars <- data.frame(year = seq(from = 1970, to = 2020,by = 1), pct = c(42.89775075,42.84433634,44.77677471,44.85480762,47.99554459,52.54366749,54.43925224,52.32448099,50.16665252,52.20531415,54.18659884,56.81571904,58.66587838,60.59223031,62.42090278,57.61286343,52.61840475,46.38805699,47.34022262,47.09933416,43.76347843,42.33719659,43.19912786,47.23675511,47.26050782,47.21783577,49.26831336,54.54585376,57.44416867,57.82741502,61.15772736,64.20724285,63.55235581,61.14550266,60.80299419,64.72708653,65.86078181,67.14628425,67.27948531,67.50953254,69.46248803,71.47311478,74.74412886,77.68007089,79.97193598,80.57281764,80.08800355,79.74770395,79.80641797,79.84127132,79.58991954))

SWIFTFEB22 <- data.frame(currency = c("US Dollar","Euro","Pound","Yen","Canadian Dollar","Australian Dollar","Renminbi"),pct = c(41.51,38.94,4.29,3.41,2.16,1.53,1.43))


ggsave(dpi = "retina",plot = EPOP_SA_NSA_Graph, "EPOP NSA SA.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
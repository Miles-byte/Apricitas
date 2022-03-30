pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

PPGDollars <- data.frame(year = seq(from = 1970, to = 2020,by = 1), pct = c(42.89775075,42.84433634,44.77677471,44.85480762,47.99554459,52.54366749,54.43925224,52.32448099,50.16665252,52.20531415,54.18659884,56.81571904,58.66587838,60.59223031,62.42090278,57.61286343,52.61840475,46.38805699,47.34022262,47.09933416,43.76347843,42.33719659,43.19912786,47.23675511,47.26050782,47.21783577,49.26831336,54.54585376,57.44416867,57.82741502,61.15772736,64.20724285,63.55235581,61.14550266,60.80299419,64.72708653,65.86078181,67.14628425,67.27948531,67.50953254,69.46248803,71.47311478,74.74412886,77.68007089,79.97193598,80.57281764,80.08800355,79.74770395,79.80641797,79.84127132,79.58991954))

SWIFT <- data.frame(currency = c("US Dollar","Euro","Pound","Yen","Canadian Dollar","Australian Dollar","Renminbi"),pct = c(41.51,38.94,4.29,3.41,2.16,1.53,1.43))
SWIFT$currency <- factor(SWIFT$currency, levels = c("Renminbi","Australian Dollar","Canadian Dollar","Yen","Pound","Euro","US Dollar"), ordered = TRUE)


Reserves <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/afe55d970a4fb58333dd7a9f5862409c5646ceb9/Sanctions%20Don't%20Threaten%20Reserve%20Currency%20Status/IMF_Reserves.csv")
Reserves$Date <- as.Date(Reserves$Date)

ggsave(dpi = "retina",plot = EPOP_SA_NSA_Graph, "EPOP NSA SA.png", type = "cairo-png") #cairo gets rid of anti aliasing

PPGDollars_Graph <- ggplot() + #plotting ice corporate index
  geom_line(data=PPGDollars, aes(x=year,y= pct/100,color= "Percent of Foreign Currency Debt in Dollars, Low and Middle Income Countries"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Dollar Dominance") +
  labs(caption = "Graph created by @JosephPolitano using IMF data",subtitle = "Low and Middle Income Countries Have Become More Likely To Borrow in Dollars") +
  theme_apricitas + theme(legend.position = c(.50,.95)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = 1970-(.1861*50), xmax = 1970-(0.049*50), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

Reserves_Graph <- ggplot() + #plotting ice corporate index
  geom_line(data=Reserves, aes(x=Date,y= Dollars/100,color= "Dollars"), size = 1.25) +
  geom_line(data=Reserves, aes(x=Date,y= Euro/100,color= "Euros"), size = 1.25) +
  geom_line(data=Reserves, aes(x=Date,y= Renminbi/100,color= "Renminbi"), size = 1.25) +
  geom_line(data=Reserves, aes(x=Date,y= Yen/100,color= "Yen"), size = 1.25) +
  geom_line(data=Reserves, aes(x=Date,y= Pound/100,color= "Pounds"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Reserve Currency") +
  labs(caption = "Graph created by @JosephPolitano using IMF data",subtitle = "The Dollar Remains Far and Away the Largest Share of Foreign Exchange Reserves") +
  theme_apricitas + theme(legend.position = c(.80,.45), legend.text = element_text(size = 13, color = "white")) +
  scale_color_manual(name = NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Dollars","Euros","Yen","Pounds","Renminbi")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1999-04-01")-(.1861*8219), xmax = as.Date("1999-04-01")-(0.049*8219), ymin = 0-(.3*1), ymax = 0) +
  coord_cartesian(clip = "off")

SWIFT_Reserves_Graph <- ggplot(SWIFT, aes(x = currency, y = pct/100))+
  geom_bar(aes(fill = currency), position = "dodge", stat = "identity", width = 0.7, color = NA) +
  scale_x_discrete(labels = c("Renminbi","Australian Dollar","Canadian Dollar","Yen","Pound","Euro","US Dollar")) +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,.5), breaks = c(0,.1,.2,.3,.4,.5), expand = c(0,0)) + #adding % format
  theme_apricitas +  theme(legend.position = "none") +
  xlab("Currency") +
  ylab("Share of International Payments Outside Eurozone, February 2022") +
  ggtitle("Reserve Currency") +
  labs(caption = "Graph created by @JosephPolitano Using SWIFT data",subtitle = "The Dollar and Euro are Dominant in International Payments") +
  scale_fill_manual(values = c("#FFE98F","#FFE98F","#FFE98F","#FFE98F","#FFE98F","#FFE98F","#FFE98F")) +
  coord_flip()

ggsave(dpi = "retina",plot = PPGDollars_Graph, "PPGDollars.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = Reserves_Graph, "Reserves.png", type = "cairo-png") #cairo gets rid of anti aliasing
ggsave(dpi = "retina",plot = SWIFT_Reserves_Graph, "Swift.png", type = "cairo-png") #cairo gets rid of anti aliasing


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
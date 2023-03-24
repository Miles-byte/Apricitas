pacman::p_load(dplyr,seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

CS <- tq_get("CS", from = "2007-04-25")

CS_TOTAL_RETURN_graph <- ggplot() + #plotting loan performance data
  geom_line(data=CS, aes(x=date,y= (adjusted/adjusted[1]*100)/100-1,color= "Credit Suisse (CS), Total Return Since April 2007"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Total Return, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,-.25,-.5,-.75,-1.00), limits = c(-1.00,0), expand = c(0,0)) +
  ggtitle("The Death of Credit Suisse") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "Before Being Sold to UBS, Credit Suisse's Financial Struggles Had Been Going On for Years") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-04-25")-(.1861*(today()-as.Date("2007-04-25"))), xmax = as.Date("2007-04-25")-(0.049*(today()-as.Date("2007-04-25"))), ymin = -1-(.3*1), ymax = -1.00) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CS_TOTAL_RETURN_graph, "CS Total Return Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

AT1_ETF <- tq_get("AT1.L", from = "2023-03-01")

AT1_ETF_TOTAL_RETURN_graph <- ggplot() + #plotting loan performance data
  geom_line(data=AT1_ETF, aes(x=date,y= (adjusted/adjusted[1]*100)/100-1,color= "Invesco European AT1 Capital USD Bond ETF"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Total Return, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,-0.05,-0.10,-0.15), limits = c(-.16,0.01), expand = c(0,0)) +
  ggtitle("Additional Problems") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "Investors Have Soured on European AT1 Capital Bonds in the Wake of Credit Suisse's Fall") +
  theme_apricitas + theme(legend.position = c(.4,.10)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-03-01")-(.1861*(today()-as.Date("2023-03-01"))), xmax = as.Date("2023-03-01")-(0.049*(today()-as.Date("2023-03-01"))), ymin = -.16-(.3*0.17), ymax = -.16) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = AT1_ETF_TOTAL_RETURN_graph, "AT1 Total Return Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

CS_12_31 <- as.data.frame(read.csv("https://www.ffiec.gov/npw/FinancialReport/ReturnFinancialReportCSV?rpt=FRY9C&id=1574834&dt=20221231")) %>%
  subset(! ItemName %in% c("Institution Name","Street Address","City","State","Zip Code")) %>%
  mutate(date = as.Date(c("2022-12-31")))



test <- CS_12_31[-9,]

test <- transpose(CS_12_31)
transpose(test)
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
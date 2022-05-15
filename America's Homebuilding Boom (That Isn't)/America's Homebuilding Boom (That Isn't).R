pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

TOTAL_HOUSING_GROWTH <- fredr(series_id = "ETOTALUSQ176N", units = "pc1") #downloading total housing growth
TOTAL_HOUSING_STARTS <- fredr(series_id = "HOUST") #downloading total housing starts
TOTAL_HOUSING_STARTS_SUBSET <- fredr(series_id = "HOUST", observation_start = as.Date("2000-01-01")) #downloading total housing starts

TOTAL_HOUSING_COMPLETIONS <- fredr(series_id = "COMPUTSA", observation_start = as.Date("2000-01-01")) #downloading total completions
TOTAL_HOUSING_UNDERCONSTRUCTION <- fredr(series_id = "UNDCONTSA", observation_start = as.Date("2000-01-01")) #total under construction

HOUSING_UNITS_5PLUS <- fredr(series_id = "HOUST5F", observation_start = as.Date("2000-01-01")) #5 or More Units
HOUSING_UNITS_SFH <- fredr(series_id = "HOUST1F", observation_start = as.Date("2000-01-01")) #Single Family Home

THIRTY_YR_FIXED <- fredr(series_id = "MORTGAGE30US", observation_start = as.Date("2000-01-01")) #Single Family Home

RENTAL_VACANCY_RATE <- fredr(series_id = "RRVRUSQ156N", observation_start = as.Date("2000-01-01")) #Single Family Home
HOMEOWNER_VACANCY_RATE <- fredr(series_id = "RHVRUSQ156N", observation_start = as.Date("2000-01-01")) #Single Family Home

AUTHORIZED_NOT_STARTED <- fredr(series_id = "AUTHNOTTSA", observation_start = as.Date("2000-01-01")) #Single Family Home

CPI_SF <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/Construction_Price_Index_SF.csv")
CPI_MF <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/America's%20Homebuilding%20Boom%20(That%20Isn't)/Construction_Price_Index_MF.csv")

colnames(CPI_MF) <- c("Date", "Multifamily", "Annual_Growth")
colnames(CPI_SF) <- c("Date", "Singlefamily", "Annual_Growth")

CPI_SF$Date <- as.Date(CPI_SF$Date,"%Y-%m-%d")
CPI_MF$Date <- as.Date(CPI_MF$Date, "%m/%d/%Y")

POPULATION <- fredr(series_id = "POPTHM") #population

Starts_Population_Merge <- merge(TOTAL_HOUSING_STARTS, POPULATION, by = "date")


STARTS_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=TOTAL_HOUSING_STARTS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Boom?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Total Housing Starts are at the Highest Levels in More than A Decade") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Privately-Owned Housing Units Started","New Privately-Owned Housing Units Completed")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*28000), xmax = as.Date("1960-01-01")-(0.049*28000), ymin = 0-(.3*3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

STARTS_PERCAPITA_Graph <- ggplot() + #plotting new housing starts
  geom_line(data=Starts_Population_Merge, aes(x=date,y= (value.x/value.y), color= "New Privately-Owned Housing Units Started Per Capita"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(), limits = c(0,.012), expand = c(0,0)) +
  ylab("Units Per Capita, Seasonally Adjusted Annual Rate") +
  ggtitle("The Housing Boom?") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "When Adjusted for Population, Housing Starts Still Look Weak") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1960-01-01")-(.1861*28000), xmax = as.Date("1960-01-01")-(0.049*28000), ymin = 0-(.3*.012), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

TOTAL_HOUSING_GROWTH_Graph <- ggplot() + #plotting growth in total housing units
  geom_line(data=TOTAL_HOUSING_GROWTH, aes(x=date,y= value/100, color= "Annual Growth in Total Housing Units"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(0,.02),breaks = c(0,0.005,0.01,0.015,0.02), expand = c(0,0)) +
  ylab("Percent Change From Year Ago, %") +
  ggtitle("Away from Home") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Growth in the Housing Stock is Extremely Low") +
  theme_apricitas + theme(legend.position = c(.7,.60)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-04-01")-(.1861*8250), xmax = as.Date("2000-04-01")-(0.049*8250), ymin = 0-(.3*.02), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SF_MF_CPI_Graph <- ggplot() + #plotting growth in total housing units
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CPI_SF, aes(x=Date,y= Annual_Growth, color= "Construction Price Index: Single Family"), size = 1.25) +
  geom_line(data=CPI_MF, aes(x=Date,y= Annual_Growth, color= "Construction Price Index: Multi-Family"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.08,.15),breaks = c(-0.05,0,0.05,0.1,0.15), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2022-01-01")))+
  ylab("Percent Change From Year Ago, %") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Construction Costs are Rising Rapidly, Especially for Single-Family Homes") +
  theme_apricitas + theme(legend.position = c(.27,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Construction Price Index: Single Family","Construction Price Index: Multi-Family")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1990-01-01")-(.1861*12250), xmax = as.Date("1990-01-01")-(0.049*12250), ymin = -0.08-(.3*.23), ymax = -0.08) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

VACANCY_RATE_Graph <- ggplot() + #plotting rental vacancy rate
  geom_line(data=RENTAL_VACANCY_RATE, aes(x=date,y= value/100, color= "Rental Vacancy Rate"), size = 1.25) +
  geom_line(data=HOMEOWNER_VACANCY_RATE, aes(x=date,y= value/100, color= "Homeowner Vacancy Rate"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), limits = c(0,.12),breaks = c(0,0.04,0.08,0.12), expand = c(0,0)) +
  ylab("Vacancy Rate, %") +
  ggtitle("No Vacancies") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Vacancy Rates are at the Lowest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.75,.80)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Rental Vacancy Rate","Homeowner Vacancy Rate")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*.12), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

AUTHORIZED_NOT_STARTED_Graph <- ggplot() + #plotting authorized not started
  geom_line(data=AUTHORIZED_NOT_STARTED, aes(x=date,y= value, color= "New Privately-Owned Housing Units Authorized but Not Started"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "K", accuracy = 1), limits = c(0,300), expand = c(0,0)) +
  ylab("Units, Thousands, Seasonally Adjusted Annual Rate") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "The Number of Authorized Units that Haven't Broke Ground is at the Highest Level in Decades") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*300), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

SFH_MF_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=HOUSING_UNITS_5PLUS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started: Units in Buildings with 5 Units or More"), size = 1.25) +
  geom_line(data=HOUSING_UNITS_SFH, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started: Single-Family Units"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("La Vie Boheme") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Multifamily Housing Starts Are Above Pre-2008 Levels, But Single Family Starts Aren't") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

UNDER_CONSTRUCTION_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=TOTAL_HOUSING_UNDERCONSTRUCTION, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Under Construction"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Supply Chain Issues Have Meant that Many Housing Starts Haven't Been Finished") +
  theme_apricitas + theme(legend.position = c(.5,.93)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

STARTS_COMPLETIONS_Graph <- ggplot() + #plotting SF and MF housing
  geom_line(data=TOTAL_HOUSING_STARTS_SUBSET, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Started"), size = 1.25) +
  geom_line(data=TOTAL_HOUSING_COMPLETIONS, aes(x=date,y= value/1000, color= "New Privately-Owned Housing Units Completed"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), limits = c(0,2.3), expand = c(0,0)) +
  ylab("Units, Millions, Seasonally Adjusted Annual Rate") +
  ggtitle("Under Construction") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Supply Chain Issues Have Meant that Many Housing Starts Haven't Been Finished") +
  theme_apricitas + theme(legend.position = c(.65,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("New Privately-Owned Housing Units Started","New Privately-Owned Housing Units Completed")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*8250), xmax = as.Date("2000-01-01")-(0.049*8250), ymin = 0-(.3*2.3), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

THIRTY_YEAR_FIXED_Graph <- ggplot() + #plotting growth in total housing units
  geom_line(data=THIRTY_YR_FIXED, aes(x=date,y= value/100, color= "30-Year Fixed Rate Mortgage Average in the United States"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.09),breaks = c(0,0.02,0.04,0.06,0.08), expand = c(0,0)) +
  ylab("%") +
  ggtitle("Adjusting Rates") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve data",subtitle = "Mortgage Rates Have Rapidly Rebounded to 5%") +
  theme_apricitas + theme(legend.position = c(.5,.90)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*7250), xmax = as.Date("2000-01-01")-(0.049*7250), ymin = 0-(.3*.09), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


ggsave(dpi = "retina",plot = THIRTY_YEAR_FIXED_Graph, "Thirty Year Fixed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_COMPLETIONS_Graph, "Starts Completions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = UNDER_CONSTRUCTION_Graph, "Under Construction.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SFH_MF_Graph, "Single Family Multi Family.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = TOTAL_HOUSING_GROWTH_Graph, "Total Housing Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_PERCAPITA_Graph, "Starts Per Capita.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = STARTS_Graph, "Starts.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = AUTHORIZED_NOT_STARTED_Graph, "Authorized Not Started.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = VACANCY_RATE_Graph, "Vacancy Rate.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = SF_MF_CPI_Graph, "SF MF CPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
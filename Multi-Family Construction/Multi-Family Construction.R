pacman::p_load(ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


MULTIFAM_STARTS <- fredr("HOUST5F", observation_start = as.Date("2017-01-01"))

MULTIFAM_COMPLETE <- fredr("COMPU5MUSA", observation_start = as.Date("2017-01-01"))

MULTIFAM_STARTS_COMPLETIONS_Graph <- ggplot() + #Starts completions
  geom_line(data = MULTIFAM_STARTS, aes(x=date, y = value, color = "Starts"), size = 1.25) + 
  geom_line(data = MULTIFAM_COMPLETE, aes(x=date, y = value, color = "Completions"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), limits = c(0,650), expand = c(0,0)) +
  ylab("Units, Annual Rate") +
  ggtitle("Multifamily Starts and Completions") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Multifamily Housing Completions are Now at a 50-Year High, Well Exceeding Multifamily Starts") +
  theme_apricitas + theme(legend.position = c(.29,.88)) +
  scale_color_manual(name= "5+ Unit Housing Construction, Annual Rate",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Starts","Completions")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*650), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MULTIFAM_STARTS_COMPLETIONS_Graph, "Multifamily Starts Completions Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


MULTIFAM_AUTH_NOT_COMPLETE <- fredr("AUTHNOT5MUSA", observation_start = as.Date("2017-01-01"))

MULTIFAM_UNDER_CONSTRUCTION <- fredr("UNDCON5MUSA", observation_start = as.Date("2017-01-01"))

MULTIFAM_AUTH_CONSTRUCTION <- merge(MULTIFAM_AUTH_NOT_COMPLETE,MULTIFAM_UNDER_CONSTRUCTION, by = "date") %>%
  select(date, `Under Construction` = value.y, `Authorized but Not Started` = value.x) %>%
  pivot_longer(-date)

MULTIFAM_AUTH_CONSTRUCTION_graph <- ggplot(data = MULTIFAM_AUTH_CONSTRUCTION, aes(x = date, y = value/1000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA, width = 32) +
  xlab("Date") +
  ylab("Units") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "M"), breaks = c(0,.2,.4,.6,.8,1,1.2), limits = c(0,1.25), expand = c(0,0)) +
  ggtitle("The Multifamily Housing Pipeline") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "Nearly 1.1M Multifamily Units Remain Either Under Construction or Awaiting Groundbreaking") +
  theme_apricitas + theme(legend.position = c(.225,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Multifamily (5+) Housing Units",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("Under Construction","Authorized but Not Started")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 0-(.3*1.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MULTIFAM_AUTH_CONSTRUCTION_graph, "Multifam Auth Construction Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MULTIFAM_PRICE_INDEX <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/425cbf6e7ac348dcb9a400cf5bcc2593aa123dc3/Multi-Family%20Construction/MF_CONS_PRICE_INDEX.csv") %>%
  arrange(date) %>%
  mutate(date = as.Date(date)) %>%
  mutate(pct_growth = (value-lag(value,4))/lag(value,4)) %>%
  drop_na() %>%
  filter(date >= as.Date("2017-01-01"))

MULTIFAM_PRICE_INDEX_graph <- ggplot() + #plotting Deposits, Insured and Uninsured
  geom_line(data= MULTIFAM_PRICE_INDEX, aes(x=date,y= pct_growth, color= "Multifamily Housing Construction Price Index\nYear-on-Year Growth"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Percent Growth") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(-.025,0,0.025,0.05,.075,.1), limits = c(-.025,.1), expand = c(0,0)) +
  ggtitle("Multifamily Housing Inflation") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "Multifamily Housing Inflation Has Cooled Significantly Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.55,.95)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -.025-(.3*.125), ymax = -0.025) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MULTIFAM_PRICE_INDEX_graph, "Multifam Price Index Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#CONSTRUCTION TIME
MULTIFAM_CONSTRUCTION_TIME <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Multi-Family%20Construction/MF_CONS_TIME.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

MULTIFAM_CONSTRUCTION_TIME_graph <- ggplot() + #plotting Deposits, Insured and Uninsured
  geom_line(data= MULTIFAM_CONSTRUCTION_TIME, aes(x=date,y= two_four, color= "2-4 Unit Projects"), size = 1.25) +
  geom_line(data= MULTIFAM_CONSTRUCTION_TIME, aes(x=date,y= five_nine, color= "5-9 Unit Projects"), size = 1.25) +
  geom_line(data= MULTIFAM_CONSTRUCTION_TIME, aes(x=date,y= ten_nineteen, color= "10-19 Unit Projects"), size = 1.25) +
  geom_line(data= MULTIFAM_CONSTRUCTION_TIME, aes(x=date,y= twenty_plus, color= "20+ Unit Projects"), size = 1.25) +
  geom_line(data= MULTIFAM_CONSTRUCTION_TIME, aes(x=date,y= total, color= "All Multifamily Housing Projects"), size = 2.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Months") +
  scale_y_continuous(labels = scales::number_format(), breaks = c(7.5,10,12.5,15,17.5,20), limits = c(7.5,20), expand = c(0,0)) +
  ggtitle("Average Multifamily Project Duration") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "The Average Multifamily Housing Projects ") +
  theme_apricitas + theme(legend.position = c(.43,.76)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= "Average Length of Time From Start to Completion, Multifamily Buildings",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("All Multifamily Housing Projects","2-4 Unit Projects","5-9 Unit Projects","10-19 Unit Projects","20+ Unit Projects"), guide=guide_legend(override.aes=list(lwd = c(2.25,1.25,1.25,1.25,1.25)))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1999-01-01")-(.1861*(today()-as.Date("1999-01-01"))), xmax = as.Date("1999-01-01")-(0.049*(today()-as.Date("1999-01-01"))), ymin = 7.5-(.3*13.5), ymax = 7.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MULTIFAM_CONSTRUCTION_TIME_graph, "Multifam Construction Project Duration Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MULTIFAM_CONSTRUCTION_SIZE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Multi-Family%20Construction/MF_UNIT_SIZE.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

MULTIFAM_CONSTRUCTION_SIZE_graph <- ggplot() + #plotting Deposits, Insured and Uninsured
  geom_line(data= MULTIFAM_CONSTRUCTION_SIZE, aes(x=date,y= Under_1k, color= "<1000 sqft"), size = 1.25) +
  geom_line(data= MULTIFAM_CONSTRUCTION_SIZE, aes(x=date,y= X1_1.2k+X1.2_1.4l, color= "1000-1400sqft"), size = 1.25) +
  geom_line(data= MULTIFAM_CONSTRUCTION_SIZE, aes(x=date,y= X1.4_1.8k+X1.8k_plus, color= ">1400sqft"), size = 1.25) +
  xlab("Date") +
  ylab("Months") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"), breaks = c(0,50,100,150,200), limits = c(0,200), expand = c(0,0)) +
  ggtitle("Multifamily Units Completed by Size") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau data", subtitle = "Construction of <1000sqft Units Has Driven the Multifamily Boom of the Last Several Years") +
  theme_apricitas + theme(legend.position = c(.5,.82)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("<1000 sqft","1000-1400sqft",">1400sqft")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1999-01-01")-(.1861*(today()-as.Date("1999-01-01"))), xmax = as.Date("1999-01-01")-(0.049*(today()-as.Date("1999-01-01"))), ymin = 0-(.3*200), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MULTIFAM_CONSTRUCTION_SIZE_graph, "Multifam Construction Project Size Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

MSA_MF_PERMIT_DATA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Multi-Family%20Construction/PERMITS_METRO_ANNUAL.csv") %>%
  filter(CBSA %in% c(35620, 31080, 16980, 19100, 26420, 47900, 37980, 33100, 12060, 14460, 38060, 41860, 40140, 19820, 42660, 33460, 41740, 45300, 19740, 12580, 41180, 36740, 16740, 41700, 38900, 38300, 40900, 12420, 29820, 17460, 28140, 17410, 18140, 26900, 34980, 41940, 47260, 39300, 27260, 33340, 36420, 39580, 31140, 32820, 40060, 41620, 13820, 15380, 23420, 25540)) %>%
  select(CBSA,Name, X5_Plus_2023,X5_Plus_2022) %>%
  transmute(FIPS = CBSA, name = Name, Apt_2023=X5_Plus_2023, Apt_2022=X5_Plus_2022, Pct_Growth = (X5_Plus_2023-X5_Plus_2022)/X5_Plus_2022) %>%
  mutate(Pct_Growth = case_when(
    Pct_Growth > 0.5 ~ 0.5,
    Pct_Growth < -0.5 ~ -0.5,
    TRUE ~ Pct_Growth
  )) #creating bounds for pct growth

MSA_map <- core_based_statistical_areas(cb = TRUE, year = 2021) %>%
  mutate(FIPS = GEOID)

states <- states(cb = TRUE, year = 2021) %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84") %>%
  mutate(state_abbv = gsub("\\s", "", STUSPS)) %>%
  shift_geometry(position = "outside")

MSA_map_US <- merge(MSA_map, MSA_MF_PERMIT_DATA, by = "FIPS") %>%
  st_transform(crs = "+proj=aea +lat_1=20 +lat_2=50 +lat_0=0 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84")
  
MSA_map_US_centroids <- MSA_map_US %>%
  st_centroid()

MSA_MF_PERMIT_DATA_BUB_GRAPH <- MSA_map_US %>%
  ggplot() +
  geom_sf(data = filter(states, !state_abbv %in% c("HI", "AK", "AS", "GU", "MP", "VI", "PR")), color = "grey20", fill = "grey50", lwd = 0.25) + # Black borders for states
  geom_sf(fill = "grey75") +
  geom_point(data = MSA_map_US_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], fill = Pct_Growth, size = Apt_2023/1000), shape = 21, alpha = 0.5, show.legend = TRUE) +
  geom_point(data = MSA_map_US_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], size = Apt_2023/1000), shape = 21, color = "black", fill = NA, alpha = 0.5, show.legend = FALSE) +#geom_point(data = counties_map_increase_centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = Increase > 0, size = Increase/1000000), alpha = 0.5, stroke = 0.25) +
  scale_fill_viridis_c(name = "Growth, 2022-2023",
    limits = c(-.50,.50),
    labels = c("-50%+","-25%","0%","+25%","+50%+"),
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  scale_size_area(name = "Units Permitted, 2023",
                  max_size = 15,
                  breaks = c(0,5,10,15,20,25),
                  labels = c("0","5k","10k","15k","20k","25k"),
                  guide = guide_legend(override.aes = list(fill = c("#3083DC")))) +
  #guides(name = NULL, color = guide_legend(override.aes = list(fill = c("#EE6055","#F5B041","#FFE98F", "#AED581", "#00A99D")))) +
  ggtitle("     Multifamily (5+ Unit) Apartment Permits Issued\n            50 Largest Metro Areas by Population") +
  labs(caption = "Graph created by @JosephPolitano using Census data") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(0, 0, 0, 0), "in"), legend.key = element_blank()) +
  theme(plot.title = element_text(size = 26),axis.title.x = element_blank(),axis.title.y = element_blank())

ggsave(dpi = "retina",plot = MSA_MF_PERMIT_DATA_BUB_GRAPH, "MSA MF Permit Data Bub Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#SLOOS TIGHTENING TO MF PROJECTS
SLOOS_TIGHT <- fredr("SUBLPDRCSM", observation_start = as.Date("2017-01-01"))
SLOOS_DEMAND <- fredr("SUBLPDRCDM", observation_start = as.Date("2017-01-01"))

SLOOS_TIGTEN_DEMAND_GRAPH <- ggplot() + #plotting tightening
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=SLOOS_TIGHT, aes(x=date,y= value/100, color= "Net % of Banks Tightening Standards"), size = 1.25) +
  geom_line(data=SLOOS_DEMAND, aes(x=date,y= -value/100, color= "Net % Reporting Weaker Demand"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.5,.85), expand = c(0,0)) +
  ylab("Percent") +
  ggtitle("The Multifamily Credit Crunch") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Banks Have Rapidly Tightened Credit to Multifamily Projects as Demand Cools") +
  theme_apricitas + theme(legend.position = c(.45,.9)) +
  scale_color_manual(name= "Commercial Real Estate Loans Secured by Multifamily Residential Properties" ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = -0.5-(.3*1.35), ymax = -0.5) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SLOOS_TIGTEN_DEMAND_GRAPH, "SLOOS Tighten Demand Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


APARTMENT_PERMITS_SELECTED_METROS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Multi-Family%20Construction/APARTMENT_PERMITS_SELECTED_METROS.csv") %>%
  drop_na() %>%
  mutate(date = as.Date(date))

APARTMENT_PERMITS_SELECTED_METROS_Graph <- ggplot() + #plotting tightening
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= NYC/1000, color= "New York City"), size = 1.25) +
  geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= DAL/1000, color= "Dallas-Fort Worth"), size = 1.25) +
  geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= HOU/1000, color= "Houston"), size = 1.25) +
  geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= AUS/1000, color= "Austin"), size = 1.25) +
  #geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= ATL/1000, color= "Atlanta"), size = 1.25) +
  #geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= DC/1000, color= "Washington, DC"), size = 1.25) +
  geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= LAX/1000, color= "Los Angeles"), size = 1.25) +
  geom_line(data=APARTMENT_PERMITS_SELECTED_METROS, aes(x=date,y= PHX/1000, color= "Phoenix"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), limits = c(0,50), expand = c(0,0)) +
  ylab("Thousands of Units") +
  ggtitle("Apartment Permits by Metro Area") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Permits Have Contracted in NY, DFW, & Houston—While Staying Stable in Phoenix, Austin, & LA") +
  theme_apricitas + theme(legend.position = c(.75,.15)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E","#3083DC"), breaks = c("New York City","Los Angeles","Houston","Dallas-Fort Worth", "Austin","Phoenix")) +
  guides(color=guide_legend(ncol=2)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-365-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-365-as.Date("2017-01-01"))), ymin = 0-(.3*50), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = APARTMENT_PERMITS_SELECTED_METROS_Graph, "Apartment Permits Selected Metros Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)


Employ_by_sector <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Interpersonal%20Economy%20-%20Automation%20and%20You/employment-by-economic-sector.csv") #importing data on jobs by sector from OWID

colnames(Employ_by_sector) <- c("Country","Country_Code","Year","Services","Manufacturing","Agriculture") #changing colnames to make more sense

Employ_by_sector_percent <- Employ_by_sector #creaging a separate dataframe to calculate percentages

Employ_by_sector_percent$Agriculture <- Employ_by_sector$Agriculture/(Employ_by_sector$Agriculture + Employ_by_sector$Manufacturing + Employ_by_sector$Services)
Employ_by_sector_percent$Manufacturing <- Employ_by_sector$Manufacturing/(Employ_by_sector$Agriculture + Employ_by_sector$Manufacturing + Employ_by_sector$Services)
Employ_by_sector_percent$Services <- Employ_by_sector$Services/(Employ_by_sector$Agriculture + Employ_by_sector$Manufacturing + Employ_by_sector$Services) #calculating percentages of employment in each sector by year

Employ_by_sector <- pivot_longer(Employ_by_sector, cols = Services:Agriculture, names_to = "Sector", values_to = "Employment") #gathering data from sectoral employment for geom_area

Employ_by_sector_percent <- pivot_longer(Employ_by_sector_percent, cols = Services:Agriculture, names_to = "Sector", values_to = "Employment") #gathering data from sectoral employment to one column for geom_area


theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 11, color = "white")) #using the FT theme and white axis lines for a "theme_apricitas"

Employ_by_sector_graph <- ggplot() + 
  geom_area(data = Employ_by_sector[Employ_by_sector$Country_Code == "USA", ], aes(x=Year, y = Employment/1000000, fill = Sector),color = NA, size = 0) +
  xlab("Year") +
  scale_y_continuous(limits = c(0,150), breaks = c(0,50,100,150), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Employment by Sector, Millions") +
  ggtitle("At Your Service") +
  labs(caption = "Graph created by @JosephPolitano using OWID data based on Herrendorf et al. (2014)",subtitle = "The Service Sector Represents the Vast Majority of Employment in America") +
  theme_apricitas + theme(legend.position = c(.70,.8)) +
  scale_fill_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))

Employ_by_sector_percent_graph <- ggplot() + 
  geom_area(data = Employ_by_sector_percent[Employ_by_sector$Country_Code == "USA", ], aes(x=Year, y = Employment, fill = Sector),color = NA, size = 0) + #color is NA and size is 0 so that the borders do not show up on the area chart
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), breaks = c(0,0.25,0.5,.75,1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("Employment by Sector, % of Total Employment") +
  ggtitle("At Your Service") +
  labs(caption = "Graph created by @JosephPolitano using OWID data based on Herrendorf et al. (2014)",subtitle = "The Service Sector Represents the Vast Majority of Employment in America") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) + 
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))

ggsave(dpi = "retina",plot = Employ_by_sector_graph, "Employ_by_sector.png", type = "cairo-png") #Saving Image of Employment by sector graph
ggsave(dpi = "retina",plot = Employ_by_sector_percent_graph, "Employ_by_sector_percent.png", type = "cairo-png") #Saving Image of employment by sector % graph




p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

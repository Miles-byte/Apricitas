pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)


Employ_by_sector <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Interpersonal%20Economy%20-%20Automation%20and%20You/employment-by-economic-sector.csv") #importing data on jobs by sector from OWID

GDP_Goods_Services <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Interpersonal%20Economy%20-%20Automation%20and%20You/Services_Manufacturing_GDP.csv")

Labor_Capital_Share <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/The%20Interpersonal%20Economy%20-%20Automation%20and%20You/LaborCapital.csv")

CES_Data <-import("C:/Users/Joseph/Documents/SubStack/The Interpersonal Economy/CES_data.txt")

CES_Data$period <- gsub("M","", CES_Data$period) #removing "M" from period to contruct month
CES_Data$Date <- paste(CES_Data$year, "-", CES_Data$period, "-","01") #combining year and month with 1 day to form date
CES_Data$Date <- as.Date(CES_Data$Date,"%Y - %m - %d") #make date

CES_supersector <- CES_Data[CES_Data$series_id %in% c("CES0000000001","CES3000000001","CES6000000001","CES6500000001","CES7000000001","CES9000000001"), ]
CES_supersector <- pivot_wider(CES_supersector, names_from = series_id, values_from = value) #widening data by series
CES_supersector$CES3000000001 <- CES_supersector$CES3000000001/CES_supersector$CES0000000001 #taking percent of total nonfarm employment of CES categories
CES_supersector$CES6000000001 <- CES_supersector$CES6000000001/CES_supersector$CES0000000001
CES_supersector$CES6500000001 <- CES_supersector$CES6500000001/CES_supersector$CES0000000001
CES_supersector$CES7000000001 <- CES_supersector$CES7000000001/CES_supersector$CES0000000001
CES_supersector$CES9000000001 <- CES_supersector$CES9000000001/CES_supersector$CES0000000001

CES_Data_Sector <- pivot_wider(CES_Data, names_from = series_id, values_from = value)
CES_Data_Sector <- CES_Data_Sector[,-1:-4] #removing non-numeric columns
CES_Data_Sector <- select(CES_Data_Sector, ends_with("01")) #only taking data that ends with 01, which indicates "employment level"
CES_Data_Sector <- sapply(CES_Data_Sector, function(CES_Data_Sector) max(CES_Data_Sector, na.rm=TRUE) - min(CES_Data_Sector, na.rm=TRUE)) #calculating the range of all columns
sort(CES_Data_Sector, decreasing = TRUE)# identifying the columns with the largest range as those with the largest change to employment levels

#CES6562000001 - Health Care #CES7072000001 - Accommodation and Food Services #CES9093161101 - Local Government Education #CES6054000001 - Professional and Technical Services 6562100001 - Ambulatory Health Care Services 9093200001 - Local Government ex education 6056000001 - Administrative and Waste Services

CES_Data_Sector <- CES_Data[CES_Data$series_id %in% c("CES6562000001","CES7072000001","CES9093161101","CES6054000001","CES9093200001","CES6056000001"), ] #taking categories with the largest change in employment levels
CES_Data_Sector <- pivot_wider(CES_Data_Sector, names_from = series_id, values_from = value) #widening data by series


Labor_Capital_Share$DATE <- as.Date(Labor_Capital_Share$DATE,"%m/%d/%Y")
Labor_Capital_Share <- pivot_longer(Labor_Capital_Share, cols = `Labor`:`Capital`, names_to = "Factor", values_to = "Percent") #gathering data from labor and capital into one colum

 
GDP_Goods_Services$DATE <- as.Date(GDP_Goods_Services$DATE,"%m/%d/%Y")

colnames(GDP_Goods_Services) <- c("DATE","Services Production","Goods Production","Services Consumption","Goods Consumption")
GDP_Goods_Services <- pivot_longer(GDP_Goods_Services, cols = `Services Production`:`Goods Consumption`, names_to = "Sector", values_to = "Percent") #gathering data from sectoral employment to one column


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

ggplot() + 
  geom_line(data = GDP_Goods_Services, aes(x=DATE, y = Services_Produced, color = "Services Production"),size = 1.25,stat = "density", alpha = 1) + 
  geom_line(data = GDP_Goods_Services, aes(x=DATE, y = Manufacturing_Produced, color = "Goods Production"),size = 1.25, alpha = 1) + 
  geom_line(data = GDP_Goods_Services, aes(x=DATE, y = Services_Consumed, color = "Services Consumption"),size = 1.25, alpha = 0.5) + 
  geom_line(data = GDP_Goods_Services, aes(x=DATE, y = Manufacturing_Consumed, color = "Goods Consumption"),size = 1.25, alpha = 0.5) + 
  xlab("Year") +
  #scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), breaks = c(0,0.25,0.5,.75,1), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("Employment by Sector, % of Total Employment") +
  ggtitle("At Your Service") +
  labs(caption = "Graph created by @JosephPolitano using OWID data based on Herrendorf et al. (2014)",subtitle = "The Service Sector Represents the Vast Majority of Employment in America") +
  theme_apricitas + theme(legend.position = c(.85,.6)) +
  #scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#EE6055","#EE6055"), guide=guide_legend(override.aes=list(alpha = (1,0.5,1,0.5))))
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#EE6055","#EE6055"), guide=guide_legend(override.aes=list(alpha=c(1,.5,1,.5))))
  
GDP_Goods_Services_graph <- ggplot() + 
  geom_line(data = GDP_Goods_Services, aes(x=DATE, y = Percent/100, color = Sector, alpha = Sector),size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.15,.5), breaks = c(0.2,0.3,.4,.5), expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) + #expand = 0,0 sets the margins for the chart between the axis and data points to 0
  ylab("Consumption and Production by Sector, % of GDP") +
  ggtitle("At Your Service") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data",subtitle = "The Service Sector Has Grown to Exceed Manufacturing Sector's Share of GDP") +
  theme_apricitas + theme(legend.position = c(.85,.6)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#EE6055","#EE6055")) +#, guide=guide_legend(override.aes=list(alpha=c(1,.5,1,.5))))+
  scale_alpha_manual(name= NULL,values = c(1,.5,1,.5))

Labor_Capital_Share_Graph <- ggplot() + 
  geom_area(data = Labor_Capital_Share, aes(x=DATE, y = Percent, fill = Factor),color = NA, size = 0) +
  xlab("Year") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1), breaks = c(.0,.5,.25,.75,1), expand = c(0,0)) +
  scale_x_date(expand = c(0,0)) +
  ylab("Share of National Income, %") +
  ggtitle("Breaking Down Shares of National Income") +
  labs(caption = "Graph created by @JosephPolitano using data from Penn World Table",subtitle = "Labor's Share of National Income has been Decreasing, but Only by About 3%") +
  theme_apricitas + theme(legend.position = "right") +
  scale_fill_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"))

CES_supersector_graph <- ggplot() + 
  geom_line(data = CES_supersector, aes(x=Date, y = CES3000000001, color = "Manufacturing"),size = 1.25) +
  geom_line(data = CES_supersector, aes(x=Date, y = CES6000000001, color = "Professional and Business Services"),size = 1.25) +
  geom_line(data = CES_supersector, aes(x=Date, y = CES6500000001, color = "Education and Health Services (ex Public Schools)"),size = 1.25) +
  geom_line(data = CES_supersector, aes(x=Date, y = CES7000000001, color = "Leisure and Hospitality"),size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(),limits = c(.0,.4), breaks = c(0.1,0.2,.3,.4), expand = c(0,0)) +
  ylab("Number of Employees, % of Non-Farm Employment") +
  ggtitle("The Changing World of Work") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Various Service Sectors Have Exceeded Manufacturing's Share of Employment") +
  theme_apricitas + theme(legend.position = c(.65,.8)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"))
  
CES_Data_Sector_graph <- ggplot() + 
  geom_line(data = CES_Data_Sector, aes(x=Date, y = CES6562000001, color = "Health Care"),size = 1.25) +
  geom_line(data = CES_Data_Sector, aes(x=Date, y = CES7072000001, color = "Accommodation and Food Services"),size = 1.25) +
  geom_line(data = CES_Data_Sector, aes(x=Date, y = CES6056000001, color = "Administrative and Waste Services"),size = 1.25) +
  geom_line(data = CES_Data_Sector, aes(x=Date, y = CES6054000001, color = "Professional and Technical Services"),size = 1.25) +
  xlab("Date") +
  #scale_y_continuous(labels = scales::percent_format(),limits = c(.0,.4), breaks = c(0.1,0.2,.3,.4), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2021-08-01"))) +
  ylab("Number of Employees, thousands") +
  ggtitle("The Changing World of Work") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "The Fastest-Growing Employment Categories Have Come From the Service Sector") +
  theme_apricitas + theme(legend.position = c(.3,.8)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"))


ggsave(dpi = "retina",plot = Employ_by_sector_graph, "Employ_by_sector.png", type = "cairo-png") #Saving Image of Employment by sector graph
ggsave(dpi = "retina",plot = Employ_by_sector_percent_graph, "Employ_by_sector_percent.png", type = "cairo-png") #Saving Image of employment by sector % graph
ggsave(dpi = "retina",plot = GDP_Goods_Services_graph, "GDP_Goods_Services.png", type = "cairo-png") #Saving Image of Goods and Services as a share of GDP graph
ggsave(dpi = "retina",plot = Labor_Capital_Share_Graph, "Labor_Capital_Share.png", type = "cairo-png") #Saving Image of labor and capital share of income graph
ggsave(dpi = "retina",plot = CES_supersector_graph, "CES_Supersector.png", type = "cairo-png") #Saving Image of employment by supersector % graph
ggsave(dpi = "retina",plot = CES_Data_Sector_graph, "CES_Data_Sector.png", type = "cairo-png") #Saving Image of employment by supersector % graph


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

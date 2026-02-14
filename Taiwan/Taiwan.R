library(readxl)
library(dplyr)
library(ggplot2)

# Read the data
url <- "https://ws.dgbas.gov.tw/001/Upload/464/relfile/10320/2632/webdata2008yoy(057).xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Read the data
TAIWAN_GDP_BULK <- read_excel(temp_file, sheet = "Growth rates")

TAIWAN_GDP_CLEAN <- TAIWAN_GDP_BULK %>%
  select(3, 43) %>%
  rename(quarter = 1, gdp_yoy = 2) %>%
  filter(!is.na(gdp_yoy), !is.na(quarter)) %>%
  mutate(gdp_yoy = as.numeric(gdp_yoy)) %>%
  filter(grepl("Q", quarter) & !grepl("f", quarter, ignore.case = TRUE)) %>%
  mutate(quarter = lubridate::yq(quarter))

# Create chart
TAIWAN_GDP_YOY_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_GDP_CLEAN, quarter >= as.Date("2014-01-01")), aes(x=quarter,y= gdp_yoy/100,color= "Taiwanese Real GDP, Year-on-Year Growth"), size = 1.25)+ 
  #geom_line(data=USNGDI, aes(x=date,y= value/100,color= "NGDI Growth"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Year-on-Year Growth") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-.10,-0.05,0,0.05,0.1,0.15), limits = c(-.05,.15), expand = c(0,0)) +
  ggtitle("Taiwan's GDP is Soaring Amidst the AI Boom") +
  labs(caption = "Graph created by @JosephPolitano using ROC National Statistics data", subtitle = "Driven by Ravenous Demand for AI Chips, Taiwanese GDP Has Grown More than 12% Over the Last Year") +
  theme_apricitas + theme(legend.position = c(.42,.85)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = -.05-(.3*.20), ymax = -.05) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TAIWAN_GDP_YOY_GRAPH, "Taiwan GDP Yoy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

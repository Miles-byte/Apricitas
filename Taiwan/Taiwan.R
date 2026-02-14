pacman::p_load(readr,readxl,RcppRoll,sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


# Read the data
url <- "https://ws.dgbas.gov.tw/001/Upload/464/relfile/10320/2632/webdata2008yoy(057).xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Read the data
TAIWAN_GDP_BULK_YOY <- read_excel(temp_file, sheet = "Growth rates")

TAIWAN_GDP_CLEAN_YOY <- TAIWAN_GDP_BULK_YOY %>%
  select(3, 43) %>%
  rename(quarter = 1, gdp_yoy = 2) %>%
  filter(!is.na(gdp_yoy), !is.na(quarter)) %>%
  mutate(gdp_yoy = as.numeric(gdp_yoy)) %>%
  filter(grepl("Q", quarter) & !grepl("f", quarter, ignore.case = TRUE)) %>%
  mutate(quarter = lubridate::yq(quarter))

TAIWAN_GDP_CLEAN_YOY <- TAIWAN_GDP_BULK_YOY %>%
  setNames({
    data_start <- which(apply(TAIWAN_GDP_BULK_YOY, 1, function(r) {
      any(grepl("^\\d{4}", r), na.rm = TRUE)
    }))[1]
    
    hdr <- TAIWAN_GDP_BULK_YOY[1:(data_start - 1), ]
    
    sapply(seq_len(ncol(hdr)), function(j) {
      vals <- as.character(hdr[[j]])
      vals <- vals[!is.na(vals) & vals != ""]
      eng <- vals[grepl("[a-zA-Z]", vals) & !grepl("[\u4e00-\u9fff]", vals)]
      if (length(eng) > 0) paste(eng, collapse = " - ") else paste0("V", j)
    }) |> make.unique()
  })  %>%
  select(-1,-V2) %>%
  mutate(across(-1, ~ suppressWarnings(as.numeric(.))))  %>%
  filter(if_any(-1, ~ !is.na(.))) %>%
  rename(date = 1) %>%
  filter(grepl("Q", date) & !grepl("f", date, ignore.case = TRUE)) %>%
  mutate(date = lubridate::yq(date))

# Create chart
TAIWAN_GDP_YOY_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_GDP_CLEAN_YOY, date >= as.Date("2014-01-01")), aes(x=date,y= GDP/100,color= "Taiwanese Real GDP, Year-on-Year Growth"), size = 1.25) + 
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

url <- "https://ws.dgbas.gov.tw/001/Upload/464/relfile/10320/2632/webdata2008saar(057).xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

TAIWAN_GDP_BULK_IND <- read_excel(temp_file, sheet = "GDP(Chained (2021) Dollars)SA")

TAIWAN_GDP_CLEAN_IND <- TAIWAN_GDP_BULK_IND %>%
  setNames({
    data_start <- which(apply(TAIWAN_GDP_BULK_IND, 1, function(r) {
      any(grepl("^\\d{4}", r), na.rm = TRUE)
    }))[1]
    
    hdr <- TAIWAN_GDP_BULK_IND[1:(data_start - 1), ]
    
    sapply(seq_len(ncol(hdr)), function(j) {
      vals <- as.character(hdr[[j]])
      vals <- vals[!is.na(vals) & vals != ""]
      eng <- vals[grepl("[a-zA-Z]", vals) & !grepl("[\u4e00-\u9fff]", vals)]
      if (length(eng) > 0) paste(eng, collapse = " - ") else paste0("V", j)
    }) |> make.unique()
  })  %>%
  select(-1,-V2) %>%
  mutate(across(-1, ~ suppressWarnings(as.numeric(.))))  %>%
  filter(if_any(-1, ~ !is.na(.))) %>%
  rename(date = 1) %>%
  filter(grepl("Q", date) & !grepl("f", date, ignore.case = TRUE)) %>%
  mutate(date = lubridate::yq(date))

TAIWAN_GDP_FOREIGN_DEMAND_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_GDP_CLEAN_IND, date >= as.Date("2014-01-01")), aes(x=date,y= `Net Foreign Demand`/1000000,color= "Taiwanese Real Net Exports"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("2021 New Taiwanese Dollars") +
  scale_y_continuous(labels = scales::number_format(prefix = "NT$",accuracy = .1, suffix = "T"), breaks = c(0,.4,.8,1.2), limits = c(0,1.5), expand = c(0,0)) +
  ggtitle("Taiwanese Exports are Skyrocketing") +
  labs(caption = "Graph created by @JosephPolitano using ROC National Statistics data", subtitle = "Real Net Exports From Taiwan Have Hit Record Highs As the Country's Chipmakers Supply the AI Boom") +
  theme_apricitas + theme(legend.position = c(.42,.85)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*1.5), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TAIWAN_GDP_FOREIGN_DEMAND_GRAPH, "Taiwan GDP Foreign Demand Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

TAIWAN_GDP_FIXED_INVESTMENT_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_GDP_CLEAN_IND, date >= as.Date("2014-01-01")), aes(x=date,y= `Gross Capital Formation`/1000000,color= "Taiwanese Real Gross Capital Formation"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("2021 New Taiwanese Dollars") +
  scale_y_continuous(labels = scales::number_format(prefix = "NT$",accuracy = .1, suffix = "T"), breaks = c(0,.4,.8,1.2,1.6,2), limits = c(0,2), expand = c(0,0)) +
  ggtitle("Taiwanese Investment is High") +
  labs(caption = "Graph created by @JosephPolitano using ROC National Statistics data", subtitle = "Taiwanese Fixed Investment Remains Near All-Time Highs") +
  theme_apricitas + theme(legend.position = c(.40,.85)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(today()-as.Date("2014-01-01"))), ymin = 0-(.3*2), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "panel")

ggsave(dpi = "retina",plot = TAIWAN_GDP_FIXED_INVESTMENT_GRAPH, "Taiwan GDP Fixed Investment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



TSMC_STOCK <- tq_get("TSM",from = "2019-01-01")

TSMC_STOCK_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=TSMC_STOCK, aes(x=date,y= close,color= "TSMC Share Price"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Dollar ") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), breaks = c(0,100,200,300,400), limits = c(0,400), expand = c(0,0)) +
  ggtitle("TSMC's Share Price Boom") +
  labs(caption = "Graph created by @JosephPolitano using ROC National Statistics data", subtitle = "Driven by Ravenous Demand for AI Chips, TSMC's Share Price Has Skyrocketed") +
  theme_apricitas + theme(legend.position = c(.42,.85)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*400), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TSMC_STOCK_GRAPH, "TSMC Stock Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

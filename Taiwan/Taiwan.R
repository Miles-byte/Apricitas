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


url <- "https://ws.dgbas.gov.tw/001/Upload/464/relfile/10320/2632/webdata2008yoy(057).xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

TAIWAN_GFCF_BULK <- read_excel(
  temp_file,
  sheet = "GFCF(chained dollars)",
  col_names = FALSE,
  skip = 7
)


TAIWAN_GFCF_CLEAN <- TAIWAN_GFCF_BULK %>%
  select(
    date            = 3,
    GFCF_Total      = 4,
    Construction    = 9,
    Transport       = 14,
    Machinery       = 19,
    IPP             = 24
  ) %>%
  mutate(across(-date, ~ suppressWarnings(as.numeric(.)))) %>%
  filter(grepl("Q", as.character(date))) %>%
  mutate(
    date = lubridate::yq(str_extract(as.character(date), "\\d{4}Q\\d"))
  ) %>%
  filter(!is.na(date)) %>%
  mutate(across(-date, ~ . / .[date == as.Date("2019-10-01")] * 100)) %>%
  drop_na()

TAIWAN_GFCF_COMPONENTS_GRAPH <- ggplot() +
  geom_line(data = filter(TAIWAN_GFCF_CLEAN, date >= as.Date("2016-01-01")),aes(x = date, y = Construction, color = "Construction"),size = 1.25) +
  geom_line(data = filter(TAIWAN_GFCF_CLEAN, date >= as.Date("2016-01-01")),aes(x = date, y = Machinery,color = "Machinery & Equipment"),size = 1.25) +
  geom_line(data = filter(TAIWAN_GFCF_CLEAN, date >= as.Date("2016-01-01")),aes(x = date, y = GFCF_Total,color = "Total Gross Fixed Capital Formation"),size = 2.25) +
  xlab("Date") +
  ylab("Index (Q4 2019 = 100)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = ""), breaks = c(50, 75, 100, 125, 150, 175), limits = c(45, 155), expand = c(0, 0)) +
  ggtitle("Taiwanese Fixed Investment") +
  labs(caption = "Graph created by @JosephPolitano using ROC National Statistics data",subtitle = "Gross Fixed Investment in Taiwan Has Grown Significantly, Especially Equipment & Machinery") +
  theme_apricitas + theme(legend.position = c(.38,.85)) + theme(legend.spacing.y = unit(0, "cm")) +
  scale_color_manual(name = "Real GFCF Components, Indexed to Q4 2019 = 100, NSA", values = c("#FFE98F", "#00A99D", "#EE6055", "#9A348E", "#A7ACD9"), breaks = c("Total Gross Fixed Capital Formation","Machinery & Equipment","Construction")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01") - (.1861 * (today() - as.Date("2016-01-01"))), xmax = as.Date("2016-01-01") - (0.049 * (today() - as.Date("2016-01-01"))), ymin = 45 - (.3 * 110), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina", plot = TAIWAN_GFCF_COMPONENTS_GRAPH, "Taiwan GFCF Components Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


TAIWAN_GDP_CONTRIB <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/Taiwan_GDP_Contributions.csv") %>%
  mutate(date = yq(Period)) %>%
  select(-Period,-GDP,-Domestic.Demand,-Exports,-Imports) %>%
  transmute(date,Consumption = `Private.Consumption`+`Government.Consumption`, `Gross Capital Formation` = `Gross.Capital.Formation`, `Net Exports` = `Net.Exports`) %>%
  pivot_longer(-date)

TAIWAN_GDP_QUARTERLY <- TAIWAN_GDP_CONTRIB %>%
  group_by(date) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(name = "Total Quarterly GDP Growth")

TAIWAN_GDP_CONTRIB_Graph <- ggplot(filter(TAIWAN_GDP_CONTRIB,date>= as.Date("2022-01-01")), aes(fill=name, x=date, y=value/100)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data = filter(TAIWAN_GDP_QUARTERLY, date>= as.Date("2022-01-01")), aes(x=date, y = value/100, color = "Total Quarterly GDP Growth"), size = 2) +
  geom_point(data = filter(TAIWAN_GDP_QUARTERLY,date>=as.Date("2022-01-01")), aes(x=date, y = value/100), size = 3, fill ="black", color = "white", shape = 23) +
  #guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.12,0.32), breaks = c(-.1,0,.1,.2,.3), expand = c(0,0)) +
  ylab("Contributions, Percent, Seasonally Adjusted at Annual Rates") +
  ggtitle("Taiwanese Real GDP Growth & Contributions") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = NULL) +
  theme_apricitas + theme(legend.position = c(0.21,0.95), legend.margin=margin(0,0,-80,0)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#A7ACD9","#00A99D","#FFE98F","black"), breaks = c("Consumption","Gross Capital Formation","Net Exports")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  #theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = -0.12-(.3*.44), ymax = -0.12) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TAIWAN_GDP_CONTRIB_Graph, "Taiwan GDP Contribu Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


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



TAIWAN_EXPORT_ORDERS_ELEC_COUNTRY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_EXPORT_ORDERS_ELECTRONICS_COUNTRY.csv") %>%
  select(-date,-X) %>%
  drop_na() %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq.Date(from = as.Date("2016-01-01"), by = "month", length.out = nrow(.)))

TAIWAN_EXPORT_ORDERS_ICT_COUNTRY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Chip%20War/TWN_EXPORT_ORDERS_ICT_COUNTRY.csv") %>%
  select(-date,-X) %>%
  drop_na() %>%
  ts(., start = c(2016,1), frequency = 12) %>%
  seas(x11 = "") %>%
  final() %>%
  as.data.frame(value = melt(.)) %>%
  mutate(date = seq.Date(from = as.Date("2016-01-01"), by = "month", length.out = nrow(.)))


TAIWAN_EXPORT_ORDERS_COUNTRY <- merge(TAIWAN_EXPORT_ORDERS_ELEC_COUNTRY,TAIWAN_EXPORT_ORDERS_ICT_COUNTRY, by = "date") %>%
  transmute(date,USA = USA.x+USA.y, Japan = Japan.x+Japan.y, China_HK = China_HK.x+China_HK.y,ASEAN = ASEAN.x+ASEAN.y,Europe = Europe.x+Europe.y,Others = Others.x+Others.y)

TAIWAN_EXPORT_ORDERS_COUNTRY_Graph <- ggplot() + #plotting integrated circuits exports
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= (Others+Japan)/1000,color= "Others"), size = 1.25) +
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= Europe/1000,color= "Europe (Including Russia)"), size = 1.25) + 
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= ASEAN/1000,color= "ASEAN"), size = 1.25) + 
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= USA/1000,color= "USA"), size = 1.25) + 
  geom_line(data=TAIWAN_EXPORT_ORDERS_COUNTRY, aes(x=date,y= China_HK/1000,color= "China and HK"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B",prefix = "$", accuracy = 1),limits = c(0,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Dollars, Monthly") +
  ggtitle("Taiwanese Computer & Electronics Export Orders") +
  labs(caption = "Graph created by @JosephPolitano using MOEA data seasonally adjusted usint X-13ARIMA",subtitle = "Taiwanese Exports orders are Skyrocketing, Especially to the USA & ASEAN") +
  theme_apricitas + theme(legend.position = c(.3,.775)) +
  scale_color_manual(name= "Computer & Electronics Export Orders by Region", values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("USA","China and HK","ASEAN","Europe (Including Russia)","Others")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*25), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot") +
  theme(plot.title = element_text(size = 25))

ggsave(dpi = "retina",plot = TAIWAN_EXPORT_ORDERS_COUNTRY_Graph, "Taiwan Export Orders County Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


#Data from here:
#https://www.esist.org.tw/a0303/02/en/newest/monthly/?tab=Electricity

TAIWAN_ELEC_CONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/Taiwan_elec_consumption.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(elec = gsub(",","",elec)) %>%
  mutate(elec = as.numeric(elec))

TAIWAN_ELEC_CONS_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_ELEC_CONS, date >= as.Date("2006-01-01")), aes(x=date,y= `elec`/1000,color= "Taiwanese Electricity Consumption\nby Electronics & Electrical Manufacturing"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Annual Electricity Consumption, TWh") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "TWh"), breaks = c(0,20,40,60), limits = c(0,75), expand = c(0,0)) +
  ggtitle("Taiwanese Chipmakers' Power Consumption") +
  labs(caption = "Graph created by @JosephPolitano using MOEA Energy data", subtitle = "Electronics Manufacturers, Mostly TSMC, Consume Roughly a Quarter of Taiwan's Electricity") +
  theme_apricitas + theme(legend.position = c(.40,.875)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*75), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TAIWAN_ELEC_CONS_GRAPH, "Taiwan Elec Cons Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

#Pulled From Here:

TAIWAN_EXPORT_MONTHLY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/Taiwan_Exports_Monthly.csv") %>%
  setNames(c("category","date","country","value")) %>%
  mutate(date = ym(str_sub(date, 1, 7))) %>%
  mutate(country_group = case_when(
    country == "United States" ~ "USA",
    country == "China"         ~ "China and HK",
    country == "Hong Kong"     ~ "China and HK",
    country == "Macao"         ~ "China and HK",
    TRUE                       ~ "Other")) %>%
  summarise(value = sum(value, na.rm = TRUE), .by = c(category, date, country_group)) %>%
  mutate(country_group = factor(country_group,levels = rev(c("USA","China and HK","Other"))))


TAIWAN_EXPORT_MONTHLY_Graph <- ggplot(filter(TAIWAN_EXPORT_MONTHLY,date>= as.Date("2016-01-01")), aes(fill=country_group, x=date, y=value*12/1000000)) + 
  geom_bar(position="stack", stat="identity", size = 0, color = NA, width = 32) + #putting color to NA gets rid of borders
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  #guides(fill = guide_legend(override.aes = list(shape = NA)), color = "none") +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,800), breaks = c(0,200,400,600,800), expand = c(0,0)) +
  ylab("Exports, Annualized, Billions of Dollars") +
  ggtitle("Taiwanese Exports by Destination") +
  labs(caption = "Graph created by @JosephPolitano using Taiwan Customs data", subtitle = "Taiwanese Exports are Skyrocketing, Especially to the United States") +
  theme_apricitas + theme(legend.position = c(0.21,0.95), legend.margin=margin(0,0,-80,0)) +
  #scale_color_manual(name = NULL, values = "black") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","black"), breaks = c("USA","China and HK","Other")) +
  #theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13), plot.title = element_text(size = 26)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*800), ymax = 0) +
  coord_cartesian(clip = "off") 

ggsave(dpi = "retina",plot = TAIWAN_EXPORT_MONTHLY_Graph, "Taiwan Export Monthly Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing



TAIWAN_ELEC_CONS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/Taiwan_elec_consumption.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(elec = gsub(",","",elec)) %>%
  mutate(elec = as.numeric(elec))

TAIWAN_ELEC_CONS_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_ELEC_CONS, date >= as.Date("2006-01-01")), aes(x=date,y= `elec`/1000,color= "Taiwanese Electricity Consumption\nby Electronics & Electrical Manufacturing"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Annual Electricity Consumption, TWh") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "TWh"), breaks = c(0,20,40,60), limits = c(0,75), expand = c(0,0)) +
  ggtitle("Taiwanese Chipmakers' Power Consumption") +
  labs(caption = "Graph created by @JosephPolitano using MOEA Energy data", subtitle = "Electronics Manufacturers, Mostly TSMC, Consume Roughly a Quarter of Taiwan's Electricity") +
  theme_apricitas + theme(legend.position = c(.40,.875)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*75), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TAIWAN_ELEC_CONS_GRAPH, "Taiwan Elec Cons Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



TAIWAN_INDPRO_SA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/TAIWAN_INDPRO_SA.csv") %>%
  mutate(date = as.Date(date)) %>%
  select(-X) %>%
  setNames(c("date","Manufacturing","Electronics & Information Industry","Electronic Parts & Components","Computers, Electronic, & Optical Products")) %>%
  drop_na()

TAIWAN_INDPRO_SA_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_INDPRO_SA, date >= as.Date("2006-01-01")), aes(x=date,y= Manufacturing,color= "Total Manufacturing"), size = 1.25) + 
  geom_line(data=filter(TAIWAN_INDPRO_SA, date >= as.Date("2006-01-01")), aes(x=date,y= `Electronics & Information Industry`,color= "Electronics & Information Manufacturing"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Index, 2021 = 100") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,25,50,75,100,125,150,175), limits = c(0,175), expand = c(0,0)) +
  ggtitle("Taiwanese Industrial Production") +
  labs(caption = "Graph created by @JosephPolitano using MOEA Energy data", subtitle = "Taiwanese Industrial Production is at a Record High, Driven by Rising Chip Manufacturing") +
  theme_apricitas + theme(legend.position = c(.40,.875)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Total Manufacturing","Electronics & Information Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*175), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_INDPRO_SA_GRAPH, "Taiwan Indpro SA Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


TAIWAN_INDPRO_NSA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/TAIWAN_INDPRO_NSA.csv") %>%
  mutate(date = as.Date(Item)) %>%
  select(-X) %>%
  mutate(across(where(is.character) & !c(Item), ~as.numeric(gsub(",", "", .))))
  
TAIWAN_INDPRO_NSA_CHIPS_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Integrated.Circuits`,color= "Semiconductors (Integrated Circuits)"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Discrete.Devices`,color= "Discrete Devices"), size = 1.25) + 
  geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Packaging.and.Testing.of.Semi.conductors`,color= "Packaging & Testing of Semiconductors"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Electronic.Passive.Devices`,color= "Electronic Passive Devices"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Bare.Printed.Circuit.Boards`,color= "Bare Printed Circuit Boards"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Panel.and.Components`,color= "Panel and Components"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Printed.Circuit.Assembly`,color= "Printed Circuit Assembly"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Index, 2021 = 100, NSA") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1), breaks = c(0,25,50,75,100,125,150,175), limits = c(0,175), expand = c(0,0)) +
  ggtitle("Taiwanese Chip Industrial Production") +
  labs(caption = "Graph created by @JosephPolitano using MOEA Energy data", subtitle = "Taiwanese Industrial Production is at a Record High, Driven by Rising Chip Manufacturing") +
  theme_apricitas + theme(legend.position = c(.40,.875)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"),breaks = c("Semiconductors (Integrated Circuits)","Packaging & Testing of Semiconductors")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*175), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_INDPRO_NSA_CHIPS_GRAPH, "Taiwan Indpro NSA Chips Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE




TAIWAN_INDPRO_NSA_COMPUTER_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Computers`,color= "Taiwan Industrial Production of Computers"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Monitors.and.Terminals`,color= "Monitors and Terminals"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Other.Computer.Peripheral.Equipment`,color= "Other Computer Peripherals"), size = 1.25) + 
  
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Telephones.and.Cellular.Phones`,color= "Telephones and "), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Other.Communication.Equipment`,color= "Other Communication Equipment"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Audio.and.Video.Equipment`,color= "Audio and Video"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Magnetic.and.Optical.Media`,color= "Magnetic and Optical Media"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Measuring..Navigating.and.Control.Equipment`,color= "Navigating and Control Equipment"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Watches.and.Clocks`,color= "Watches and Clocks"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Irradiation.and.Electromedical.Equipment`,color= "Irradiation"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Cameras`,color= "Cameras"), size = 1.25) + 
  #geom_line(data=filter(TAIWAN_INDPRO_NSA, date >= as.Date("2006-01-01")), aes(x=date,y= `Manufacture.of.Other.Optical.Instruments.and.Equipment`,color= "Optical"), size = 1.25) + 
  
  
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Index, 2021 = 100, NSA") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1), breaks = c(0,250,500,750,1000,1250), limits = c(0,1250), expand = c(0,0)) +
  ggtitle("Taiwanese Computer Production") +
  labs(caption = "Graph created by @JosephPolitano using MOEA data", subtitle = "Taiwanese Computer Production is at Record Highs as TSMC Integrates Chips into Servers") +
  theme_apricitas + theme(legend.position = c(.30,.875)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*1250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_INDPRO_NSA_COMPUTER_GRAPH, "Taiwan Indpro NSA Computer Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


TSMC_CAPX <- data.frame(
  Year = 2018:2026,
  TSMC = c(10.5, 14.9, 17.2, 30.4, 36.3, 30.5, 29.8, 40.9, 54)
) %>%
  mutate(
    Year_Date = as.Date(paste0(Year, "-01-01")),
    is_forecast = Year == 2026
  )

TSMC_CAPEX_GRAPH <- ggplot() +
  annotate("rect", xmin = as.Date("2025-06-01"), xmax = as.Date("2027-01-01"),
           ymin = -Inf, ymax = Inf, fill = "#EE6055", color = NA, alpha = 0.3) +
  annotate("text", label = "2026 Guidance\n", x = as.Date("2024-06-01"), y = 54,
           color = "#EE6055", size = 4.5, alpha = 0.6, lineheight = 0.8) +
  geom_line(data = filter(TSMC_CAPX, !is_forecast),
            aes(x = Year_Date, y = TSMC), color = "#FFE98F", size = 1.25) +
  geom_line(data = filter(TSMC_CAPX, Year >= 2025),
            aes(x = Year_Date, y = TSMC), color = "#FFE98F",
            size = 0.75, alpha = 0.5, linetype = "dashed") +
  geom_point(data = filter(TSMC_CAPX, is_forecast),
             aes(x = Year_Date, y = TSMC), color = "#FFE98F", size = 2.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B"),
                     limits = c(0, 65), expand = c(0, 0)) +
  xlab("Year") +
  ylab("Capital Expenditure, $B") +
  ggtitle("TSMC Capital Expenditures Hit Record Highs") +
  labs(caption = "Graph created by @JosephPolitano using TSMC IR data",
       subtitle = "The Taiwanese Chipmaker is Ramping up Spending to Meet Surging AI Demand") +
  theme_apricitas +
  theme(legend.position = "none") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01") - (.1861 * (today() - as.Date("2018-01-01"))), xmax = as.Date("2018-01-01") - (0.049 * (today() - as.Date("2018-01-01"))), ymin = 0 - (.3 * 65), ymax = 0) +
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina", plot = TSMC_CAPEX_GRAPH, "TSMC Capex Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TWNUSDEXRT <- fredr(series_id = "DEXTAUS",observation_start = as.Date("2018-01-01")) %>%
  mutate(value = 1/value) %>%
  select(date,value) %>%
  drop_na()

TWNUSDEXRT_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=filter(TWNUSDEXRT, date >= as.Date("2023-01-01")), aes(x=date,y= value,color= "Value of Taiwanese Dollar in USD"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Taiwanese Dollar to USD") +
  scale_y_continuous(labels = scales::number_format(accuracy = .001), breaks = c(0.029,0.03,0.031,0.032,0.033,0.034,0.035), limits = c(0.029,0.035), expand = c(0,0)) +
  ggtitle("Taiwanese/US Dollar Exchange Rate") +
  labs(caption = "Graph created by @JosephPolitano using MOEA Energy data", subtitle = "The Taiwanese Dollar Has Barely Appreciated Amidst the AI Boom, Giving Up Most of its Post-Liberation Day Gains") +
  theme_apricitas + theme(legend.position = c(.40,.875)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = 0.029-(.3*0.006), ymax = 0.029) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TWNUSDEXRT_GRAPH, "Taiwanese Dollar USD Exchange Rate Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE



TAIWAN_EMPLOYMENT <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/EMP_GROWTH_DATA.csv") %>%
  mutate(electronic_parts_and_components = as.numeric(gsub(",","",electronic_parts_and_components)),
         computers_electronic_and_optical_products = as.numeric(gsub(",","",computers_electronic_and_optical_products))) %>%
  mutate(date = as.Date(date))

TAIWAN_EMPLOYMENT_GRAPH <- ggplot() + #plotting Taiwan GDP data
  geom_line(data=TAIWAN_EMPLOYMENT, aes(x=date,y= electronic_parts_and_components/1000,color= "Electronic Parts & Components Manufacturing"), size = 1.25) + 
  geom_line(data=TAIWAN_EMPLOYMENT, aes(x=date,y= computers_electronic_and_optical_products/1000,color= "Computer & Electronic Product Manufacturing"), size = 1.25) + 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  xlab("Date") +
  ylab("Job Growth, Year-on-Year") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-20,-10,0,10,20,30), limits = c(-20,30), expand = c(0,0)) +
  ggtitle("Taiwan Year-on-Year Chipmaking Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using MOEA data", subtitle = "Hiring in Taiwanese Electronics Manufacturers Have Rebounded Amidst the AI Boom") +
  theme_apricitas + theme(legend.position = c(.35,.90)) + theme(legend.spacing.y = unit(0,"cm")) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -20-(.3*50), ymax = -20) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = TAIWAN_EMPLOYMENT_GRAPH, "Taiwanese Employment Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


TAIWAN_FINANCIAL_ACCTS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Taiwan/TAIWAN_FINANCIAL_ACCOUNTS.csv") %>%
  mutate(date = as.Date(date, "%d/%m/%Y")) %>%
  setNames(c("date","Foreign Direct Investment","Debt Securities","Equity & Investment Fund Shares","Deposits, Loans, Trade Credit, & Other Investment","Financial Derivatives","Reserve Assets")) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Foreign Direct Investment","Reserve Assets","Equity & Investment Fund Shares","Deposits, Loans, Trade Credit, & Other Investment","Debt Securities","Financial Derivatives"))))


TAIWAN_FINANCIAL_ACCTS_GRAPH <- ggplot(data = TAIWAN_FINANCIAL_ACCTS, aes(x = date, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Annual Net Outflows, Billions") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(-50,0,50,100,150,200), limits = c(-50,200), expand = c(0,0)) +
  ggtitle("Taiwanese Financial Account Net Outflows") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Flipside of Taiwan's Large Trade Surplus are Large Financial Outflows, Led by FDI & Lending") +
  theme_apricitas + theme(legend.position = c(.35,.82), legend.key.size = unit(0.5,"cm"), legend.spacing.y = unit(0, "cm")) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  #theme(legend.text =  element_text(size = 12, color = "white"), legend.title = element_text(size = 13)) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = -50-(.3*250), ymax = -50) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = TAIWAN_FINANCIAL_ACCTS_GRAPH, "Taiwan Financial Accts Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

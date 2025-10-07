pacman::p_load(jsonlite,ggpubr,ggrepel,tigris,purrr,forecast,imputeTS,tsibble,sf,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

install_github("keberwein/blscrapeR")
library(blscrapeR)

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)



GDPbyIndustry <- beaSearch("GDPbyIndustry", beaKey = Sys.getenv("BEA_KEY"))

beaSets(beaKey = Sys.getenv("BEA_KEY"))
beaParams("GDPbyIndustry", beaKey = Sys.getenv("BEA_KEY"))
beaParamVals("GDPbyIndustry","TableID",beaKey = Sys.getenv("BEA_KEY"))


GDPbyIndustry_BULK_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'GDPbyIndustry',
  'TableID' = 'ALL',
  'Industry' = 'ALL',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

GDPbyIndustry_BULK <- beaGet(GDPbyIndustry_BULK_specs, asTable = FALSE) %>%
  content(as = "text", encoding = "UTF-8") %>%
  gsub("[\r\n]", "", .) %>%
  fromJSON(simplifyDataFrame = TRUE) %>%
  purrr::pluck("BEAAPI", "Results", "Data") %>%
  { if (is.data.frame(.)) as_tibble(.) else purrr::map_dfr(., ~ as_tibble(.x)) } %>%
  as.data.frame() %>%
  `colnames<-`(c("TableID","Frequency","Year","Quarter","Industry","IndustryDescription","DataValue","NoteRef")) %>%
  mutate(q_num = dplyr::recode(Quarter,"I" = 1L, "II" = 2L, "III" = 3L, "IV" = 4L),date = as.Date(as.yearqtr(paste0(Year," Q",q_num), format = "%Y Q%q"))) %>%
  select(-q_num) %>%
  mutate(DataValue = as.numeric(DataValue))

GDPbyIndustry_CONTRIB <- GDPbyIndustry_BULK %>%
  filter(TableID == 13) %>%
  filter(IndustryDescription %in% c(
    "Finance and insurance",
    "Agriculture, forestry, fishing, and hunting",
    "Wholesale trade",
    "Retail trade",
    "Mining",
    "Other services, except government",
    "Management of companies and enterprises",
    "Utilities",
    "Arts, entertainment, and recreation",
    "Accommodation and food services",
    "Transportation and warehousing",
    "Educational services",
    "Construction",
    "Nondurable goods",
    "Administrative and waste management services",
    "Federal",
    "Durable goods",
    "Professional, scientific, and technical services",
    "Health care and social assistance",
    "State and local",
    "Information",
    "Real estate and rental and leasing"
  )) %>%
  filter(date == max(date)) %>%
  mutate(
    IndustryDescription = case_when(
      IndustryDescription == "Nondurable goods" ~ "Nondurable manufacturing",
      IndustryDescription == "Durable goods" ~ "Durable manufacturing",
      IndustryDescription == "State and local" ~ "State & local government",
      IndustryDescription == "Federal" ~ "Federal government",
      TRUE ~ IndustryDescription
    ),
    IndustryDescription = gsub(" and ", " & ", IndustryDescription),
    IndustryDescription = str_to_title(IndustryDescription)
  ) %>%
  arrange(desc(DataValue)) %>%
  mutate(IndustryDescription = factor(IndustryDescription, levels = rev(IndustryDescription)))

GDPbyIndustry_CONTRIB_GRAPH <- ggplot(data = GDPbyIndustry_CONTRIB, aes(x = IndustryDescription, y = DataValue/100)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA, fill = "#FFE98F") +
  xlab(NULL) +
  ggtitle(paste0("US RGDP Contributions by Industry, ", format(as.yearqtr(GDPbyIndustry_CONTRIB$date), "Q%q %Y"))) +
  ylab("Contribution, Seasonally Adjusted at Annual Rates") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0)) +
  #labs(subtitle = "By % of US Imports") +
  labs(caption = "Graph created by @JosephPolitano using BEA Data") +
  theme_apricitas + theme(legend.position = c(.5,.4), plot.margin= grid::unit(c(0.2, .2, 0.2, .2), "in"), axis.text.y = element_text(size = 14, color = "white"), axis.title.x = element_text(size = 14, color = "white")) +
  coord_flip() +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = GDPbyIndustry_CONTRIB_GRAPH, "GDP by Industry Contrib Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


RVAbyIndustry <- GDPbyIndustry_BULK %>%
  filter(TableID == "10") %>%
  filter(IndustryDescription != "Government enterprises") %>%
  filter(IndustryDescription != "General government") %>%
  select(IndustryDescription,DataValue,date) %>%
  pivot_wider(names_from = IndustryDescription, values_from = DataValue)


RVAbyIndustry_GRAPH <- ggplot() + #indexed fixed investment
  geom_line(data = RVAbyIndustry, aes(x=date, y = Manufacturing/1000, color = "US Real Value Added by Industry: Manufacturing"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T"), limits = c(1.9,2.5), breaks = c(1.9,2,2.1,2.2,2.3,2.4,2.5), expand = c(0,0)) +
  ylab("Real Value Added, 2017 Dollars") +
  ggtitle("US Real Manufacturing Output") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Growth in US Manufacturing Value-Added Has Been Weak") +
  theme_apricitas + theme(legend.position = c(.4,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2017-01-01")-(.1861*(today()-as.Date("2017-01-01"))), xmax = as.Date("2017-01-01")-(0.049*(today()-as.Date("2017-01-01"))), ymin = 1.9-(.3*0.6), ymax = 1.9) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = RVAbyIndustry_GRAPH, "RVA by Industry Manufacturing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

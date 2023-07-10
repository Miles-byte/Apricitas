pacman::p_load(bea.R,cbsodataR,seasonal,eurostat,censusapi,estatapi,janitor,openxlsx,dplyr,BOJ,readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FEDERAL_STUDENT_LOANS <- fredr(series_id = "FGCCSAQ027S", observation_start = as.Date("2006-01-01")) %>%
  mutate(series_id = "Federal")
ALL_STUDENT_LOANS <- fredr(series_id = "BOGZ1FL153166220Q", observation_start = as.Date("2006-01-01")) %>%
  mutate(series_id = "Other")
DSPI <- fredr(series_id = "DSPI", observation_start = as.Date("2006-01-01"), frequency = "q", aggregation_method = "avg") 

STUDENT_LOANS <- rbind(ALL_STUDENT_LOANS,FEDERAL_STUDENT_LOANS,DSPI) %>%
  pivot_wider(names_from = series_id) %>%
  transmute(date, Other = (Other-Federal)/(DSPI*1000), Federal = Federal/(DSPI*1000)) %>%
  drop_na() %>%
  pivot_longer(cols = c(Other, Federal)) %>%
  mutate(name = factor(name,levels = c("Other", "Federal")))

STUDENT_LOANS_DSPI_graph <- ggplot(data = STUDENT_LOANS, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("% of Disposable Income") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.05,0.1), limits = c(0,.105), expand = c(0,0)) +
  ggtitle("All Student Loans, % of Aggregate Disposable Income") +
  labs(caption = "Graph created by @JosephPolitano using Federal Reserve and BEA data", subtitle = "The Pandemic-era Forebearance Caused the Aggregate Burden of Student Loans To Fall") +
  theme_apricitas + theme(legend.position = c(.12,.82)) +
  theme(plot.title = element_text(size = 21)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E"), breaks = c("Federal", "Other")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2006-01-01")-(.1861*(today()-as.Date("2006-01-01"))), xmax = as.Date("2006-01-01")-(0.049*(today()-as.Date("2006-01-01"))), ymin = 0-(.3*.105), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = STUDENT_LOANS_DSPI_graph, "Student Loans DSPI.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

url1 <- 'https://studentaid.gov/sites/default/files/fsawg/' |>
paste0('datacenter/library/Portfolio-by-Debt-Size.xls')

UA <- 'Mozilla/5.0 (Windows NT 10.0; Win64;' |> 
  paste('x64; rv:109.0) Gecko/20100101 Firefox/114.0')

httr::GET(url1, httr::user_agent(UA), 
          httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

LOAN_BY_DEBT_SIZE <- read_xls(tf)

  
  
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

p_unload(all)

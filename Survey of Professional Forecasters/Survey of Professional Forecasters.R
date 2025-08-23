pacman::p_load(readxl,RcppRoll,DSSAT,tidyr,eia,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

MEDIAN_GDP_LVLS <- read_excel("https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/survey-of-professional-forecasters/historical-data/medianLevel.xlsx?sc_lang=en&hash=E52F5DEF4551C3EBD24031A04CE6E98A", sheet = "RGDP")

MEDIAN_GDP_GROWTH <- read_xlsx("https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/survey-of-professional-forecasters/historical-data/medianGrowth.xlsx?sc_lang=en&hash=44EE1B3E0569944F81475E78F1C7FEED", sheet = "RGDP")

library(readxl)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


MEDIAN_LEVELS_TEMPFILE <- tempfile(fileext = ".xlsx")
download.file("https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/survey-of-professional-forecasters/historical-data/medianLevel.xlsx?sc_lang=en&hash=E52F5DEF4551C3EBD24031A04CE6E98A",
              destfile = MEDIAN_LEVELS_TEMPFILE, mode = "wb")

MEDIAN_GROWTH_TEMPFILE <- tempfile(fileext = ".xlsx")
download.file("https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/survey-of-professional-forecasters/historical-data/medianGrowth.xlsx?sc_lang=en&hash=44EE1B3E0569944F81475E78F1C7FEED",
              destfile = MEDIAN_GROWTH_TEMPFILE, mode = "wb")


MEDIAN_GDP_GROWTH <- read_xlsx(MEDIAN_GROWTH_TEMPFILE, sheet = "RGDP") %>%
  mutate(drgdp6 = as.numeric(drgdp6)) %>%
  mutate(date = yq(paste0(YEAR, " Q", QUARTER))) %>%
  mutate(fcasted_date_2 = date,
         fcasted_date_3 = date %m+% months(3),
         fcasted_date_4 = date %m+% months(6),
         fcasted_date_5 = date %m+% months(9),
         fcasted_date_6 = date %m+% months(12)) %>%
  select(-YEAR,-QUARTER) %>%
  pivot_longer(
    cols = matches("drgdp\\d+|fcasted_date_\\d+"),
    names_to = c(".value", "horizon"),
    names_pattern = "([a-z_]+)(\\d+)"
  ) %>%
  rename(
    forecast = drgdp,
    date_forecasted = fcasted_date_
  ) %>%
  select(date, forecast, date_forecasted)


GDP_GROWTH_GRAPH <- ggplot() + #plotting GDP Forecast Growth
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(MEDIAN_GDP_GROWTH, date_forecasted == as.Date("2025-07-01")), aes(x=date,y= forecast/100, color= "Median GDP Forecast for Q3 2025"), size = 1.25) +
  geom_line(data=filter(MEDIAN_GDP_GROWTH, date_forecasted == as.Date("2025-10-01")), aes(x=date,y= forecast/100, color= "Median GDP Forecast for Q4 2025"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,0.025), expand = c(0,0)) +
  ylab("Percent Growth, Seasonally Adjusted Annual Rate") +
  ggtitle("Superstar Upset") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Prices Growth in Smaller Metros Has Caught Up to Larger Metros") +
  theme_apricitas + theme(legend.position = c(.25,.25)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-01-01")-(.1861*(today()-as.Date("2015-01-01"))), xmax = as.Date("2015-01-01")-(0.049*(today()-as.Date("2015-01-01"))), ymin = 0-(.3*.08), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")


MEDIAN_GDP_LVLS <- read_xlsx(MEDIAN_LEVELS_TEMPFILE, sheet = "RGDP") %>%
  select(-RGDPA, -RGDPB, -RGDPC, -RGDPD) %>% #removing annual averages
  mutate(RGDP6 = as.numeric(RGDP6)) %>%
  mutate(date = yq(paste0(YEAR, " Q", QUARTER))) %>%
  mutate(fcasted_date_2 = date,
         fcasted_date_3 = date %m+% months(3),
         fcasted_date_4 = date %m+% months(6),
         fcasted_date_5 = date %m+% months(9),
         fcasted_date_6 = date %m+% months(12)) %>%
  select(-YEAR,-QUARTER) %>%
  pivot_longer(
    cols = matches("RGDP\\d+|fcasted_date_\\d+"),
    names_to = c(".value", "horizon"),
    names_pattern = "([A-Za-z_]+)(\\d+)"
  ) %>%
  rename(
    forecast = RGDP,
    date_forecasted = fcasted_date_
  ) %>%
  select(date, forecast, date_forecasted)

REAL_GDP <- fredr(series_id = "GDPC1", observation_start = as.Date("2023-01-01"))

new_rows <- MEDIAN_GDP_LVLS %>%
  distinct(date) %>%
  mutate(date_forecasted = date %m-% months(3)) %>%
  left_join(REAL_GDP %>% select(date, forecast = value),
            by = c("date_forecasted" = "date")) %>%
  filter(!is.na(forecast)) %>%
  select(date, forecast, date_forecasted)

MEDIAN_GDP_LVLS <- MEDIAN_GDP_LVLS %>%
  bind_rows(new_rows) %>%
  arrange(date, date_forecasted)

GDP_LEVELS_GRAPH <- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(MEDIAN_GDP_LVLS, date == as.Date("2025-01-01")), aes(x=date_forecasted,y= forecast/1000, color= "Q1 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=filter(MEDIAN_GDP_LVLS, date == as.Date("2025-04-01")), aes(x=date_forecasted,y= forecast/1000, color= "Q2 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=filter(MEDIAN_GDP_LVLS, date == as.Date("2025-07-01")), aes(x=date_forecasted,y= forecast/1000, color= "Q3 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=REAL_GDP, aes(x=date,y= value/1000, color= "Real GDP"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 0.1, suffix = "T"), limits = c(22,25), expand = c(0,0)) +
  ylab("Real GDP") +
  ggtitle("US Real GDP & Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using Philly Fed Survey of Professional Forecasters data",subtitle = "Forecasters Have Increased GDP Projections, But they Remain Below Pre-Trade-War Levels") +
  theme_apricitas + theme(legend.position = c(.25,.75)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Real GDP","Q1 2025 Median Forecasts","Q2 2025 Median Forecasts","Q3 2025 Median Forecasts")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()+365-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()+365-as.Date("2023-01-01"))), ymin = 22-(.3*3), ymax = 22) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(override.aes = list(linewidth = c(2.25, 1.25, 1.25, 1.25))))

ggsave(dpi = "retina",plot = GDP_LEVELS_GRAPH, "GDP Levels Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


MEDIAN_UNEMP_LVLS <- read_xlsx(MEDIAN_LEVELS_TEMPFILE, sheet = "UNEMP") %>%
  select(-UNEMPA, -UNEMPB, -UNEMPC, -UNEMPD) %>% #removing annual averages
  mutate(UNEMP6 = as.numeric(UNEMP6)) %>%
  mutate(date = yq(paste0(YEAR, " Q", QUARTER))) %>%
  mutate(fcasted_date_2 = date,
         fcasted_date_3 = date %m+% months(3),
         fcasted_date_4 = date %m+% months(6),
         fcasted_date_5 = date %m+% months(9),
         fcasted_date_6 = date %m+% months(12)) %>%
  select(-YEAR,-QUARTER) %>%
  pivot_longer(
    cols = matches("UNEMP\\d+|fcasted_date_\\d+"),
    names_to = c(".value", "horizon"),
    names_pattern = "([A-Za-z_]+)(\\d+)"
  ) %>%
  rename(
    forecast = UNEMP,
    date_forecasted = fcasted_date_
  ) %>%
  select(date, forecast, date_forecasted)

UNRATE <- fredr(series_id = "UNRATE", observation_start = as.Date("2023-01-01"), frequency = "q")

new_rows <- MEDIAN_UNEMP_LVLS %>%
  distinct(date) %>%
  mutate(date_forecasted = date %m-% months(3)) %>%
  left_join(UNRATE %>% select(date, forecast = value),
            by = c("date_forecasted" = "date")) %>%
  filter(!is.na(forecast)) %>%
  select(date, forecast, date_forecasted)

MEDIAN_UNEMP_LVLS <- MEDIAN_UNEMP_LVLS %>%
  bind_rows(new_rows) %>%
  arrange(date, date_forecasted)

UNEMP_LEVELS_GRAPH <- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(MEDIAN_UNEMP_LVLS, date == as.Date("2025-01-01")), aes(x=date_forecasted,y= forecast/100, color= "Q1 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=filter(MEDIAN_UNEMP_LVLS, date == as.Date("2025-04-01")), aes(x=date_forecasted,y= forecast/100, color= "Q2 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=filter(MEDIAN_UNEMP_LVLS, date == as.Date("2025-07-01")), aes(x=date_forecasted,y= forecast/100, color= "Q3 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=UNRATE, aes(x=date,y= value/100, color= "Unemployment Rate"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0.03,0.05), expand = c(0,0)) +
  ylab("Unemployment Rate") +
  ggtitle("US Unemployment Rate & Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using Philly Fed Survey of Professional Forecasters data",subtitle = "Forecasters Have Lowered Unemployment Projections, But they Remain Above Q1 2025 Levels") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Unemployment Rate","Q1 2025 Median Forecasts","Q2 2025 Median Forecasts","Q3 2025 Median Forecasts")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()+365-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()+365-as.Date("2023-01-01"))), ymin = 0.03-(.3*0.02), ymax = 0.03) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(override.aes = list(linewidth = c(2.25, 1.25, 1.25, 1.25))))

ggsave(dpi = "retina",plot = UNEMP_LEVELS_GRAPH, "UNEMP Levels Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



MEDIAN_EMP_LVLS <- read_xlsx(MEDIAN_LEVELS_TEMPFILE, sheet = "EMP") %>%
  select(-EMPA, -EMPB) %>% #removing annual averages
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(date = yq(paste0(YEAR, " Q", QUARTER))) %>%
  mutate(fcasted_date_2 = date,
         fcasted_date_3 = date %m+% months(3),
         fcasted_date_4 = date %m+% months(6),
         fcasted_date_5 = date %m+% months(9),
         fcasted_date_6 = date %m+% months(12)) %>%
  select(-YEAR,-QUARTER) %>%
  pivot_longer(
    cols = matches("EMP\\d+|fcasted_date_\\d+"),
    names_to = c(".value", "horizon"),
    names_pattern = "([A-Za-z_]+)(\\d+)"
  ) %>%
  rename(
    forecast = EMP,
    date_forecasted = fcasted_date_
  ) %>%
  select(date, forecast, date_forecasted)

EMP <- fredr(series_id = "PAYEMS", observation_start = as.Date("2023-01-01"), frequency = "q")

new_rows <- MEDIAN_UNEMP_LVLS %>%
  distinct(date) %>%
  mutate(date_forecasted = date %m-% months(3)) %>%
  left_join(EMP %>% select(date, forecast = value),
            by = c("date_forecasted" = "date")) %>%
  filter(!is.na(forecast)) %>%
  select(date, forecast, date_forecasted)

MEDIAN_EMP_LVLS <- MEDIAN_EMP_LVLS %>%
  bind_rows(new_rows) %>%
  arrange(date, date_forecasted)

EMP_LEVELS_GRAPH <- ggplot() + #plotting rent by A/B/C City Size
  annotate(geom = "hline",y = 0.0,yintercept = 0.0, size = .25,color = "white") +
  geom_line(data=filter(MEDIAN_EMP_LVLS, date == as.Date("2025-01-01")), aes(x=date_forecasted,y= forecast/1000, color= "Q1 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=filter(MEDIAN_EMP_LVLS, date == as.Date("2025-04-01")), aes(x=date_forecasted,y= forecast/1000, color= "Q2 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=filter(MEDIAN_EMP_LVLS, date == as.Date("2025-07-01")), aes(x=date_forecasted,y= forecast/1000, color= "Q3 2025 Median Forecasts"), size = 1.25) +
  geom_line(data=EMP, aes(x=date,y= value/1000, color= "Payroll Employment"), size = 2.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits = c(154,165), expand = c(0,0)) +
  ylab("Nonfarm Payrolls") +
  ggtitle("US Payroll Employment & Forecasts") +
  labs(caption = "Graph created by @JosephPolitano using Philly Fed Survey of Professional Forecasters data",subtitle = "Forecasters Have Lowered Employment Projections, But they Remain Above Q1 2025 Levels") +
  theme_apricitas + theme(legend.position = c(.25,.85)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"),breaks = c("Payroll Employment","Q1 2025 Median Forecasts","Q2 2025 Median Forecasts","Q3 2025 Median Forecasts")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()+365-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()+365-as.Date("2023-01-01"))), ymin = 154-(.3*11), ymax = 154) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(override.aes = list(linewidth = c(2.25, 1.25, 1.25, 1.25))))

ggsave(dpi = "retina",plot = EMP_LEVELS_GRAPH, "EMP Levels Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


pacman::p_load(sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

?bls_api

MIDDLE_20_GROCERIES <- bls_api("CXUFOODHOMELB0104M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUFOODHOMELB0104M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

MIDDLE_20_FUEL <- bls_api("CXUGASOILLB0104M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUGASOILLB0104M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

MIDDLE_20_INCOME <- bls_api("CXUINCAFTTXLB0104M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXUINCAFTTXLB0104M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

MIDDLE_20_GROCERY_SHARE <- merge(MIDDLE_20_GROCERIES,MIDDLE_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

MIDDLE_20_FUEL_SHARE <- merge(MIDDLE_20_FUEL,MIDDLE_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

MIDDLE_20_UTILS_SHARE <- merge(MIDDLE_20_UTILS,MIDDLE_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

BOTTOM_20_GROCERIES <- bls_api("CXUFOODHOMELB0102M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXUFOODHOMELB0102M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

BOTTOM_20_FUEL <- bls_api("CXUGASOILLB0102M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXUGASOILLB0102M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

BOTTOM_20_INCOME <- bls_api("CXUINCAFTTXLB0102M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUINCAFTTXLB0102M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

BOTTOM_20_GROCERY_SHARE <- merge(BOTTOM_20_GROCERIES,BOTTOM_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

BOTTOM_20_FUEL_SHARE <- merge(BOTTOM_20_FUEL,BOTTOM_20_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

SHARES_BOTTOM_Graph <- ggplot() + 
  geom_line(data=BOTTOM_20_FUEL_SHARE, aes(x=date,y= value,color= "Bottom 20%: Gasoline and Other Fuels as a Share of Post-tax Income"), size = 1.25)+ 
  geom_line(data=BOTTOM_20_GROCERY_SHARE, aes(x=date,y= value,color= "Bottom 20%: Food at Home as a Share of Post-tax Income"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.42),breaks = c(0,.10,.20,.30,.40), expand = c(0,0)) +
  ggtitle("Back in Business") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Unemployment Rate is Near Historic Lows") +
  theme_apricitas + theme(legend.position = c(.6,.88)) +
  scale_color_manual(name= "Lowest 20% of Consumer Units by Pretax Income",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1950-01-01")-(.1861*26420), xmax = as.Date("1950-01-01")-(0.049*26420), ymin = 0-(.3*.15), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SHARES_BOTTOM_Graph, "Share of Expenditures Bottom 20.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

SHARES_MIDDLE_Graph <- ggplot() +
  geom_line(data=MIDDLE_20_FUEL_SHARE, aes(x=date,y= value,color= "Gasoline, Other Fuels, and Motor Oil as a Share of Post-tax Income"), size = 1.25)+ 
  geom_line(data=MIDDLE_20_GROCERY_SHARE, aes(x=date,y= value,color= "Food at Home as a Share of Post-tax Income"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.12),breaks = c(0,.05,.10), expand = c(0,0)) +
  ggtitle("The Cost of Gas and Grocery Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Relative Spending on Groceries and Gasoline Increased Significantly in 2022") +
  theme_apricitas + theme(legend.position = c(.5,.15)) +
  scale_color_manual(name= "Middle 20% of Consumer Units by Pretax Income",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1984-01-01")-(.1861*(today()-as.Date("1984-01-01"))), xmax = as.Date("1984-01-01")-(0.049*(today()-as.Date("1984-01-01"))), ymin = 0-(.3*.13), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = SHARES_MIDDLE_Graph, "Share of Expenditures Middle 20.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing

RENT_EXPEND <- bls_api("CXURNTDWELLLB1705M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>% 
  select(-latest) %>%
  rbind(.,bls_api("CXURNTDWELLLB1705M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

RENT_INCOME <- bls_api("CXUINCAFTTXLB1705M", startyear = 2004, registrationKey = Sys.getenv("BLS_KEY")) %>%
  select(-latest) %>%
  rbind(.,bls_api("CXUINCAFTTXLB1705M", startyear = 1984, registrationKey = Sys.getenv("BLS_KEY"))) %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  select(date, value)

RENT_INCOME_EXPEND_SHARE <- merge(RENT_EXPEND,RENT_INCOME, by = "date") %>%
  transmute(date, value = value.x/value.y)

SHARES_MIDDLE_Graph <- ggplot() +
  geom_line(data=RENT_INCOME_EXPEND_SHARE, aes(x=date,y= value,color= "Rent as a Share of Post-tax Income, Renter Households"), size = 1.25)+ 
  xlab("Date") +
  ylab("%") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.3),breaks = c(0,.05,.10), expand = c(0,0)) +
  ggtitle("The Cost of Gas and Grocery Inflation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Relative Spending on Groceries and Gasoline Increased Significantly in 2022") +
  theme_apricitas + theme(legend.position = c(.5,.15)) +
  scale_color_manual(name= "Middle 20% of Consumer Units by Pretax Income",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("1984-01-01")-(.1861*(today()-as.Date("1984-01-01"))), xmax = as.Date("1984-01-01")-(0.049*(today()-as.Date("1984-01-01"))), ymin = 0-(.3*.13), ymax = 0) +
  coord_cartesian(clip = "off")

p_unload(all)  # Remove all packages using the package manager

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
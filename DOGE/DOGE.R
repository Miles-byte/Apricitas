pacman::p_load(janitor,bea.R,RcppRoll,sf,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
install.packages("cli")
install_github("keberwein/blscrapeR")
library(blscrapeR)

theme_apricitas <- theme_ft_rc() +
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

FED_WFH_PCT <- bls_api("LNU0201BAC7", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  mutate(series_id = "Federal") %>%
  drop_na()

PRI_WFH_PCT <- bls_api("LNU0201BAE5", startyear = 2019, registrationKey = Sys.getenv("BLS_KEY")) %>%
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))%>%
  .[order(nrow(.):1),] %>%
  select(date, value) %>%
  mutate(series_id = "Federal") %>%
  drop_na()


FED_WFH_PCT_Graph <- ggplot() + #plotting Wage Growth
  geom_line(data=FED_WFH_PCT, aes(x=date,y= value/100,color= "Federal Employees, % of All Hours Worked From Home"), size = 1.25) +
  geom_line(data=PRI_WFH_PCT, aes(x=date,y= value/100,color= "Private Sector Employees, % of All Hours Worked From Home"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.25), breaks = c(0,0.05,0.1,0.15,0.2,0.25), expand = c(0,0)) +
  ylab("Percent of All Hours Worked") +
  ggtitle("DOGE Reduced Federal Work From Home") +
  labs(caption = "Graph created by @JosephPolitano using BLS Data",subtitle = "Federal Work From Home Hours Were Cut By More than 50% Amidst DOGE") +
  theme_apricitas + theme(legend.position = c(.4,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-10-01")-(.1861*(today()-as.Date("2022-10-01"))), xmax = as.Date("2022-10-01")-(0.049*(today()-as.Date("2022-10-01"))), ymin = 0-(.3*0.25), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = FED_WFH_PCT_Graph, "Federal WFH Percent Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #cairo gets rid of anti aliasing


FED_EMP_IND_2025 <- bls_api("CES9091000001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Total Federal Government") %>%
  select(date,value,name,pct)

CIV_EMP_IND_2025 <- bls_api("CES9091999901", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Civilian Agencies") %>%
  select(date,value,name,pct)

DOD_EMP_IND_2025 <- bls_api("CES9091911001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Department of Defense") %>%
  select(date,value,name,pct)

POS_EMP_IND_2025 <- bls_api("CES9091912001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Post Office") %>%
  select(date,value,name,pct)

MED_EMP_IND_2025 <- bls_api("CES9091622001", startyear = 2025, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(pct = (value-value[nrow(.)])/value[nrow(.)]) %>%
  mutate(value = value-value[nrow(.)]) %>%
  mutate(name = "Federal Hospitals (VA/IHS)") %>%
  select(date,value,name,pct)

FED_RBIND <- rbind(CIV_EMP_IND_2025,DOD_EMP_IND_2025,POS_EMP_IND_2025,MED_EMP_IND_2025) %>%
  group_by(date) %>%
  filter(n() == 4)

FED_EMP_IND_2025 <- FED_EMP_IND_2025 %>%
  filter(date <= max(FED_RBIND$date))

US_FED_EMP_IND_DECOMP_graph <- ggplot(data = filter(FED_RBIND, date >= as.Date("2023-01-01")), aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  geom_line(data = filter(FED_EMP_IND_2025, date >= as.Date("2023-01-01")), aes(x=date, y = value, color = "Total Federal Government"), size = 2) +
  xlab("Date") +
  ylab("Change in Jobs, Thousands of Jobs") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(-300,-200,-100,0), limits = c(-300,50), expand = c(0,0)) +
  ggtitle("US Federal Employment is Falling") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Federal Employment has Decreased by ", -FED_EMP_IND_2025$value[1], "k, or ", -round(FED_EMP_IND_2025$pct[1],4)*100,"% since January amidst DOGE cuts")) +
  theme_apricitas + theme(legend.position = c(.35,.25)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_fill_manual(name= "Change in Federal Employment Since Jan 2025",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Post Office","Federal Hospitals (VA/IHS)","Department of Defense","Civilian Agencies")) +
  scale_color_manual(name = NULL, values = "#EE6055") +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2025-01-01")-(.1861*(today()-as.Date("2025-01-01"))), xmax = as.Date("2025-01-01")-(0.049*(today()-as.Date("2025-01-01"))), ymin = -300-(.3*350), ymax = -300) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = US_FED_EMP_IND_DECOMP_graph, "US Fed Empy Ind Decomp.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

FED_UNEMP <- bls_api("LNU03097260", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  select(date,value,name) %>%
  arrange(date) %>%
  mutate(roll = c(0,0,rollmean(value,3)))

FED_UNEMP <- bls_api("LNU03000000", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), (lag(.x) + lead(.x)) / 2, .x))) %>% #mutating across NA to average the month before and after to cover for October missing data
  select(date,value,name) %>%
  arrange(date) %>%
  mutate(roll = c(0,0,rollmean(value,3)))


US_FED_UNEMP_graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data = filter(FED_UNEMP, date >= as.Date("2022-01-01")), aes(x=date, y = roll, color = "Unemployed Former Federal Employees"), size = 1.25) +
  geom_line(data = filter(FED_UNEMP, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Unemployed Former Federal Employees"), size = 0.75,linetype = "dashed", alpha = 0.75) +
  xlab("Date") +
  ylab("Unemployment Level") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,100,200), limits = c(0,250), expand = c(0,0)) +
  ggtitle("200K Former Federal Employees are Unemployed") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("The Number of Unemployed Former Federal Employees Has Doubled to More than 200k Amidst DOGE")) +
  theme_apricitas + theme(legend.position = c(.35,.85)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_color_manual(name= "Dashed = Monthly, Solid = 3M Moving Average\nNot Seasonally Adjusted",values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*250), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = US_FED_UNEMP_graph, "US Fed Unemp Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


FED_HIRES <- bls_api("JTS910000000000000HIL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  select(date,value,name)

FED_FIRES <- bls_api("JTS910000000000000LDL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  select(date,value,name)

FED_RETR <- bls_api("JTS910000000000000OSL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  select(date,value,name)

FED_QUITS <- bls_api("JTS910000000000000QUL", startyear = 2019, endyear = format(Sys.Date(), "%Y"), Sys.getenv("BLS_KEY")) %>% #federal unemployment levels
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(name = "Unemployment Level, Former Federal Employees") %>%
  select(date,value,name)

FED_LAB_FLO_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data = filter(FED_HIRES, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Hires"), size = 1.25) +
  geom_line(data = filter(FED_FIRES, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Layoffs & Discharges"), size = 1.25) +
  geom_line(data = filter(FED_RETR, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Retirements & Other Separations"), size = 1.25) +
  geom_line(data = filter(FED_QUITS, date >= as.Date("2022-01-01")), aes(x=date, y = value, color = "Quits"), size = 1.25) +
  annotate(geom = "segment", x = as.Date("2025-01-01"), xend = as.Date("2025-01-01"), y = 0, yend = 55, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "DOGE\nBegins", x = as.Date("2024-08-15"), y = 23, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  annotate("text", label = "Defferred\nResignations", x = as.Date("2025-05-01"), y = 50, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Thousands, Monthly") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, suffix = "k"), breaks = c(0,10,20,30,40,50,60), limits = c(0,55), expand = c(0,0)) +
  ggtitle("Federal Employee Labor Flows") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = paste0("Federal Employment Fell Due to Reduced Hires, Increased Quits, More Retirements, and Layoffs")) +
  theme_apricitas + theme(legend.position = c(.40,.65)) + theme(legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 13)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Hires","Quits","Retirements & Other Separations","Layoffs & Discharges")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-01-01")-(.1861*(today()-as.Date("2022-01-01"))), xmax = as.Date("2022-01-01")-(0.049*(today()-as.Date("2022-01-01"))), ymin = 0-(.3*60), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off") +
  theme(plot.title.position = "plot")

ggsave(dpi = "retina",plot = FED_LAB_FLO_Graph, "US Fed Labor Flo Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



BEA_FED_OUTPUT_specs <- list(
  'UserID' =  Sys.getenv("BEA_KEY"),
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T31006',
  'Frequency' = 'Q',
  'Year' = paste(seq(from = 2017, to = as.integer(format(Sys.Date(), "%Y"))), collapse = ","),
  'ResultFormat' = 'json'
)

BEA_FED_OUTPUT <- beaGet(BEA_FED_OUTPUT_specs, iTableStyle = FALSE) %>%
  mutate(date = (seq(as.Date("2017-01-01"), length.out = nrow(.), by = "3 months"))) %>%
  clean_names()

BEA_FED_OUTPUT_graph <- ggplot() + #plotting defense and nondefense intermediate goods spending
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data = filter(BEA_FED_OUTPUT, date >= as.Date("2023-01-01")), aes(x=date, y = t31006_w087rx_28_intermediate_goods_and_services_purchased_chained_dollars_level_6/t31006_w087rx_28_intermediate_goods_and_services_purchased_chained_dollars_level_6[8]-1, color = "Defense"), size = 1.25) +
  geom_line(data = filter(BEA_FED_OUTPUT, date >= as.Date("2023-01-01")), aes(x=date, y = t31006_w131rx_39_intermediate_goods_and_services_purchased_chained_dollars_level_6/t31006_w131rx_39_intermediate_goods_and_services_purchased_chained_dollars_level_6[8]-1, color = "Nondefense"), size = 1.25) +
  geom_line(data = filter(BEA_FED_OUTPUT, date >= as.Date("2023-01-01")), aes(x=date, y = t31006_w113rx_17_intermediate_goods_and_services_purchased_chained_dollars_level_6/t31006_w113rx_17_intermediate_goods_and_services_purchased_chained_dollars_level_6[8]-1, color = "Total"), size = 2.25) +
  annotate(geom = "segment", x = as.Date("2024-10-01"), xend = as.Date("2024-10-01"), y = -.2, yend = 0, color = "white",linetype = "dashed", size = 1, alpha = 0.75) +
  annotate("text", label = "DOGE\nBegins", x = as.Date("2024-07-01"), y = -.1, color = "white", size = 5, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  ylab("Change in Real Spending, %") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-.2,0.1), breaks = c(-.2,-.1,0,.1), expand = c(0,0)) +
  ggtitle("US Federal Contractor Spending is Down") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Federal Contractor Spending Is Down, With Cuts Especially Concentrated in Nondefense Spending") +
  theme_apricitas + theme(legend.position = c(.40,.85)) + theme(plot.title = element_text(size = 26), legend.margin=margin(0,0,-7,0), legend.spacing.y = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) +
  scale_color_manual(name= "Real Federal Purchases of Intermediate Goods & Services\n(Proxy for Contractor Spending), Change Since Q4 2024",values = c("#EE6055","#FFE98F","#00A99D","#9A348E","#A7ACD9","#3083DC","#6A4C93"), breaks = c("Defense","Nondefense","Total")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2023-01-01")-(.1861*(today()-as.Date("2023-01-01"))), xmax = as.Date("2023-01-01")-(0.049*(today()-as.Date("2023-01-01"))), ymin = -.2-(.3*0.3), ymax = -.2) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BEA_FED_OUTPUT_graph, "BEA Fed Output Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

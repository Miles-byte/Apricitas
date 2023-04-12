Relative_Importance <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/RelativeImportance.csv") %>%
  `colnames<-`(c("Category","2018-01-01","2020-01-01","2022-01-01","2023-01-01")) %>%
  select(-`2023-01-01`) %>% #DELETE THIS BEFORE JAN CPI
  pivot_longer(cols=c(-Category),names_to="Original_Vars")%>%
  pivot_wider(names_from=c(Category)) %>%
  mutate(Original_Vars = as.Date(Original_Vars)) %>%
  select(Original_Vars, `All items`,Food,Energy,`Commodities less food and energy commodities`,`Services less energy services`)%>%
  `colnames<-`(c("date","All","Food","Energy","Goods_LFE","Services_LE")) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  `colnames<-`(c("date","Category","value","Indicator")) %>%
  mutate(Indicator = "Relative_Importance")

CPI_ALL <- bls_api("CUUR0000SA0", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "All") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_FOOD <- bls_api("CUUR0000SAF1", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Food") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_ENERGY <- bls_api("CUUR0000SA0E", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Energy") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_COM_LFE <- bls_api("CUUR0000SACL1E", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Goods_LFE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_SERV_LE <- bls_api("CUUR0000SASLE", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Services_LE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

# CPI_ALL <- fredr(series_id = "CPIAUCNS",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "All") %>%
#   select(date,value,Category)
# CPI_FOOD <- fredr(series_id = "CPIUFDNS",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "Food")%>%
#   select(date,value,Category)
# CPI_ENERGY <- fredr(series_id = "CPIENGNS",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Energy")%>%
#   select(date,value,Category)
# CPI_COM_LFE <- fredr(series_id = "CUUR0000SACL1E",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Goods_LFE")%>%
#   select(date,value,Category)
# CPI_SERV_LE <- fredr(series_id = "CUUR0000SASLE",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Services_LE")%>%
#   select(date,value,Category)

CPI_Indices <- rbind(CPI_ALL,CPI_FOOD,CPI_ENERGY,CPI_COM_LFE,CPI_SERV_LE) %>%
  mutate(Indicator = "Index_NSA")

CPI_CONTRIBUTION <- rbind(CPI_Indices,Relative_Importance) %>%
  pivot_wider(names_from = "Indicator") %>%
  mutate_cond(Category == "All" & date < as.Date("2020-01-01") & date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/246.524*100) %>%
  mutate_cond(Category == "All" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/256.974*100) %>%
  mutate_cond(Category == "All" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/278.802*100) %>%
  mutate_cond(Category == "Food" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/251.238*13.38400) %>%
  mutate_cond(Category == "Food" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/259.823*13.771) %>%
  mutate_cond(Category == "Food" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/286.966*13.37) %>%
  mutate_cond(Category == "Energy" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/206.598*7.513) %>%
  mutate_cond(Category == "Energy" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/212.982*6.706) %>%
  mutate_cond(Category == "Energy" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/256.207*7.348) %>%
  mutate_cond(Category == "Goods_LFE" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/142.647*19.849000) %>%
  mutate_cond(Category == "Goods_LFE" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/142.920*20.137000) %>%
  mutate_cond(Category == "Goods_LFE" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/160.850*21.699000) %>%
  mutate_cond(Category == "Services_LE" & date < as.Date("2020-01-01")& date > as.Date("2018-01-01"), Relative_Importance = Index_NSA/322.250*59.254000) %>%
  mutate_cond(Category == "Services_LE" & date < as.Date("2022-01-01") & date > as.Date("2020-01-01"), Relative_Importance = Index_NSA/341.347*59.387) %>%
  mutate_cond(Category == "Services_LE" & date > as.Date("2022-01-01"), Relative_Importance = Index_NSA/359.559*57.583)

CPI_RI_ANNUAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index_NSA), names_from = "Category", values_from = Relative_Importance) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  arrange(match(name, c("All","Food","Energy","Goods_LFE","Services_LE")))

CPI_CONTRIBUTION_ANNUAL <- CPI_CONTRIBUTION
CPI_CONTRIBUTION_ANNUAL$Relative_Importance <- CPI_RI_ANNUAL_CALCULATIONS$value
CPI_CONTRIBUTION_ANNUAL <- drop_na(CPI_CONTRIBUTION_ANNUAL)


write.csv(CPI_CONTRIBUTION, "RI and Contrib.csv")
#making updated relative importance calculations
CPI_RI_FINAL_CALCULATIONS <- pivot_wider(select(CPI_CONTRIBUTION, - Index_NSA), names_from = "Category", values_from = Relative_Importance) %>%
  mutate(Food = Food/All*100) %>%
  mutate(Energy = Energy/All*100) %>%
  mutate(Goods_LFE = Goods_LFE/All*100) %>%
  mutate(Services_LE = Services_LE/All*100) %>%
  mutate(All = All/All*100) %>%
  pivot_longer(cols = c("All","Food","Energy","Goods_LFE","Services_LE")) %>%
  arrange(match(name, c("All","Food","Energy","Goods_LFE","Services_LE")))


CPI_CONTRIBUTION$Relative_Importance <- CPI_RI_FINAL_CALCULATIONS$value
CPI_CONTRIBUTION <- drop_na(CPI_CONTRIBUTION)
#CPI_CONTRIBUTION$January <- JANUARY$value

#adding seasonally adjusted data for seasonally adjusted monthly charts

CPI_ALL_SA <- bls_api("CUSR0000SA0", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "All") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_FOOD_SA <- bls_api("CUSR0000SAF1", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Food") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_ENERGY_SA <- bls_api("CUSR0000SA0E", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Energy") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_COM_LFE_SA <- bls_api("CUSR0000SACL1E", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Goods_LFE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

CPI_SERV_LE_SA <- bls_api("CUSR0000SASLE", startyear = 2017, endyear = 2023, Sys.getenv("BLS_KEY")) %>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(Category = "Services_LE") %>%
  subset(date >= as.Date("2017-12-01")) %>%
  select(date, value, Category)

# 
# CPI_ALL_SA <- fredr(series_id = "CUSR0000SA0",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "All") %>%
#   select(date,value,Category)
# CPI_FOOD_SA <- fredr(series_id = "CPIUFDSL",observation_start = as.Date("2017-12-01")) %>%
#   mutate(Category = "Food")%>%
#   select(date,value,Category)
# CPI_ENERGY_SA <- fredr(series_id = "CPIENGSL",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Energy")%>%
#   select(date,value,Category)
# CPI_COM_LFE_SA <- fredr(series_id = "CUSR0000SACL1E",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Goods_LFE")%>%
#   select(date,value,Category)
# CPI_SERV_LE_SA <- fredr(series_id = "CUSR0000SASLE",observation_start = as.Date("2017-12-01"))%>%
#   mutate(Category = "Services_LE")%>%
#   select(date,value,Category)

CPI_Indices_SA <- rbind(CPI_ALL_SA,CPI_FOOD_SA,CPI_ENERGY_SA,CPI_COM_LFE_SA,CPI_SERV_LE_SA)

JANUARY <- CPI_Indices_SA %>% #sometimes YOY calculations cross a weight update so I am creating a column for just the January relative importance data 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) %>%
  subset(date <= as.Date("2023-01-01"))

CPI_Indices_SA <- subset(CPI_Indices_SA, date > as.Date("2017-12-01"))

CPI_CONTRIBUTION_FINAL <- CPI_CONTRIBUTION %>%
  subset(., date > as.Date("2017-12-01")) %>%
  mutate(Index_SA = CPI_Indices_SA$value) %>%
  #mutate(January = JANUARY$value) %>%
  mutate(Monthly_Contribution_NSA = (Index_NSA/lead(Index_NSA))*lead(Relative_Importance)-lead(Relative_Importance)) %>%
  mutate(Monthly_Contribution_SA = (Index_SA/lead(Index_SA))*lead(Relative_Importance)-lead(Relative_Importance)) %>%
  #mutate(January_Contribution_NSA = lag(Relative_Importance,13)*(January-lag(Index_SA,12))/lag(Index_SA,12) + lag(Relative_Importance)*((Index_SA-January)/January)) %>%
  #mutate(January_Contribution_NSA = lag(Relative_Importance,12)*(January-lag(Index_SA,12))/lag(Index_SA,12) + lag(Relative_Importance)*((Index_SA-January)/January)) %>%
  mutate(Yearly_Contribution = (Index_NSA/lead(Index_NSA,12))*lead(Relative_Importance,12)-lead(Relative_Importance,12)) %>%
  drop_na() %>%
  subset(date >= as.Date("2019-01-01"))
  
ALL <- subset(CPI_CONTRIBUTION_ANNUAL, Category == "All") %>%
  mutate(All = Relative_Importance) %>%
  select(date, All) %>%
  bind_rows(replicate(3, ., simplify = FALSE))

JANUARY_ALL <- ALL %>% #sometimes YOY calculations cross a weight update so I am creating a column for just the January relative importance data 
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) 

JANUARY_RI <- CPI_CONTRIBUTION_ANNUAL %>% #sometimes YOY calculations cross a weight update so I am creating a column for just the January relative importance data 
  subset(Category != "All") %>%
  select(date, Category, Relative_Importance) %>%
  filter(month(ymd(date)) %in% c(1)) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(date,date + months(11), by = 'month'))) %>%
  unnest(cols = c(date)) 

#CPI_CONTRIBUTION_JANUARY <- CPI_CONTRIBUTION_ANNUAL %>%
  #mutate(Index_SA = CPI_Indices_SA$value) %>%
  #subset(Category != "All") %>%
  #mutate(JanuaryALL = JANUARY_ALL$All) %>%
  #mutate(JanuaryRI = as.numeric(JANUARY_RI$Relative_Importance)) %>%
  #mutate(All = ALL$All) %>%
  #mutate(January_Contribution_NSA = lag(Relative_Importance,13)*((January-lag(Index_SA,12))/lag(Index_SA,12)) + lag(Relative_Importance)*((Index_SA-January)/January)) %>%
  #mutate(All_January_Contribution_NSA = JANUARY_RI*lag(All,12)) %>%
  #mutate(All_January_Contribution_NSA = (JANUARY_RI/lag(All,12)-(lag(Relative_Importance,12)/lag(All,12)))+(Relative_Importance/JANUARY_ALL - JANUARY_RI/JANUARY_ALL)) %>%
  #select(date,Category,All_January_Contribution_NSA) %>%
  #subset(Category != "All") %>%
  #subset(date >= as.Date("2019-02-01")) %>%
  #pivot_wider(names_from = Category, values_from = January_Contribution_NSA) %>%
  #mutate()
  
#write.csv(CPI_CONTRIBUTION_FINAL,"CPICONTRIBFINAL.csv")

CPI_YOY_GROWTH <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2019-02-01"),units = "pc1") 

CPI_CONTRIBUTION_JANUARY <- CPI_CONTRIBUTION_FINAL %>%
  select(date,Category,January_Contribution_NSA) %>%
  subset(Category != "All") %>%
  subset(date >= as.Date("2019-02-01")) %>%
  pivot_wider(names_from = Category, values_from = January_Contribution_NSA) #%>%
  #mutate(Total = CPI_YOY_GROWTH$value) %>%
  #mutate(test = Food + Energy + Goods_LFE + Services_LE - Total) %>%
  #mutate(Food = Food *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
  #mutate(Energy = Energy *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
  #mutate(Goods_LFE = Goods_LFE *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
  #mutate(Services_LE = Services_LE *(Food + Energy + Goods_LFE + Services_LE)/Total) %>%
  #mutate(test = Food + Energy + Goods_LFE + Services_LE - Total)
  
CPI_CONTRIBUTION_JANUARY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = January_Contribution_NSA/100, fill = Category), color = NA, size = 0, stat= "identity") +
  #geom_line(data=CPI_YOY_GROWTH, aes(x=date, y=value/100, color="Annual Inflation"), size = 1.25) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.1), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation is Now Mostly a Story about Food and Core Services Prices") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  #scale_color_manual(name = NULL,values = c("#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.025-(.3*.125), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_JANUARY_GRAPH, "CPI January.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CPI_CONTRIBUTION_ANNUAL_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = Yearly_Contribution/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.025,.1), breaks = c(-.025,0,.025,.05,.075,.1), expand = c(0,0)) +
  ylab("Annual Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation Now Mostly Comes From Increases in Food and Core Services Prices") +
  theme_apricitas + theme(legend.position = c(.25,.80)) +
  scale_fill_manual(name= "Contributions to Annual CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.025-(.3*.125), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_ANNUAL_GRAPH, "CPI Annual.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


CPI_CONTRIBUTION_MONTHLY_NSA_GRAPH <- ggplot() + #plotting components of monthly inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All"), aes(x = date, y = Monthly_Contribution_NSA/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.01,.015), breaks = c(-.01,-0.005,0,0.005,.01,.015), expand = c(0,0)) +
  ylab("Monthly Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Dropping Energy Prices Pulled Inflation Down In August") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_fill_manual(name= "Contributions to Monthly CPI Inflation (NSA)",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.01-(.3*.025), ymax = -0.01) +
  coord_cartesian(clip = "off")

CPI_CONTRIBUTION_ANNUAL %>% subset(CPI_CONTRIBUTION_ANNUAL, date>= as.Date("2019-01-01"))

CPI_CONTRIBUTION_MONTHLY_SA_GRAPH <- ggplot() + #plotting components of monthly inflation
  geom_bar(data = subset(CPI_CONTRIBUTION_FINAL, Category != "All" & date >= as.Date("2019-01-01")), aes(x = date, y = Monthly_Contribution_SA/100, fill = Category), color = NA, size = 0, stat= "identity") +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5),limits = c(-.01,.015), breaks = c(-.01,-0.005,0,0.005,.01,.015), expand = c(0,0)) +
  ylab("Monthly Inflation, Percent") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Monthly Inflation is Now Mostly a Core Services Story") +
  theme_apricitas + theme(legend.position = c(.45,.80)) +
  scale_fill_manual(name= "Contributions to Monthly CPI Inflation",values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC"), breaks = c("Services_LE","Goods_LFE","Energy","Food"), labels = c("Core Services","Core Goods","Energy","Food")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = -0.01-(.3*.025), ymax = -0.01) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_CONTRIBUTION_MONTHLY_SA_GRAPH, "CPI RENT Contrib SA.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


ZORI <- read.csv("https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_sm_month.csv?t=1665666510") %>%
  select(-RegionID, -SizeRank, - RegionType, - StateName) %>%
  subset(RegionName == "United States") %>%
  #transpose() %>%
  gather(key = "date", value = "value", -1) %>%
  #`colnames<-`(.[1, ]) %>%
  mutate(date = c(seq(as.Date("2015-03-01"), as.Date("2023-03-01"), "months"))) %>%
  .[-1, ] %>%
  mutate(value = (value-lag(value,12))/lag(value,12))

CPIRENT <- bls_api("CUSR0000SEHA", startyear = 2017, endyear = 2023, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12))  %>%
  subset(date >= as.Date("2019-01-01")) #cpi rent data
CPIORENT <- bls_api("CUSR0000SEHC", startyear = 2017, endyear = 2023, calculations = TRUE, Sys.getenv("BLS_KEY"))%>% #headline cpi data
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y"))) %>%
  mutate(value = (value-lead(value,12))/lead(value,12)) %>%
  subset(date >= as.Date("2019-01-01"))
#cpi owners equivalent rent

ApartmentList <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Repeat%20Use%20Charts/CPI%20Releases/091322/apartmentlist.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

NTRR <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/NTRR/NTRR.csv") %>%
  mutate(date = as.Date(date))

CPI_Rent_Zillow <- ggplot() + #plotting Rent and Owner's Equivalent Rent Price Growth
  geom_line(data=CPIRENT, aes(x=date,y= (value) ,color= "CPI Rent"), size = 1.25) +
  geom_line(data=CPIORENT, aes(x=date,y= (value) ,color= "CPI Owner's Equivalent Rent"), size = 1.25) +
  geom_line(data=subset(ZORI, date > as.Date("2018-03-01")), aes(x=date+365,y= (value) ,color= "Zillow Observed Rent Index, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(ApartmentList, date > as.Date("2018-03-01")), aes(x=date+365,y= annualpct ,color= "ApartmentList Median New Lease, Lagged 1 Year"), size = 1.25) +
  geom_line(data=subset(NTRR,date > as.Date("2018-01-01")), aes(x=date+365,y= NTRR/100,color= "New Tenant Repeat Rent Index, Lagged 1 Year"), size = 1.25)+ 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-0.025,.20), breaks = c(0,.05,0.1,0.15,0.2), expand = c(0,0)) +
  ylab("Percent Change From a Year Ago, %") +
  ggtitle("Pandemic Prices") +
  labs(caption = "Graph created by @JosephPolitano using BLS,Zillow, and ApartmentList data",subtitle = "Whether Rent Growth Actually Peaks Will Be Critical to Future Inflation Prints") +
  theme_apricitas + theme(legend.position = c(.35,.70)) +
  scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E"), breaks = c("CPI Rent","CPI Owner's Equivalent Rent","Zillow Observed Rent Index, Lagged 1 Year","ApartmentList Median New Lease, Lagged 1 Year","New Tenant Repeat Rent Index, Lagged 1 Year")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()+365-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()+365-as.Date("2019-01-01"))), ymin = -0.025-(.3*0.225), ymax = -0.025) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPI_Rent_Zillow, "CPI RENT ZILLOW.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CPIPCT <- fredr(series_id = "CPIAUCSL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #CPI pct 
CPILFEPCT <- fredr(series_id = "CPILFESL",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL, units = "pc1") #CPI lfe

CPIPCT_Graph <- ggplot() + #plotting CPI/PCEPI against 2% CPI trend
  geom_line(data=CPIPCT, aes(x=date,y= (value/100) ,color= "CPI"), size = 1.25) +
  geom_line(data=CPILFEPCT, aes(x=date,y= value/100 ,color= "Core CPI"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.09), breaks = c(0,0.03,0.06,0.09), expand = c(0,0)) +
  ylab("Percent Change From Year Ago") +
  ggtitle("The Inflation Situation") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Inflation Is Coming Down Off a 40 Year High") +
  theme_apricitas + theme(legend.position = c(.40,.50)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*1200), xmax = as.Date("2019-01-01")-(0.049*1200), ymin = 0-(.3*0.09), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CPIPCT_Graph, "CPI PCT.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


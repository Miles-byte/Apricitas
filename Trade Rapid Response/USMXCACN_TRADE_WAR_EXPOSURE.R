MXCACN_IMPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/imports/hs",
  vars = c("GEN_VAL_YR","CTY_CODE", "CTY_NAME","I_COMMODITY_LDESC"), 
  time = "2024-12",
  CTY_NAME = "CHINA",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CANADA",
)

MXCACN_EXPORTS_BULK <- getCensus(
  name = "timeseries/intltrade/exports/hs",
  vars = c("ALL_VAL_YR","CTY_CODE", "CTY_NAME","E_COMMODITY_LDESC"), 
  time = "2024-12",
  CTY_NAME = "CHINA",
  CTY_NAME = "MEXICO",
  CTY_NAME = "CANADA",
)

MXCACN_EXPORTS <- MXCACN_EXPORTS_BULK %>%
  transmute(date = as.Date(paste0(time,"-01-01")), country = str_to_title(CTY_NAME), value = as.numeric(ALL_VAL_YR),category = "Imports")

MXCACN_IMPORTS <- MXCACN_IMPORTS_BULK %>%
  transmute(date = as.Date(paste0(time,"-01-01")), country = str_to_title(CTY_NAME), value = as.numeric(GEN_VAL_YR),category = "Exports")

US_TRADE <- rbind(MXCACN_EXPORTS,MXCACN_IMPORTS) %>%
  group_by(category) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(country = "USA") %>%
  mutate(category = case_when(
    category == "Exports" ~ "Imports",
    category == "Imports" ~ "Exports"
  )) %>%
  mutate(date = as.Date("2024-12-01"))
  
MXCACN_TRADE <- rbind(MXCACN_EXPORTS,MXCACN_IMPORTS) %>%
  rbind(.,US_TRADE) %>%
  mutate(value = case_when(
    country == "Mexico" ~ value / 1848125000000,
    country == "Canada" ~ value / 2214796000000,
    country == "China"  ~ value / 18273357000000,
    country == "USA"  ~ value / 29167779000000,
    TRUE               ~ value
  )) %>%
  mutate(category = factor(category, levels = rev(c("Exports","Imports")))) %>%
  mutate(country = factor(country, levels = rev(c("Mexico","Canada","USA","China"))))

EXPOSURE_TO_US_TRADE_WARS <- ggplot(data = MXCACN_TRADE, aes(x = country, y = value, fill = category)) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab(NULL) +
  ylab("% of 2024 GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.5), expand = c(0,0)) +
  ggtitle("Exposure to US Trade Wars, % of GDP") +
  labs(caption = "Graph created by @JosephPolitano using Census and IMF Data") +
  scale_fill_manual(name= "Trade with USA\n(or USA trade with MX,CN,CA)\n% of GDP",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Exports","Imports")) +
  theme_apricitas + theme(legend.position = c(.75,.35), axis.text.y = element_text(size = 20, color = "white"), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 29)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = EXPOSURE_TO_US_TRADE_WARS, "Exposure to US Trade Wars Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

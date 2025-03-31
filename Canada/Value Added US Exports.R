
VALUE_ADDED_IN_EXPORTS_BULK <- statcan_data("12-10-0100-01", "eng") 

VALUE_ADDED_IN_GOODS_EXPORTS <- VALUE_ADDED_IN_EXPORTS_BULK %>%
  filter(GEO=="Canada" & `Country Code`=="United States" & `Value added exports variable` == "Value added exports") %>%
  filter(Industry %in% c("Crop and animal production [BS11A]","Forestry and logging [BS113]","Fishing, hunting and trapping [BS114]","Support activities for agriculture and forestry [BS115]","Mining, quarrying, and oil and gas extraction [BS210]","Utilities [BS220]","Manufacturing [BS3A0]")) %>%
  mutate(Industry = case_when(
    Industry %in% c(
      "Crop and animal production [BS11A]",
      "Fishing, hunting and trapping [BS114]",
      "Forestry and logging [BS113]",
      "Support activities for agriculture and forestry [BS115]"
    ) ~ "Agriculture & Forestry",
    Industry %in% c(
      "Mining, quarrying, and oil and gas extraction [BS210]",
      "Utilities [BS220]"
    ) ~ "Oil, Energy, & Mining",
    Industry %in% "Manufacturing [BS3A0]" ~ "Manufacturing",
    TRUE ~ NA_character_
  )) %>%
  group_by(REF_DATE,Industry) %>%
  summarise(VALUE = sum(VALUE, na.rm = TRUE)) %>%
  pivot_wider(names_from = Industry, values_from = VALUE)

VALUE_ADDED_IN_TOTAL_EXPORTS <- VALUE_ADDED_IN_EXPORTS_BULK %>%
  filter(GEO=="Canada" & `Country Code`=="United States" & `Value added exports variable` == "Value added exports", Aggregation == "Summary level") %>%
  filter(Industry == "Total industries") %>%
  group_by(REF_DATE) %>%
  summarise(VALUE = sum(VALUE, na.rm = TRUE))


TOTAL_VALUE_ADDED <- VALUE_ADDED_IN_EXPORTS_BULK %>%
  filter(GEO=="Canada" & `Value added exports variable` == "Value added") %>%
  filter(Industry == "Total industries" & Aggregation == "Summary level")

VALUE_ADDED_US_EXPORTS_MERGE <- merge(VALUE_ADDED_IN_GOODS_EXPORTS,VALUE_ADDED_IN_TOTAL_EXPORTS, by = "REF_DATE") %>%
  mutate(`Services Industries` = VALUE-`Agriculture & Forestry`-`Manufacturing`-`Oil, Energy, & Mining`) %>%
  select(-VALUE) %>%
  merge(.,TOTAL_VALUE_ADDED, by = "REF_DATE") %>%
  transmute(date = REF_DATE, `Agriculture & Forestry` = `Agriculture & Forestry`/VALUE, Manufacturing = Manufacturing/VALUE, `Oil, Energy, & Mining` = `Oil, Energy, & Mining`/VALUE, `Service Industries` = `Services Industries`/VALUE) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels = rev(c("Manufacturing","Service Industries","Oil, Energy, & Mining","Agriculture & Forestry"))))

VALUE_ADDED_US_EXPORTS_GRAPH <- ggplot(data = VALUE_ADDED_US_EXPORTS_MERGE, aes(x = date, y = value, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Value Add in Canadian Exports to US, % of Canadian GDP") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.05,0.1,0.15,.2,.25), limits = c(0,.25), expand = c(0,0)) +
  ggtitle("Canadian Value-Add in Exports to US") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data", subtitle = "Exports to the US Support Much of Canada's Economy, Especially Via Manufacturing & Energy") +
  theme_apricitas + theme(legend.position = c(.5,.80)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "Value Add in US-Bound Exports, Percent of Canadian GDP",values = rev(c("#FFE98F","#EE6055","#00A99D","#9A348E"))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 0-(.3*.25), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = VALUE_ADDED_US_EXPORTS_GRAPH, "Value Added US Exports Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


VALUE_ADDED_IN_EXPORTS_PROVINCE <- VALUE_ADDED_IN_EXPORTS_BULK %>%
  filter(REF_DATE == "2021-01-01") %>%
  filter(GEO!="Canada" & `Country Code`=="United States" & `Value added exports variable` == "Value added exports") %>%
  filter(Industry == "Total industries" & Aggregation == "Summary level") %>%
  select(GEO, VALUE)

TOTAL_VALUE_ADDED_PROVINCE <- VALUE_ADDED_IN_EXPORTS_BULK %>%
  filter(REF_DATE == "2021-01-01") %>%
  filter(GEO!="Canada" & `Value added exports variable` == "Value added") %>%
  filter(Industry == "Total industries" & Aggregation == "Summary level") %>%
  select(GEO, VALUE)

VALUE_ADDED_US_EXPORTS_PROVINCE_MERGE <- merge(VALUE_ADDED_IN_EXPORTS_PROVINCE,TOTAL_VALUE_ADDED_PROVINCE, by = "GEO") %>%
  transmute(GEO, value = VALUE.x/VALUE.y) %>%
  mutate(label = paste0(round(value, 2) * 100, "%")) %>%
  merge(PROV,., by.y = "GEO", by.x = "PRENAME") 


VALUE_ADDED_US_EXPORTS_PROVINCE <- ggplot(VALUE_ADDED_US_EXPORTS_PROVINCE_MERGE) + 
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  #coord_sf(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  geom_prov(data = VALUE_ADDED_US_EXPORTS_PROVINCE_MERGE, fill = "value", colour = "black", size = 0.1) +
  scale_fill_gradientn(colors = rev(c("#EE6055","#F5B041","#FFE98F", "#AED581","#00A99D","#3083DC")),label = scales::percent_format(accuracy = 1),breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45), expand = c(0,0)) +
  ggtitle("Value-Added in US-Bound Exports\n        % of Province GDP") +
  theme(plot.title = element_text(size = 24)) +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data as of 2021") +
  labs(fill = NULL) +
  theme_apricitas + theme(legend.position = "right", panel.grid.major=element_blank(), axis.line = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(),plot.margin= grid::unit(c(t = 0.15, r = 0, b= 0,l = -0.5), "in"), legend.key = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
#geom_text(data = TRADE_EXPOSURE_GDP_STATE_LABELS, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = paste0(state_abbv,"\n",round(trade_GDP_share,1),"%")), size = 3, color = "white", check_overlap = TRUE)
#geom_text(aes(x = x, y = y, label = paste0(state_abbv, "\n",round(trade_GDP_share*100,1),"%")), size = 3, check_overlap = TRUE, color = "white")# Add state labels

ggsave(dpi = "retina",plot = VALUE_ADDED_US_EXPORTS_PROVINCE, "Value Added US Exports Province Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


EMPLOYMENT_CONNECTED_TO_VALUE_ADDED <- VALUE_ADDED_IN_EXPORTS_BULK %>%
  filter(GEO=="Canada" & `Country Code`=="United States" & `Value added exports variable` %in% c("Direct jobs embodied in exports","Total jobs embodied in exports")) %>%
  filter(Industry == "Total industries", Aggregation == "Summary level") %>%
  select(REF_DATE, `Value added exports variable`,VALUE) %>%
  transmute(date = REF_DATE, category = `Value added exports variable`, value = VALUE) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  mutate(date, `Direct Jobs` = `Direct jobs embodied in exports`, `Indirect Jobs` = `Total jobs embodied in exports`-`Direct jobs embodied in exports`)
  
  
EXPORT_ORIENTED_JOB_GROWTH_GRAPH <- ggplot() + #plotting Labor Shortage
  geom_line(data=EMPLOYMENT_CONNECTED_TO_VALUE_ADDED, aes(x=date,y= `Total jobs embodied in exports`/1000000, color= "Total Canadian Jobs Embodied in Exports to the US"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = .1, suffix = "M"), limits = c(1.8,2.7), expand = c(0,0)) +
  ylab("Number") +
  ggtitle("Canadian Export-Oriented Job Growth") +
  labs(caption = "Graph created by @JosephPolitano using Statistics Canada data",subtitle = "Today More than 2.6M Canadian Jobs are Embodied in Exports to the US, A Record High") +
  theme_apricitas + theme(legend.position = c(.525,.9)) +
  scale_color_manual(name= NULL ,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2007-01-01")-(.1861*(today()-as.Date("2007-01-01"))), xmax = as.Date("2007-01-01")-(0.049*(today()-as.Date("2007-01-01"))), ymin = 1.8-(.3*.9), ymax = 1.8) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = EXPORT_ORIENTED_JOB_GROWTH_GRAPH, "Export Oriented Job Growth.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

  
  
  
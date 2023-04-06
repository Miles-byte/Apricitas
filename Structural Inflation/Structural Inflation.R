pacman::p_load(Quandl,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

ISMProductionLead <- Quandl("ISM/BUY_PROD_MAT") %>%
  select(Date, `Average Days`)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

ISM_LEAD_MANUAL <- data.frame(Date = seq.Date(from = as.Date("2022-04-01"), to = as.Date("2023-01-01"), by = "month"),
           `Average Days` = c(100,99,100,100,96,94,93,84,85,87))

colnames(ISM_LEAD_MANUAL) <- c("Date","Average Days")

ISMProductionLead <- rbind(ISMProductionLead,ISM_LEAD_MANUAL)

Quandl.api_key("fEweXnVbZ38SxxcytVJY")

ISMProductionLead_Graph <- ggplot() + 
  geom_line(data = subset(ISMProductionLead, Date > as.Date("2017-12-01")), aes(x = Date, y = `Average Days`, color = "Average Lead Times, Production Materials"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(limits = c(50,100), breaks = c(50,60,70,80,90,100), expand = c(0,0)) +
  ylab("Days") +
  ggtitle("The Supply Chain Crisis") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Input Lead Times are Falling Off Record Highs") +
  theme_apricitas + theme(legend.position = c(.50,.12)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#A7ACD9","#9A348E","#EE6055","#3083DC","RED")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 50-(.3*50), ymax = 50) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ISMProductionLead_Graph, "ISM Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SHORT_SUPPLY <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Structural%20Inflation/Structural_Inflation.csv") %>%
  select(-total) %>%
  drop_na() %>%
  mutate(date = as.Date(date))

colnames(SHORT_SUPPLY) <- c("date","1 Month","2-3 Months","4-6 Months","7-12 Months","13-24 Months","25+ Months")

SHORT_SUPPLY <- pivot_longer(SHORT_SUPPLY, cols = `1 Month`:`25+ Months`) %>%
  mutate(name =  factor(name, levels = c("1 Month","2-3 Months","4-6 Months","7-12 Months","13-24 Months","25+ Months")))

ISM_SHORT_SUPPLY_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_bar(data = SHORT_SUPPLY, aes(x = date, y = value, fill = name), color = NA, size = 0, stat= "identity") +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(0,40), breaks = c(0,10,20,30,40), expand = c(0,0)) +
  ylab("Number") +
  ggtitle("Commodities in Short Supply") +
  labs(caption = "Graph created by @JosephPolitano",subtitle = "Semiconductors, Electrical Components, and Electronic Components Have Persistent Supply Issues") +
  theme_apricitas + theme(legend.position = c(.35,.75)) +
  scale_fill_manual(name= "Number of Consecutive Months in Short Supply",values = c("#ea3c2e","#d12215","#a21b10","#74130c","#460b07","#170402")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 0-(.3*40), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = ISM_SHORT_SUPPLY_GRAPH, "ISM Short Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

QSPC <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Are%20Supply%20Chains%20Healing/Compiled_QSPC.csv") %>%
  mutate(date = paste(Year,"Q",Quarter)) %>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q %q"))) %>%
  mutate(Value = as.numeric(Value))

QSPC_Supply_Graph <- ggplot() + #plotting BIE
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Insufficient Supply of Materials"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Logistics/transportation constraints"), aes(x=date,y= Value/100,color= "Logistics/Transportation Constraints"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.50), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Healing Supply Chains") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Manufacturers Are Now Citing Materials/Labor Shortages and Logistics Constraints Less") +
  theme_apricitas + theme(legend.position = c(.40,.75)) +
  scale_color_manual(name= "US Manufacturers, Reasons for Not Running at Full Capacity",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-04-01")))), ymin = 0-(.3*.50), ymax = 0) +
  coord_cartesian(clip = "off")

QSPC_Supply_Selected_Graph <- ggplot() + #plotting BIE
  geom_line(data=subset(QSPC, Sector == "325" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Chemical Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "334" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Computers/Electronics Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "335" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Electrical Equipment/Appliances Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "336" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Transportation Equipment Manufacturing"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "333" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Machinery Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.70), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Still Stressed Out") +
  labs(caption = "Graph created by @JosephPolitano using US Census data",subtitle = "Key Industries Still Cite Materials Shortage and Logistics Constraints for Underutilization") +
  theme_apricitas + theme(legend.position = c(.40,.75)) +
  scale_color_manual(name= "% Citing Materials Shortages for Not Running at Full Capacity",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Computers/Electronics Manufacturing","Transportation Equipment Manufacturing","Machinery Manufacturing","Electrical Equipment/Appliances Manufacturing","Chemical Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2014-01-01")-(.1861*(today()-as.Date("2014-01-01"))), xmax = as.Date("2014-01-01")-(0.049*(.1861*(today()-as.Date("2014-04-01")))), ymin = 0-(.3*.70), ymax = 0) +
  coord_cartesian(clip = "off")
  
QSPC_Demand_Graph <- ggplot() + #plotting QSPC Demand
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient orders"), aes(x=date,y= Value/100,color= "Insufficient Orders"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of materials"), aes(x=date,y= Value/100,color= "Insufficient Supply of Materials"), size = 1.25) +
  geom_line(data=subset(QSPC, Sector == "All" & Measure == "Insufficient supply of labor"), aes(x=date,y= Value/100,color= "Insufficient Supply of Labor"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.90), breaks = c(0,.20,.40,.60,.80), expand = c(0,0)) +
  ylab("Index") +
  ggtitle("Supply and Demand Chains") +
  labs(caption = "Graph created by @JosephPolitano using Census data",subtitle = "Firms Aren't Complaining About a Shortage of Orders As Much as Before the Pandemic") +
  theme_apricitas + theme(legend.position = c(.40,.5)) +
  scale_color_manual(name= "US Manufacturers, Reasons for Not Running at Full Capacity",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2013-01-01")-(.1861*(today()-as.Date("2013-01-01"))), xmax = as.Date("2013-01-01")-(0.049*(.1861*(today()-as.Date("2013-04-01")))), ymin = 0-(.3*.90), ymax = 0) +
  coord_cartesian(clip = "off")

QSPC_Category <- QSPC %>%
  mutate(Sector = gsub("311","Food",Sector)) %>%
  mutate(Sector = gsub("312","Beverages/Tobacco",Sector)) %>%
  mutate(Sector = gsub("313","Textile",Sector)) %>%
  mutate(Sector = gsub("314","Textile Products",Sector)) %>%
  mutate(Sector = gsub("315","Apparel",Sector)) %>%
  mutate(Sector = gsub("316","Leather Products",Sector)) %>%
  mutate(Sector = gsub("321","Wood Products",Sector)) %>%
  mutate(Sector = gsub("322","Paper",Sector)) %>%
  mutate(Sector = gsub("323","Printing & Related",Sector)) %>%
  mutate(Sector = gsub("324","Petroleum & Coal Products",Sector)) %>%
  mutate(Sector = gsub("325","Chemical",Sector)) %>%
  mutate(Sector = gsub("326","Plastics & Rubber Products",Sector)) %>%
  mutate(Sector = gsub("327","Nonmetallic Mineral Products",Sector)) %>%
  mutate(Sector = gsub("331","Primary Metal",Sector)) %>%
  mutate(Sector = gsub("332","Fabricated Metal Products",Sector)) %>%
  mutate(Sector = gsub("333","Machinery",Sector)) %>%
  mutate(Sector = gsub("334","Computer & Electronic Products",Sector)) %>%
  mutate(Sector = gsub("335","Electrical Equipment & Appliances",Sector)) %>%
  mutate(Sector = gsub("336","Transportation Equipment",Sector)) %>%
  mutate(Sector = gsub("337","Furniture & Related",Sector)) %>%
  mutate(Sector = gsub("339","Miscellaneous",Sector)) %>%
  subset(date == as.Date("2022-10-01")) %>%
  subset(Measure == "Insufficient supply of materials") %>%
  subset(Sector != "All") %>%
  mutate(Sector = factor(Sector,levels = c("Textile","Textile Products","Leather Products","Primary Metal","Paper","Nonmetallic Mineral Products","Petroleum & Coal Products","Plastics & Rubber Products","Fabricated Metal Products","Food","Beverages/Tobacco","Apparel","Chemical","Miscellaneous","Wood Products","Electrical Equipment & Appliances","Printing & Related","Furniture & Related","Machinery","Transportation Equipment","Computer & Electronic Products")))


QSPC_Category_graph <- ggplot(data = QSPC_Category, aes(x = Sector, y = Value/100, fill = "Insufficient Supply of Materials, Q4 2022")) + #plotting Deposits, Insured and Uninsured
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab("Sector") +
  ylab("Percent of Manufacturers") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,.20,.4,.6), limits = c(0,.62), expand = c(0,0)) +
  ggtitle("Capacity Constraints") +
  labs(caption = "Graph created by @JosephPolitano using US Census data", subtitle = "Cars, Computers, and Chips Have Biggest Supply Chain Issues") +
  theme_apricitas + theme(legend.position = c(.63,.1)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= "% of US Manufacturers Citing",values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93")) +
  coord_flip()

ggsave(dpi = "retina",plot = QSPC_Category_graph, "QSPC Category Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

ggsave(dpi = "retina",plot = QSPC_Supply_Graph, "QSPC Supply Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = QSPC_Supply_Selected_Graph, "QSPC Selected Supply Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")
ggsave(dpi = "retina",plot = QSPC_Demand_Graph, "QSPC Demand Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

BIE_INF_COMP <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Labor%20Shortage/BIE_inf_comp.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate_if(is.character, as.numeric) %>%
  drop_na()

BIE_INF_graph <- ggplot() + #plotting Wage Growth
  annotate("hline", y = 0.00, yintercept = 0.00, color = "white", size = 0.5) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= labor_costs,color= "Labor Costs"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= sales_level,color= "Sales Levels"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= productivity,color= "Productivity"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= non_labor_costs,color= "Non-Labor Costs"), size = 1.25) +
  geom_line(data=BIE_INF_COMP, aes(x=date,y= margin_adjustment,color= "Margin Adjustment"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(-10,85), expand = c(0,0)) +
  ylab("Index, 0 = No Influence") +
  ggtitle("Fading Cost Pushes") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed Data",subtitle = "Businesses Expect Lower Inflation Over the Next Year—Especially From Labor/Nonlabor Costs") +
  theme_apricitas + theme(legend.position = c(.52,.78)) +
  scale_color_manual(name= "Business Forecasts of Future Influence of Different Factors on Their Prices",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Labor Costs","Non-Labor Costs","Margin Adjustment","Sales Levels","Productivity")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2011-12-18")-(.1861*(today()-as.Date("2011-12-18"))), xmax = as.Date("2011-12-18")-(0.049*(today()-as.Date("2011-12-18"))), ymin = -10-(.3*95), ymax = -10) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = BIE_INF_graph, "BIE Contrib Influence.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

CONSTRUCTION_VALUE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Structural%20Inflation/Construction_Mfg_Historical_Value.csv") %>%
  mutate(date = as.Date(date)) %>%
  subset(date >= as.Date("2000-01-01"))

CONSTRUCTION_VALUE_graph <- ggplot() + #plotting Wage Growth
  geom_line(data=CONSTRUCTION_VALUE, aes(x=date,y= Chemical/1000,color= "Chemical Manufacturing"), size = 1.25) +
  geom_line(data=CONSTRUCTION_VALUE, aes(x=date,y= Transportation.equipment/1000,color= "Transportation Equipment Manufacturing"), size = 1.25) +
  geom_line(data=CONSTRUCTION_VALUE, aes(x=date,y= Computer..electronic..electrical/1000,color= "Computer, Electronic, and Electrical Manufacturing"), size = 1.25) +
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,65), expand = c(0,0)) +
  ylab("Billions of Dollars, Annual Rate") +
  ggtitle("Chips, Cars, and Chemical Capacity") +
  labs(caption = "Graph created by @JosephPolitano using US Census Data",subtitle = "US Manufacturing Construction Spending, Especially Computer/Electronic Manufacturing, is Up") +
  theme_apricitas + theme(legend.position = c(.52,.78)) +
  scale_color_manual(name= "Nominal Value of Construction Put in Place, US",values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E"), breaks = c("Computer, Electronic, and Electrical Manufacturing","Chemical Manufacturing","Transportation Equipment Manufacturing")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2000-01-01")-(.1861*(today()-as.Date("2000-01-01"))), xmax = as.Date("2000-01-01")-(0.049*(today()-as.Date("2000-01-01"))), ymin = 0-(.3*65), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CONSTRUCTION_VALUE_graph, "Construction Value.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Personal_Income_Graph <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = DSPI, aes(x=date, y = value/1000, color = "Personal Income"), size = 1.25) + 
  geom_line(data = POUT, aes(x=date, y = value/1000 , color = "Personal Outlays"), size = 1.25) + 
  geom_line(data = DSPITrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Income Growth Trend"), size = 1.25, linetype = "dashed") + 
  geom_line(data = POUTTrend, aes(x=date, y = trend/1000, color = "Pre-Covid 4% Outlays Growth Trend"), size = 1.25, linetype = "dashed") + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 0.5),limits = c(12.5,22.5), breaks = c(12.5,15,17.5,20,22.5), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Bottom Line") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Income and Spending are Well Above Trend") +
  theme_apricitas + theme(legend.position = c(.30,.85)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E"),guide=guide_legend(override.aes=list(linetype=c(1,1,2,2), lwd = c(1.25,1.25,.75,.75), size = c(1.25,1.25,.75,.75)))) +
  theme(legend.key.width =  unit(1.4, "cm")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 12.5-(.3*10), ymax = 12.5) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Personal_Income_Graph, "Personal Income.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Social_Security <- fredr(series_id = "W823RC1", observation_start = as.Date("2018-01-01"))
Personal_Taxes <- fredr(series_id = "W055RC1", observation_start = as.Date("2018-01-01"))

INDEX_SPIRAL_GRAPH <- ggplot() + #plotting personal income and outlays against income and outlays 4% pre-covid trendlines
  geom_line(data = Social_Security, aes(x=date, y = value/1000, color = "Social Security Payments"), size = 1.25) + 
  geom_line(data = Personal_Taxes, aes(x=date, y = value/1000 , color = "Personal Current Taxes Excluding Social Insurance Taxes"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "T", accuracy = 1),limits = c(0,3.5), breaks = c(0,1,2,3), expand = c(0,0)) +
  ylab("Trillions of Dollars") +
  ggtitle("The Index Impulse") +
  labs(caption = "Graph created by @JosephPolitano using BEA data",subtitle = "Taxes are Down and Social Security Payments are Up Thanks in Part to Inflation Indexing") +
  theme_apricitas + theme(legend.position = c(.50,.5)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#FFE98F","#00A99D","#EE6055","#FFE98F","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*3.5), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = INDEX_SPIRAL_GRAPH, "Index Spiral.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

Uncertainty <- fredr(series_id = "ATLSBUSRGUP", observation_start = as.Date("2018-01-01"))

Uncertainty_Graph <- ggplot() + #plotting BIE
  geom_line(data = Uncertainty, aes(x=date, y = value/100, color = "Revenue Uncertainty, Atlanta Fed Survey of Business Uncertainty"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,.07), breaks = c(0,.01,0.02,0.03,0.04,0.05,0.06,0.07), expand = c(0,0)) +
  ylab("% of Plants Citing This Reason") +
  ggtitle("Uncertain Times") +
  labs(caption = "Graph created by @JosephPolitano using Atlanta Fed data",subtitle = "US Businesses Have Been Highly Uncertain About Their Outlook Since the Start of the Pandemic") +
  theme_apricitas + theme(legend.position = c(.50,.25)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(.1861*(today()-as.Date("2018-01-01")))), ymin = 0-(.3*.07), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = Uncertainty_Graph, "Uncertainty.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

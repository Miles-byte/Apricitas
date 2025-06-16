DHS_DTS_TGA_DEPOSITS <- read.csv("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/dts/deposits_withdrawals_operating_cash?format=csv&page[size]=10000&fields=record_date,transaction_type,transaction_catg,transaction_mtd_amt&filter=transaction_type:eq:Deposits,transaction_catg:in:(Customs%20and%20Certain%20Excise%20Taxes,DHS%20-%20Customs%20and%20Certain%20Excise%20Taxes)") %>%
  transmute(date = as.Date(record_date), value = transaction_mtd_amt) %>%
  group_by(year = year(date), month = month(date)) %>% 
  filter(date == max(date)) %>%
  filter(!(year == year(Sys.Date()) & month == month(Sys.Date())))

DHS_MTS_TGA_DEPOSITS <- read.csv("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/accounting/mts/mts_table_4?format=csv&page[size]=10000&fields=record_date,classification_desc,current_month_gross_rcpt_amt&filter=classification_desc:eq:Customs%20Duties") %>%
  transmute(date = as.Date(record_date), value = current_month_gross_rcpt_amt) %>%
  group_by(year = year(date), month = month(date)) %>% 
  filter(date == max(date)) %>%
  filter(!(year == year(Sys.Date()) & month == month(Sys.Date())))



DHS_TGA_DEPOSITS_GRAPH <- ggplot() + #plotting components of annual inflation
  geom_line(data = DHS_DTS_TGA_DEPOSITS, aes(x = date, y = (value*12)/1000, color = "US Tariffs & DHS Excise Taxes Paid, Monthly Annualized"), size = 1.25) +
  #geom_line(data = DHS_MTS_TGA_DEPOSITS, aes(x = date, y = (value*12)/1000000000, color = "monthly"), size = 1.25) +
  #geom_line(data=GROSS_TRADE_BULK, aes(x=time,y= CAL_DUT_MO*12/1000000000,color= "US Tariffs Collected\nNot Seasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,(ceiling(max((DHS_DTS_TGA_DEPOSITS$value * 12) / 1000, na.rm = TRUE) / 50) * 50)), breaks = c(50,100,150,200,250,300,350,400), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Americans are Paying Billions in Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(0.45,0.85)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2005-10-01")-(.1861*(today()-as.Date("2005-10-01"))), xmax = as.Date("2005-10-01")-(0.049*(today()-as.Date("2005-10-01"))), ymin = 0-(.3*(ceiling(max((DHS_DTS_TGA_DEPOSITS$value * 12) / 1000, na.rm = TRUE) / 50) * 50)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DHS_TGA_DEPOSITS_GRAPH, "DHS TGA DEPOSITS GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

DHS_MTS_DEPOSITS_GRAPH <- ggplot() + #plotting components of annual inflation
  #geom_line(data = DHS_DTS_TGA_DEPOSITS, aes(x = date, y = (value*12)/1000, color = "US Tariffs & DHS Excise Taxes Paid, Monthly Annualized"), size = 1.25) +
  geom_line(data = DHS_MTS_TGA_DEPOSITS, aes(x = date, y = (value*12)/1000000000, color = "US Tariffs Paid, Monthly Annualized"), size = 1.25) +
  #geom_line(data=GROSS_TRADE_BULK, aes(x=time,y= CAL_DUT_MO*12/1000000000,color= "US Tariffs Collected\nNot Seasonally Adjusted Annual Rate"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"),limits = c(0,(ceiling(max((DHS_MTS_TGA_DEPOSITS$value * 12) / 1000000000, na.rm = TRUE) / 50) * 50)), breaks = c(50,100,150,200,250,300,350,400), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Americans are Paying Billions in Tariffs") +
  labs(caption = "Graph created by @JosephPolitano using US Treasury data",subtitle = "Costs are Rising as Trump Imposes Massive Tariffs on Major US Trading Partners") +
  theme_apricitas + theme(legend.position = c(0.45,0.85)) +
  scale_color_manual(name = NULL, values = c("#FFE98F","#00A99D","#EE6055","#6A4C93","#3083DC","#A7ACD9","#9A348E")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2015-03-01")-(.1861*(today()-as.Date("2015-03-01"))), xmax = as.Date("2015-03-01")-(0.049*(today()-as.Date("2015-03-01"))), ymin = 0-(.3*(ceiling(max((DHS_MTS_TGA_DEPOSITS$value * 12) / 1000000000, na.rm = TRUE) / 50) * 50)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DHS_MTS_DEPOSITS_GRAPH, "DHS TGA DEPOSITS MONTHLY GRAPH.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

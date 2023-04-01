pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

DEP_DOM_DATA_SIGNATURE <- read.csv("https://banks.data.fdic.gov/api/financials?filters=RSSDID%3A%20494261%20OR%20RSSDID%3A%202942690%20OR%20RSSDID%3A%203138146%20OR%20RSSDID%3A%204114567%20OR%20RSSDID%3A%202354985%20OR%20RSSDID%3A%203637685&fields=CERT%2CNAME%2CRISDATE%2CASSET%2CDEP%2CDEPINS%2CDEPINSR%2CSCHAR%2CSCHA%2CASSTLTR&sort_by=RISDATE&sort_order=DESC&limit=10000&offset=0&format=csv&download=true&filename=data_file") %>%
  mutate(RISDATE = ymd(RISDATE)) %>%
  select(DEP,DEPINS,RISDATE,NAME) %>%
  subset(NAME == "SIGNATURE BANK") %>%
  transmute(date = RISDATE, `Uninsured Deposits` = DEP-DEPINS, `FDIC Insured Deposits` = DEPINS) %>%
  pivot_longer(cols = c(`Uninsured Deposits`,`FDIC Insured Deposits`)) %>%
  subset(date > as.Date("2017-12-01")) %>%
  mutate(name = factor(name,levels = c("Uninsured Deposits","FDIC Insured Deposits")))

DEP_DOM_DATA_SIGNATURE_graph <- ggplot(data = DEP_DOM_DATA_SIGNATURE, aes(x = date, y = value/1000000, fill = name)) + #plotting Deposits, Insured and Uninsured
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Billions of Dollars") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "B"), breaks = c(0,50,100), limits = c(0,110), expand = c(0,0)) +
  ggtitle("Uninsured Deposits at Signature Bank") +
  labs(caption = "Graph created by @JosephPolitano using FDIC data", subtitle = "Signature Saw Growth During the Crypto Boom, But Has Started Shedding Deposits Recently") +
  theme_apricitas + theme(legend.position = c(.25,.825)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#EE6055","#00A99D","#A7ACD9","#9A348E","#3083DC","#6A4C93"), breaks = c("FDIC Insured Deposits","Uninsured Deposits")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 0-(.3*110), ymax = 0) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = DEP_DOM_DATA_SIGNATURE_graph, "Dep Dom Data Signature.png", type = "cairo-png") #cairo gets rid of anti aliasing


CMBS <- tq_get("CMBS", from = "2018-01-01")

CMBS_TOTAL_RETURN_graph <- ggplot() + #plotting loan performance data
  geom_line(data=CMBS, aes(x=date,y= (adjusted/adjusted[1]*100)/100-1,color= "iShares Commercial Mortgage Backed Securities ETF (CMBS) Total Return"), size = 1.25)+ 
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_area(stat = "identity", position = "stack", color = NA) +
  xlab("Date") +
  ylab("Total Return, Percent") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.05,0,.05,.1,.15,.20), limits = c(-0.05,.2), expand = c(0,0)) +
  ggtitle("Loan Losses") +
  labs(caption = "Graph created by @JosephPolitano using Yahoo! Finance data", subtitle = "Commercial Real Estate Loans Have Had Negative Returns in Recent Years As Interest Rates Rose") +
  theme_apricitas + theme(legend.position = c(.5,.97)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = -0.05-(.3*.25), ymax = -0.05) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CMBS_TOTAL_RETURN_graph, "CMBS Graph.png", type = "cairo-png") #cairo gets rid of anti aliasing

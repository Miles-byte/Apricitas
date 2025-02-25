CHINA_DE_MINIMIS <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/refs/heads/main/Trade%20Rapid%20Response/CHINA_DE_MINIMIS_EXPORTS.csv") %>%
  transmute(date = as.Date(paste0(time,"-01-01")),value = as.numeric(gsub(",", "", dollar)))

CHINA_DE_MINIMIS_Graph <- ggplot() + #plotting integrated circuits exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_line(data=CHINA_DE_MINIMIS, aes(x=date,y= value/1000000000,color= "Chinese 'De Minimis' Exports to the US"), size = 1.25) + 
  xlab("Date") +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B", accuracy = 1),limits = c(0,25), breaks = c(0,5,10,15,20,25), expand = c(0,0)) +
  ylab("Billions of Dollars") +
  ggtitle("Trump Ends China's De Minimis Exemption") +
  labs(caption = "Graph created by @JosephPolitano using PRC GACC data. De Minimis Shipments are HS Code 9804",subtitle = "Trump's EO Ended the 'De Minimis' Exemptions for Small Packages—Roughly $23B in Imports") +
  theme_apricitas + theme(legend.position = c(.35,.89), plot.title = element_text(size = 27)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2016-01-01")-(.1861*(today()-as.Date("2016-01-01"))), xmax = as.Date("2016-01-01")-(0.049*(today()-as.Date("2016-01-01"))), ymin = 0-(.3*(25)), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CHINA_DE_MINIMIS_Graph, "China De Minimis Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE

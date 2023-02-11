pacman::p_load(seasonal,stringi,ggpubr,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

install_github("keberwein/blscrapeR")
library(blscrapeR)

MULTIPLE_JOBHOLDERS <- fredr(series_id = "LNS12026619",observation_start = as.Date("2018-01-01")) #downloading Multiple Jobholders data
SELF_EMPLOYED <- fredr(series_id = "LNS12027714",observation_start = as.Date("2018-01-01")) #downloading Self Employed data
AGRICULTURAL <- fredr(series_id = "LNS12034560",observation_start = as.Date("2018-01-01")) #downloading Agricultural Workers

#PAYEMS_OLD <- fredr(series_id = "PAYEMS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Nonfarm Payrolls
#ELEV_OLD <- fredr(series_id = "CE16OV",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Employment Levels
#CPSADJ_OLD <- bls_api("LNS16000000", startyear = 2019) %>% #headline cpiadj
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

PAYEMS <- fredr(series_id = "PAYEMS",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Nonfarm Payrolls
ELEV <- fredr(series_id = "CE16OV",observation_start = as.Date("2019-01-01"),realtime_start = NULL, realtime_end = NULL) #Employment Levels
CPSADJ <- bls_api("LNS16000000", startyear = 2019) %>% #headline cpiadj
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))

PAYEMS <- bls_api("CES0000000001", startyear = 2019) %>% #headline cpiadj
  mutate(date = as.Date(as.yearmon(paste(periodName, year), "%b %Y")))


write.csv(CPSADJ_OLD, "CPSADJ_OLD.csv")



MULTIPLE_32022 <- fredr(series_id = "LNS12026619",observation_start = as.Date("2022-04-01"), units = "chg")%>%
  select(value,date) %>%
  mutate(CUMSUM = cumsum(value))%>%
  mutate(source = "Multiple Jobholders")
SELF_32022 <- fredr(series_id = "LNS12027714",observation_start = as.Date("2022-04-01"), units = "chg")%>%
  select(value,date) %>%
  mutate(CUMSUM = cumsum(value))%>%
  mutate(source = "Unincorporated Self Employed")
AGRICULTURAL_32022 <- fredr(series_id = "LNS12034560",observation_start = as.Date("2022-04-01"), units = "chg")%>%
  select(value,date) %>%
  mutate(CUMSUM = cumsum(value))%>%
  mutate(source = "Employment Level, Agricultural and Related Industries")


PAYEMS_32022 <- fredr(series_id = "PAYEMS",observation_start = as.Date("2022-04-01"), units = "chg")%>%
  select(value,date) %>%
  mutate(CUMSUM = cumsum(value))%>%
  mutate(source = "Nonfarm Payrolls")
ELEV_32022 <- fredr(series_id = "CE16OV",observation_start = as.Date("2022-04-01"), units = "chg") %>%
  select(value,date) %>%
  mutate(CUMSUM = cumsum(value))%>%
  mutate(source = "Employment Level")

#
CPSADJ_32022 <- subset(CPSADJ, date > as.Date("2022-02-01")) %>%
  mutate(value = rev(value)) %>%
  mutate(date = rev(date)) %>%
  select(value,date) %>%
  mutate(value = c(value-lag(.$value))) %>%
  subset(date > as.Date("2022-03-01")) %>%
  mutate(CUMSUM = cumsum(value)) %>%
  mutate(source = "Household Survey Adjusted to Nonfarm Payrolls Concepts")

EMPLOYMENT_Revisions_Graph <- ggplot() + #CPS with NFP adjusted concepts
  geom_line(data = PAYEMS_OLD, aes(x=date, y = value/1000, color = "Nonfarm Payrolls—Unrevised"), linetype = "dashed", size = 1.5) + 
  geom_line(data = PAYEMS, aes(x=date, y = value/1000, color = "Nonfarm Payrolls—Revised"), size = 1.25) + 
  geom_line(data = CPSADJ, aes(x=date, y = value/1000, color = "Household Survey Adjusted to Nonfarm Payrolls Concepts—Revised"), size = 1.25) + 
  geom_line(data = CPSADJ_OLD, aes(x=date, y = value/1000, color = "Household Survey Adjusted to Nonfarm Payrolls Concepts—Unrevised"), linetype = "dashed", size = 1.5) + 
  #annotate(geom = "vline", x = as.Date("2022-03-01"), xintercept = as.Date("2022-03-01"), color = "white", linetype = "dashed", size = 1.25) +
  #annotate(geom = "text", label = "March 2022", x = as.Date("2021-12-01"), y = 135, color ="white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(120,170), breaks = c(120,130,140,150,160,170), expand = c(0,0)) +
  ylab("Payrolls/Employees, Millions") +
  ggtitle("Labor Market Mystery Hour") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "Revisions Have Changed Our Understanding of the Labor Market") +
  theme_apricitas + theme(legend.position = c(.475,.87)) +
  theme(legend.key.width =  unit(.81, "cm")) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#FFE98F","#00A99D","#00A99D"),guide=guide_legend(override.aes=list(linetype=c(1,2,1,2)), linewidth = c(1.25,0.75,1.25,0.75))) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 120-(.3*50), ymax = 120) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

?guide_legend()

ggsave(dpi = "retina",plot = EMPLOYMENT_Revisions_Graph, "Employment Revisions.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


#Merging nonfarm payrolls, cps adjusted to nonfarm payrolls, and employment levels growth since March 2022
PAYEMS_ELEV_CPSADJ_32022 <- rbind(PAYEMS_32022,ELEV_32022,CPSADJ_32022) #%>%
  #subset(date > as.Date(today()-70))

DISCREPANCY_MEGA_MERGE <- rbind(PAYEMS_ELEV_CPSADJ_32022,MULTIPLE_32022,SELF_32022,AGRICULTURAL_32022) %>%
  #subset(date > as.Date(today()-70)) %>%
  select(date,CUMSUM,source) %>%
  pivot_wider(names_from=c(source), values_from = CUMSUM) %>%
  mutate(`Agriculture and Related Industries` = -`Employment Level, Agricultural and Related Industries`) %>%
  mutate(`Remaining Discrepancy`=`Nonfarm Payrolls`-`Household Survey Adjusted to Nonfarm Payrolls Concepts`) %>%
  mutate(`Other Concept Differences` = + `Household Survey Adjusted to Nonfarm Payrolls Concepts` - (`Employment Level` + `Multiple Jobholders`- `Unincorporated Self Employed` - `Employment Level, Agricultural and Related Industries`)) %>%
  mutate(`Unincorporated Self Employed` = -`Unincorporated Self Employed`)%>%
  select(-`Household Survey Adjusted to Nonfarm Payrolls Concepts`,-`Nonfarm Payrolls`,-`Employment Level`,-`Employment Level, Agricultural and Related Industries`) %>%
  pivot_longer(cols = c(`Multiple Jobholders`:`Other Concept Differences`)) %>%
  mutate(factor = as.numeric(c("1","1","1","1","1","2","2","2","2","2","3","3","3","3","3","4","4","4","4","4","5","5","5","5","5","6","6","6","6","6","7","7","7","7","7","8","8","8","8","8","9","9","9","9","9"))) %>%
  mutate(name = factor(name,levels = c("Remaining Discrepancy","Other Concept Differences","Unincorporated Self Employed","Multiple Jobholders","Agriculture and Related Industries")))
  
PAYEMS_ELEV_CPSADJ_32022_Graph <- ggplot(data = subset(PAYEMS_ELEV_CPSADJ_32022, date > today()-60), aes(x = date, y = CUMSUM/1000, fill = source)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  xlab("Date") +
  ylab("Growth Since March 2022") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(0,1,2,3), limits = c(0,3.25), expand = c(0,0)) +
  ggtitle("Growth Since March 2022") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Different Sources Give Wildly Different Stories of the Labor Market Since March") +
  theme_apricitas + theme(legend.position = c(.40,.95),axis.text.x=element_blank(), axis.title.x=element_blank()) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-07-01")-(.1861*(today()-as.Date("2022-07-01"))), xmax = as.Date("2022-07-01")-(0.049*(today()-as.Date("2022-07-01"))), ymin = -0.2-(.3*2.3), ymax = -0.2) +
  coord_cartesian(clip = "off")

TOTAL_DISCREPANCY_32022_Graph <- ggplot(data = DISCREPANCY_MEGA_MERGE, aes(x = factor, y = value/1000, fill = name)) + #plotting permanent and temporary job losers
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_col(stat = "identity", position = "stack", color = NA, width = 0.5) +
  xlab("Date") +
  ylab("Contribution to Gap") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.5), breaks = c(0,.5,1,1.5,2,2.5,3), limits = c(-0.2,3), expand = c(0,0)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9),labels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("Breaking Down the Gap") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "The Gap Between Payrolls and Household Employment is Large-as is the Discrepancy") +
  theme_apricitas + theme(legend.position = c(.23,.85)) +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#9A348E","#EE6055","#00A99D","#A7ACD9","#3083DC")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2022-07-01")-(.1861*(today()-as.Date("2022-07-01"))), xmax = as.Date("2022-07-01")-(0.049*(today()-as.Date("2022-07-01"))), ymin = -0.2-(.3*2.3), ymax = -0.2) +
  coord_cartesian(clip = "off")


MULTIPLE_JOBHOLDERS_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=MULTIPLE_JOBHOLDERS, aes(x=date,y= value/1000,color= "Multiple Jobholders"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(5,6,7,8,9,10), limits = c(5,9), expand = c(0,0)) +
  ggtitle("Doubling Up") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Multiple-Jobholding is Up After Dropping Precipitously at the Start of COVID") +
  theme_apricitas + theme(legend.position = c(.70,.87)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 5-(.3*4), ymax = 5) +
  coord_cartesian(clip = "off")

SELF_EMPLOYED_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=SELF_EMPLOYED, aes(x=date,y= value/1000,color= "Unincorporated Self Employed"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 1), breaks = c(8,9,10), limits = c(8,10.5), expand = c(0,0)) +
  ggtitle("Freelance Flops") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Self-Employment is Falling After Spiking at the Beginning of COVID") +
  theme_apricitas + theme(legend.position = c(.30,.89)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 8-(.3*2.5), ymax = 8) +
  coord_cartesian(clip = "off")

AGRICULTURAL_Graph <- ggplot() + #plotting permanent and temporary job losers
  geom_line(data=AGRICULTURAL, aes(x=date,y= value/1000,color= "Employment Level, Agricultural and Related Industries"), size = 1.25)+ 
  xlab("Date") +
  ylab("Millions of People") +
  scale_y_continuous(labels = scales::number_format(suffix = "M", accuracy = 0.1), breaks = c(2,2.2,2.4,2.6,2.8,3), limits = c(2,2.6), expand = c(0,0)) +
  ggtitle("Tending the Farm") +
  labs(caption = "Graph created by @JosephPolitano using BLS data", subtitle = "Agricultural Employment is Holding Approximately Steady") +
  theme_apricitas + theme(legend.position = c(.65,.94)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2018-01-01")-(.1861*(today()-as.Date("2018-01-01"))), xmax = as.Date("2018-01-01")-(0.049*(today()-as.Date("2018-01-01"))), ymin = 2-(.3*0.6), ymax = 2) +
  coord_cartesian(clip = "off")

EMPLOYMENT_Graph <- ggplot() + #CPS with NFP adjusted concepts
  geom_line(data = PAYEMS, aes(x=date, y = value/1000, color = "Nonfarm Payrolls"), size = 1.25) + 
  geom_line(data = CPSADJ, aes(x=date, y = value/1000, color = "Household Survey Adjusted to Nonfarm Payrolls Concepts"), size = 1.25) + 
  annotate(geom = "vline", x = as.Date("2022-03-01"), xintercept = as.Date("2022-03-01"), color = "white", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "March 2022", x = as.Date("2021-12-01"), y = 135, color ="white", size = 4) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "M"),limits = c(120,160), breaks = c(120,130,140,150,160), expand = c(0,0)) +
  ylab("Payrolls/Employees, Millions") +
  ggtitle("Labor Market Mystery Hour") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Gap Persists Even When Household Survey Data is Adjusted to Payrolls Concepts") +
  theme_apricitas + theme(legend.position = c(.375,.92)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 120-(.3*40), ymax = 120) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

EMPLOYMENT_INDEX_Graph <- ggplot() + #indexed employment rate
  geom_line(data = PAYEMS, aes(x=date, y = value/1521.28, color = "Nonfarm Payrolls (Establishment Survey)"), size = 1.25) + 
  geom_line(data = ELEV, aes(x=date, y = value/1586.53, color = "Employment Level (Household Survey)"), size = 1.25) + 
  annotate(geom = "vline", x = as.Date("2022-03-01"), xintercept = as.Date("2022-03-01"), color = "white", linetype = "dashed", size = 1.25) +
  annotate(geom = "text", label = "March 2022", x = as.Date("2021-12-01"), y = 90, color ="white", size = 4) +
  xlab("Date") +
  scale_y_continuous(limits = c(82,105), breaks = c(85,90,95,100,105), expand = c(0,0)) +
  ylab("Index, Jan 2020 = 100") +
  ggtitle("Labor Market Mystery Hour") +
  labs(caption = "Graph created by @JosephPolitano using BLS data",subtitle = "The Establishment Survey Shows Positive Growth, but the Household Survey Shows a Stall") +
  theme_apricitas + theme(legend.position = c(.50,.90)) +
  scale_color_manual(name= NULL,values = c("#FFE98F","#00A99D","#00A99D")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2019-01-01")-(.1861*(today()-as.Date("2019-01-01"))), xmax = as.Date("2019-01-01")-(0.049*(today()-as.Date("2019-01-01"))), ymin = 82-(.3*23), ymax = 82) + #these repeated sections place the logo in the bottom-right of each graph. The first number in all equations is the chart's origin point, and the second number is the exact length of the x or y axis
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = MULTIPLE_JOBHOLDERS_Graph, "Multiple Jobholders.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = SELF_EMPLOYED_Graph, "Self Employed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = AGRICULTURAL_Graph, "Agricultural.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = EMPLOYMENT_Graph, "Employment.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = EMPLOYMENT_INDEX_Graph, "Employment Indexed.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = PAYEMS_ELEV_CPSADJ_32022_Graph, "Payems Elev CPSADJ.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE
ggsave(dpi = "retina",plot = TOTAL_DISCREPANCY_32022_Graph, "Total Discrepancy.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in") #CAIRO GETS RID OF THE ANTI ALIASING ISSUE


p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
pacman::p_load(wbstats,openxlsx,jsonlite,INEbaseR,seasonal,cbsodataR,rsdmx,dplyr,seasonal,wiesbaden,insee,ggspatial,rnaturalearthdata,rnaturalearth,sf,ecb,eurostat,censusapi,cli,remotes,magick,cowplot,knitr,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)

Indicator_Search <- wbsearch("GDP per Capita")

new_cache <- wb_cache()

GDP_PER_CAPITA_PPP_BULK <- wb_data("NY.GDP.PCAP.PP.KD", start_date = 2019, end_date = 2050) 

GDP_PER_CAPITA_PPP_GROWTH <- GDP_PER_CAPITA_PPP_BULK %>%
  select(country, date, `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country, values_from = `NY.GDP.PCAP.PP.KD`) %>%
  mutate(across(-date, ~ last(.x) / first(.x))) %>%
  select(-date) %>%
  unique() %>%
  pivot_longer(cols = everything())

GDP_PER_CAPITA_PPP_GROWTH_MINUS_US <- GDP_PER_CAPITA_PPP_GROWTH %>%
  mutate(value = value - GDP_PER_CAPITA_PPP_GROWTH$value[GDP_PER_CAPITA_PPP_GROWTH$name == "United States"])
  
GDP_PER_CAPITA_LEVEL <- GDP_PER_CAPITA_PPP_BULK %>%
  select(country, date, `NY.GDP.PCAP.PP.KD`) %>%
  pivot_wider(names_from = country, values_from = `NY.GDP.PCAP.PP.KD`) %>%
  slice(nrow(.)) %>%
  select(-date) %>%
  pivot_longer(cols = everything())
  
GDP_PER_CAPITA_LEVEL_FILTERED <- GDP_PER_CAPITA_LEVEL %>%
  filter(name %in% c("India", "China", "United States", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Ethiopia","Philippines")) %>%
  arrange(value) %>%
  mutate(name = factor(name, levels = c("Ethiopia","Pakistan","Nigeria","Bangladesh","India","Philippines","Indonesia","Brazil","China","United States")))

GDP_PER_CAPITA_LEVEL_FILTERED_graph <- ggplot(data = GDP_PER_CAPITA_LEVEL_FILTERED, aes(x = name, y = value/1000,fill = "Real GDP/Capita, Adjusted for Purchasing Power Parity")) +
  annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +
  xlab(NULL) +
  ylab("Real GDP Per Capita (2021 PPP)") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, suffix = "k"), limits = c(0,ceiling(max(GDP_PER_CAPITA_LEVEL_FILTERED$value)/10000)*10), expand = c(0,0)) +
  ggtitle("The Scale of Global Economic Disparities") +
  labs(caption = "Graph created by @JosephPolitano using World Bank Data") +
  scale_fill_manual(name= NULL,values = c("#FFE98F","#00A99D","#EE6055","#A7ACD9","#9A348E")) +
  theme_apricitas + theme(legend.position = c(.6,.6), axis.text.y = element_text(size = 16), plot.margin = unit(c(0.2,0.6,0.2,0.1), "cm"), plot.title = element_text(size = 25)) +#, axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_flip()

ggsave(dpi = "retina",plot = GDP_PER_CAPITA_LEVEL_FILTERED_graph, "Real GDP Per Capita Level, Filtered Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



p_unload(all)  # Remove all packages using the package manager


# Clear console
cat("\014")  # ctrl+L

rm(list = ls())

dev.off()

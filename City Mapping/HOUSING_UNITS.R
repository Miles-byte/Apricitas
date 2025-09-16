pacman::p_load(ggrepel,dots,ggridges,openxlsx,censusapi,nngeo,ggpubr,sf,tigris,maps,mapproj,usmap,fips,bea.R,janitor,cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)
p_load("acs","leaflet","tigris","dplyr","stringr","lwgeom")

api.key.install(key=Sys.getenv("CENSUS_KEY"))

theme_apricitas <- theme_ft_rc() + #setting the "apricitas" custom theme that I use for my blog
  theme(axis.line = element_line(colour = "white"),legend.position = c(.90,.90),legend.text = element_text(size = 14, color = "white"), legend.title =element_text(size = 14),plot.title = element_text(size = 28, color = "white")) #using a modified FT theme and white axis lines for my "theme_apricitas"

apricitas_logo <- image_read("https://github.com/Miles-byte/Apricitas/blob/main/Logo.png?raw=true") #downloading and rasterizing my "Apricitas" blog logo from github
apricitas_logo_rast <- rasterGrob(apricitas_logo, interpolate=TRUE)


SFO <- geo.make(state = "CA",place = "San Francisco")
DCA <- geo.make(state = "DC",place = "Washington city")
SEA <- geo.make(state = "WA",place = "Seattle")
DEN <- geo.make(state = "CO",place = "Denver")
ATX <- geo.make(state = "TX",place = "Austin")
CHA <- geo.make(state = "NC",place = "Charlotte")
MIA <- geo.make(state = "FL",place = "Miami city")

CITIES <- ATX + SFO + DCA + SEA + DEN + CHA + MIA

CITIES <- SFO + MIA


CITY_HOUSING_df_TOTAL <- NULL

for (year in c(2012:2019,2021:2023:2024)) {
  CITY_HOUSING <- acs.fetch(geography = CITIES, endyear = year, table.number = "B25024", span = 1)
  
  CITY_HOUSING_df <- data.frame(cbind(data.frame(CITY_HOUSING@geography), data.frame(CITY_HOUSING@estimate))) %>% 
    summarize(NAME,
              GEOID = paste0(CITY_HOUSING@geography$state,
                             str_pad(CITY_HOUSING@geography$place,
                                     width = 3,
                                     side = "left",
                                     pad = "0")
              ),
              total_units = B25024_001,
              Large_apartments = B25024_009
    ) %>%
    mutate(year = year)
  
  CITY_HOUSING_df_TOTAL <- bind_rows(CITY_HOUSING_df_TOTAL, CITY_HOUSING_df)
}


CITY_HOUSING_df_TOTAL <- CITY_HOUSING_df_TOTAL %>%
  mutate(year = as.Date(paste0(year,"-01-01"))) %>%
  mutate(NAME_shortened = case_when(
    NAME == "Austin city, Texas" ~ "Austin, TX",
    NAME == "San Francisco city, California" ~ "San Francisco, CA",
    NAME == "Washington city, District of Columbia" ~ "Washington, DC",
    NAME == "Seattle city, Washington" ~ "Seattle, WA",
    NAME == "Denver city, Colorado" ~ "Denver, CO",
    NAME == "Charlotte city, North Carolina" ~ "Charlotte, NC",
    NAME == "Miami city, Florida" ~ "Miami, FL",
    TRUE ~ NAME
  ))

CITY_HOUSING_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(CITY_HOUSING_df_TOTAL, year <= as.Date("2020-01-01")), aes(x=year,y=total_units/1000,color= NAME_shortened), size = 1.25) +
  geom_point(data= filter(CITY_HOUSING_df_TOTAL, year <= as.Date("2020-01-01")), aes(x=year,y=total_units/1000,color= NAME_shortened), size = 3) +
  geom_line(data= filter(CITY_HOUSING_df_TOTAL, year >= as.Date("2020-01-01")), aes(x=year,y=total_units/1000,color= NAME_shortened), size = 1.25) +
  geom_point(data= filter(CITY_HOUSING_df_TOTAL, year >= as.Date("2020-01-01")), aes(x=year,y=total_units/1000,color= NAME_shortened), size = 3) +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "No\nPandemic\nData", x = as.Date("2019-10-01"), hjust = 1, y = 125, color = "white", size = 4, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Data Revised\nPost 2020\nCensus Results", x = as.Date("2020-04-01"), y = 125, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"),limits = c(0,550), expand = c(0,0)) +
  ylab("Housing Units") +
  ggtitle("Housing Units by City") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau ACS Data.",subtitle = "San Francisco Has Seen Little Housing Growth Over the Last Decade") +
  #theme_apricitas + theme(legend.position = c(.15,.81), legend.key.height = unit(0, "cm")) +
  theme_apricitas + theme(legend.position = "right", legend.key.height = unit(0, "cm")) +
  #scale_color_manual(name= "Total Housing Units",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Austin, TX","San Francisco, CA","Charlotte, NC","Seattle, WA", "Denver, CO", "Washington, DC","Miami, FL")) +
  scale_color_manual(name= "Total Housing Units",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC","#F5B041"), breaks = c("Austin, TX","San Francisco, CA","Charlotte, NC","Seattle, WA", "Denver, CO", "Washington, DC","Miami, FL")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 0-(.3*550), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = CITY_HOUSING_GRAPH, "City Housing Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")


LARGE_APARTMENTS_GRAPH <- ggplot() + #plotting EU NET EV Exports
  annotate("hline", y = 0, yintercept = 0, color = "white", size = 0.5) +
  geom_line(data= filter(CITY_HOUSING_df_TOTAL, year <= as.Date("2020-01-01")), aes(x=year,y=Large_apartments/1000,color= NAME_shortened), size = 1.25) +
  geom_point(data= filter(CITY_HOUSING_df_TOTAL, year <= as.Date("2020-01-01")), aes(x=year,y=Large_apartments/1000,color= NAME_shortened), size = 3) +
  geom_line(data= filter(CITY_HOUSING_df_TOTAL, year >= as.Date("2020-01-01")), aes(x=year,y=Large_apartments/1000,color= NAME_shortened), size = 1.25) +
  geom_point(data= filter(CITY_HOUSING_df_TOTAL, year >= as.Date("2020-01-01")), aes(x=year,y=Large_apartments/1000,color= NAME_shortened), size = 3) +
  annotate("vline", x = as.Date("2020-01-01"), xintercept = as.Date("2020-01-01"), color = "white", size = 1, linetype = "dashed", alpha = 0.75) +
  annotate("text", label = "No\nPandemic\nData", x = as.Date("2019-10-01"), hjust = 1, y = 135, color = "white", size = 4, alpha = 0.75, lineheight = 0.8) +
  annotate("text", label = "Data Revised\nPost 2020\nCensus Results", x = as.Date("2020-04-01"), y = 135, color = "white", size = 4, hjust = 0, lineheight = 0.8, alpha = 0.75) +
  xlab("Date") +
  scale_y_continuous(labels = scales::number_format(suffix = "k"),limits = c(0,150), expand = c(0,0)) +
  ylab("Housing Units") +
  ggtitle("Units in Large Apartments (50+ Units)") +
  labs(caption = "Graph created by @JosephPolitano using Census Bureau ACS Data.",subtitle = "San Francisco Has Seen Little Housing Growth Over the Last Decade") +
  theme_apricitas + theme(legend.position = c(.15,.81), legend.key.height = unit(0, "cm")) +
  #scale_color_manual(name= "Total Housing Units",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC"), breaks = c("Austin, TX","San Francisco, CA","Charlotte, NC","Seattle, WA", "Denver, CO", "Washington, DC")) +
  scale_color_manual(name= "Total Housing Units",values = c("#FFE98F","#00A99D","#EE6055","#9A348E","#A7ACD9","#3083DC","#F5B041"), breaks = c("Austin, TX","San Francisco, CA","Charlotte, NC","Seattle, WA", "Denver, CO", "Washington, DC","Miami, FL")) +
  annotation_custom(apricitas_logo_rast, xmin = as.Date("2012-01-01")-(.1861*(today()-as.Date("2012-01-01"))), xmax = as.Date("2012-01-01")-(0.049*(today()-as.Date("2012-01-01"))), ymin = 0-(.3*150), ymax = 0) +
  coord_cartesian(clip = "off")

ggsave(dpi = "retina",plot = LARGE_APARTMENTS_GRAPH, "Large Apartments Graph.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

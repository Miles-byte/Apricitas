for (metro in METRO_SELECT) {
  # Create the plot
  METRO_SELECT_Graph <- ggplot() +
    annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
    geom_point(data = filter(ZHVI_NEIGHBORHOOD_REG_2016, Metro == metro), 
               aes(x = value_2016, y = annualized_percent, color = "2016-2020")) +
    stat_smooth(data = filter(ZHVI_NEIGHBORHOOD_REG_2016, Metro == metro),
                method = "lm", aes(x = value_2016, y = annualized_percent, color = "2016-2020"), size = 1.25) +
    geom_point(data = filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro), 
               aes(x = value_2020, y = annualized_percent_lagged, color = "2020-May 2023")) +
    stat_smooth(data = filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro),
                method = "lm", aes(x = value_2020, y = annualized_percent_lagged, color = paste0("2020-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"))), size = 1.25) +
    geom_point(data=filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro), aes(x=value_lag_12,y=percent12, color= paste0(format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"),"-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date), "%b %Y"))))+
    stat_smooth(data=filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro),method = "lm", aes(x=value_lag_12,y=percent12, color= paste0(format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"),"-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date), "%b %Y"))), size = 1.25) +
    ggtitle(metro) +
    scale_x_continuous(labels = scales::number_format(accuracy = 1),limits = c(9.5,15.5), expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.05,.25), expand = c(0,0), breaks = c(-.05,0,.05,.10,.15,.20,.25)) +
    theme_apricitas +
    ggtitle(paste0("Home Value Change in ",gsub("-[^,]*,", ",", metro), "\nMetro Area By Neighborhood Price Level")) +
    ylab("Change in Home Value, Annualized %") +
    xlab("Natural Log of Starting Home Value") +
    theme(axis.title.x = element_text(size = 20, hjust = 0.5),
          axis.title.y = element_text(size = 15, vjust = 0.5),
          plot.title = element_text(size = 23.5)) +
    guides(size = "none") +
    scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055")) +
    annotation_custom(apricitas_logo_rast, xmin = 9.3-(.1861*7.3), xmax = 9.3-(0.049*7.3), ymin = -0.045-(.3*.26), ymax = -0.05) +
    coord_cartesian(clip = "off") 
  
  
  # Save the plot
  file_name <- paste0("Metro_Select_Yoy_Graph", gsub("[[:punct:]]", "", metro), ".png")
  ggsave(filename = file_name, plot = METRO_SELECT_Graph, dpi = "retina", 
         type = "cairo-png", width = 9.02, height = 5.76, units = "in")
}

METRO_ARRANGE_PLOTS <- list()

for (metro in METRO_SELECT) {
  # Create the plot
  plot_name <- paste0("METRO_ARRANGE_Yoy_Graph_", gsub("[[:punct:]]|\\s", "_", gsub("-[^,]*,", ",", metro)))
  METRO_ARRANGE_PLOTS[[plot_name]] <- ggplot() +
    annotate("hline", y = 0, yintercept = 0, color = "white", size = .5) +
    geom_point(data = filter(ZHVI_NEIGHBORHOOD_REG_2016, Metro == metro), 
               aes(x = value_2016, y = annualized_percent, color = "2016-2020"), size = 0.75) +
    stat_smooth(data = filter(ZHVI_NEIGHBORHOOD_REG_2016, Metro == metro),
                method = "lm", aes(x = value_2016, y = annualized_percent, color = "2016-2020"), size = 1.25) +
    geom_point(data = filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro), 
               aes(x = value_2020, y = annualized_percent_lagged, color = paste0("2020-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"))), size = 0.75) +
    stat_smooth(data = filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro),
                method = "lm", aes(x = value_2020, y = annualized_percent_lagged, color = paste0("2020-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"))), size = 1.25) +
    geom_point(data=filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro), aes(x=value_lag_12,y=percent12, color= paste0(format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"),"-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date), "%b %Y"))), size = 0.75)+
    stat_smooth(data=filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro),method = "lm", aes(x=value_lag_12,y=percent12, color= paste0(format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date-370), "%b %Y"),"-", format(min(filter(ZHVI_NEIGHBORHOOD_REG, Metro == metro)$date), "%b %Y"))), size = 1.25) +
    ggtitle(metro) +
    scale_x_continuous(labels = scales::number_format(accuracy = 1),limits = c(9.5,15.5), expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(-.05,.25), expand = c(0,0), breaks = c(-.05,0,.05,.10,.15,.20,.25)) +
    theme_apricitas +
    ggtitle(paste0(gsub("-[^,]*,", ",", metro)," Metro Area")) +
    ylab(NULL) +
    xlab(NULL) +
    theme(axis.title.x = element_text(size = 20, hjust = 0.5),
          axis.title.y = element_text(size = 15, vjust = 0.5),
          plot.title = element_text(size = 23.5)) +
    guides(size = "none") +
    scale_color_manual(name= NULL,values = c("#00A99D","#FFE98F","#EE6055")) +
    theme(legend.position = "right", panel.grid.major=element_blank(),plot.margin= grid::unit(c(0, 0.1, 0.05, 0), "in")) + 
    theme(plot.title = element_text(size = 14,hjust = 0.5))
}

METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Washington__DC_VA_MD_WV +
  theme_minimal()

NORTHEAST_NEIGHBORHOOD_Yoy_Graph <- ggarrange(METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_New_York__NY_NJ_PA,
                                          METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Washington__DC_VA_MD_WV,
                                          METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Philadelphia__PA_NJ_DE_MD,
                                          METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Boston__MA_NH,ncol = 2, nrow = 2, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

NORTHEAST_NEIGHBORHOOD_Yoy_Graph <- annotate_figure(NORTHEAST_NEIGHBORHOOD_Yoy_Graph, 
                                                top = text_grob("Home Value Change by Neighborhood Price Level", face = "bold", size = 26.5, color = "white"),
                                                bottom = text_grob("Natural Log of Starting Home Value", size = 20, color = "#929299"), 
                                                left = text_grob("Change in Home Value, Annualized %", size = 15, color = "#929299", rot = 90)) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = NORTHEAST_NEIGHBORHOOD_Yoy_Graph, "Northeast Neighborhood Yoy Regression.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")



CALIFORNIA_NEIGHBORHOOD_Yoy_Graph <- ggarrange(METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Los_Angeles__CA,
                                           METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Riverside__CA,
                                           METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_San_Francisco__CA,
                                           METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_San_Diego__CA,ncol = 2, nrow = 2, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

CALIFORNIA_NEIGHBORHOOD_Yoy_Graph <- annotate_figure(CALIFORNIA_NEIGHBORHOOD_Yoy_Graph, 
                                                 top = text_grob("Home Value Change by Neighborhood Price Level", face = "bold", size = 26.5, color = "white"),
                                                 bottom = text_grob("Natural Log of Starting Home Value", size = 20, color = "#929299"), 
                                                 left = text_grob("Change in Home Value, Annualized %", size = 15, color = "#929299", rot = 90)) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = CALIFORNIA_NEIGHBORHOOD_Yoy_Graph, "California Neighborhood Yoy Regression.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

MIDWEST_NEIGHBORHOOD_Yoy_Graph <- ggarrange(METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Chicago__IL_IN_WI,
                                        METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Detroit__MI,
                                        METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Minneapolis__MN_WI,
                                        METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_St__Louis__MO_IL,ncol = 2, nrow = 2, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

MIDWEST_NEIGHBORHOOD_Yoy_Graph <- annotate_figure(MIDWEST_NEIGHBORHOOD_Yoy_Graph, 
                                              top = text_grob("Home Value Change by Neighborhood Price Level", face = "bold", size = 26.5, color = "white"),
                                              bottom = text_grob("Natural Log of Starting Home Value", size = 20, color = "#929299"), 
                                              left = text_grob("Change in Home Value, Annualized %", size = 15, color = "#929299", rot = 90)) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = MIDWEST_NEIGHBORHOOD_Yoy_Graph, "Midwest Neighborhood Yoy Regression.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

TEXAS_NEIGHBORHOOD_Yoy_Graph <- ggarrange(METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Dallas__TX,
                                      METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Houston__TX,
                                      METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_San_Antonio__TX,
                                      METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Austin__TX,ncol = 2, nrow = 2, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

TEXAS_NEIGHBORHOOD_Yoy_Graph <- annotate_figure(TEXAS_NEIGHBORHOOD_Yoy_Graph, 
                                            top = text_grob("Home Value Change by Neighborhood Price Level", face = "bold", size = 26.5, color = "white"),
                                            bottom = text_grob("Natural Log of Starting Home Value", size = 20, color = "#929299"), 
                                            left = text_grob("Change in Home Value, Annualized %", size = 15, color = "#929299", rot = 90)) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = TEXAS_NEIGHBORHOOD_Yoy_Graph, "Texas Neighborhood Yoy Regression.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

SOUTH_NEIGHBORHOOD_Yoy_Graph <- ggarrange(METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Atlanta__GA,
                                      METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Miami__FL,
                                      METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Tampa__FL,
                                      METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Charlotte__NC_SC,ncol = 2, nrow = 2, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

SOUTH_NEIGHBORHOOD_Yoy_Graph <- annotate_figure(SOUTH_NEIGHBORHOOD_Yoy_Graph, 
                                            top = text_grob("Home Value Change by Neighborhood Price Level", face = "bold", size = 26.5, color = "white"),
                                            bottom = text_grob("Natural Log of Starting Home Value", size = 20, color = "#929299"), 
                                            left = text_grob("Change in Home Value, Annualized %", size = 15, color = "#929299", rot = 90)) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = SOUTH_NEIGHBORHOOD_Yoy_Graph, "South Neighborhood Yoy Regression.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

WEST_NEIGHBORHOOD_Yoy_Graph <- ggarrange(METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Phoenix__AZ,
                                     METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Seattle__WA,
                                     METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Denver__CO,
                                     METRO_ARRANGE_PLOTS$METRO_ARRANGE_Yoy_Graph_Portland__OR_WA,ncol = 2, nrow = 2, common.legend = TRUE, legend = "top") + bgcolor("#252A32") + border("#252A32")

WEST_NEIGHBORHOOD_Yoy_Graph <- annotate_figure(WEST_NEIGHBORHOOD_Yoy_Graph, 
                                           top = text_grob("Home Value Change by Neighborhood Price Level", face = "bold", size = 26.5, color = "white"),
                                           bottom = text_grob("Natural Log of Starting Home Value", size = 20, color = "#929299"), 
                                           left = text_grob("Change in Home Value, Annualized %", size = 15, color = "#929299", rot = 90)) + bgcolor("#252A32")

ggsave(dpi = "retina",plot = WEST_NEIGHBORHOOD_Yoy_Graph, "West Neighborhood Yoy Regression.png", type = "cairo-png", width = 9.02, height = 5.76, units = "in")

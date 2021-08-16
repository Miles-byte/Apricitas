pacman::p_load(pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly)

ECI_PCE <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/ECI_PCE.csv")
GDP_Comp <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/GDP_Comp.csv")
PCE_Inflation_Rates <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Is%20Inflation%20Transitory/PCE_Inflation_Rates.csv")

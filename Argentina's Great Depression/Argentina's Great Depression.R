pacman::p_load(cli,remotes,magick,cowplot,knitr,ghostscript,png,httr,grid,usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr)

DFA <- read.csv("https://raw.githubusercontent.com/Miles-byte/Apricitas/main/Understanding%20Americans'%20Excess%20Savings/dfa-income-levels-detail.csv")





cat("\014")  # ctrl+L

rm(list = ls())

dev.off()
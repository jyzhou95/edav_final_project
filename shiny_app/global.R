library(shiny)
library(data.table)
library(ggplot2)
library(lubridate)
library(plotly)
library(choroplethr)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
setwd("..")
parent_dir <- getwd()
dt.data <- fread(paste0(parent_dir, "/health_care_data/master.csv"))




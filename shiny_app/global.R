library(shiny)
library(data.table)
library(ggplot2)
library(lubridate)
library(plotly)
library(ggthemes)
library(viridis)
library(extracat)
library(glue)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
parent_dir <- getwd()
dt.data <- fread(paste0(parent_dir, "/health_care_data/master.csv"))
dt.data[,dt := as.Date(dt, format = "%m/%d/%Y")]

# Merge breach type name to data
dt.breach_type_name <- data.table(breach_type = c("DISC", "HACK", "CARD", "INSD", "PHYS", "PORT", "STAT", "UNKN"),
                                  breach_type_name = c("Unintended Disclosure",
                                                       "Hacking",
                                                       "Payment Card Fraud",
                                                       "Insider Fraud",
                                                       "Physical Loss",
                                                       "Portable Device Loss",
                                                       "Stationary Device Loss",
                                                       "Unknown"))
dt.data <- merge(dt.data, dt.breach_type_name, by = c("breach_type"))

# Scrape data from Wikipedia
dt.state_population <- fread(paste0(parent_dir, "/health_care_data/state_population.csv"))
dt.state_population[,V1 := NULL]
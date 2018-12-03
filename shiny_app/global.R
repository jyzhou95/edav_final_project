library(shiny)
library(data.table)
library(ggplot2)
library(lubridate)
library(plotly)
library(choroplethr)
library(rvest)
library(ggthemes)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
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
url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
population <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
population <- population[[1]]
dt.state_population <- data.table(population)
dt.state_population <- dt.state_population[,c(3,4)]
colnames(dt.state_population) <- c("state", "population")
dt.state_population <- dt.state_population[state %in% state.name]
dt.state_population[,population := gsub(",", "", population)]
dt.state_population[,population := as.numeric(population)]


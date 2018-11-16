library(data.table)
library(ggplot2)
library(lubridate)

dt.data <- fread("~/GitHub/edav_final_project/health_care_data/master.csv")

dt.time_series <- dt.data[,list(dt, breach_id)]
dt.time_series <- dt.time_series[order(dt)]

dt.year <- dt.time_series[,.N, by = year(dt)]
dt.year[,year := as.Date(paste0(year, "-01-01"))]

ggplot(dt.year, aes(x = year, y = N)) + geom_bar(stat = "identity", fill = "lightblue") + theme_bw(base_size = 15) + xlab("Date") + ylab("Number of breach instances") + ggtitle("Number of data breaches from 2005 until 2018") + scale_x_date(date_breaks = "2 years")


dt.month <- dt.time_series
dt.month$dt <- floor_date(as.Date(dt.month$dt), "month")
dt.month <- dt.month[,.N, by = dt]
dt.month[,year := year(dt)]
dt.month[,month := month(dt)]


ggplot(dt.month, aes(x = year, y = N)) + geom_point() + theme_bw(base_size = 15) + xlab("Date") + ylab("Number of breach instances") + ggtitle("Number of data breaches from 2005 until 2018") + facet_wrap(~month)

ggplot(dt.month, aes(x = month, y = N)) + geom_point() + theme_bw(base_size = 15) + xlab("Date") + ylab("Number of breach instances") + ggtitle("Number of data breaches from 2005 until 2018") + facet_wrap(~year)



# Filter rows without state
library(choroplethr)
dt.states <- dt.data[nchar(state) > 2][,list(dt, state, breach_id)]
dt.map <- dt.states[,.N, by = state]
dt.map <- dt.map[state %in% state.name]
dt.map$state <- tolower(dt.map$state)
# Add DC
dt.map <- rbind(dt.map,
                data.table(state = "district of columbia",
                           value = 0))
colnames(dt.map) <- c("region", "value")
state_choropleth(data.frame(dt.map),
                 "Number of data breach instances from 2005 to 2018")


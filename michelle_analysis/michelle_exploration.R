library(data.table)
library(glue)
library(vcd)
library(tidyverse)

########################
#Michelle Exploration###
########################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
parent_dir <- getwd()
dt.master <-fread(glue("{parent_dir}/health_care_data/master.csv"))

cat_records_breached <- dt.master %>% filter(records_breached <1000000& !is.na(records_breached)) %>% select(records_breached, cat_name)
ggplot(data = cat_records_breached,aes(x = reorder(cat_name,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across Category/Industry") + ylab("# Records Breached") + xlab("")

state_records_breached <-dt.master %>%filter(!is.na(records_breached) & records_breached <100000) %>% select(state, records_breached)
ggplot(data = state_records_breached,aes(x = reorder(state,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across States") + ylab("# Records Breached") + xlab("")

region_records_breached <-dt.master %>%filter(!is.na(records_breached) & records_breached <50000 & region!="" & region!="Puerto Rico") %>% select(region, records_breached, state)
ggplot(data = region_records_breached,aes(x = reorder(region,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across Region") + ylab("# Records Breached") + xlab("")


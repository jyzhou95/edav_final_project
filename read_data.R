library(data.table)
library(glue)
library(vcd)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

dt.categories <- fread(glue("{parent_dir}/health_care_data/categories.csv"))
dt.catorgs <- fread(glue("{parent_dir}/health_care_data/catsorgs.csv"))
dt.orgsindex <- fread(glue("{parent_dir}/health_care_data/orgsindex.csv"))
dt.prcbreaches <- fread(glue("{parent_dir}/health_care_data/prcbreaches2005-18.csv"))

dt.categories_final <- dt.categories[,list(cat_id = CatID,
                                     cat_name = CatName)]

dt.catorgs_final <- dt.catorgs[,list(cat_id = CatID,
                               org_id = OrgID,
                               type = Type)]

dt.orgsindex_final <- dt.orgsindex[,list(org_id = OrgID,
                                   company_name = Name,
                                   source_type = SourceType)]

dt.prcbreaches_final <- dt.prcbreaches[,list(records_breached = Records.Breached,
                                       records_breached_detail = Records.Breached...Detail,
                                       name = Name,
                                       cat_id = CatID,
                                       total_records = Total.Records,
                                       region = Region,
                                       category = Category_dm,
                                       entity_type = Entity_prc,
                                       state = State,
                                       org_id = OrgID,
                                       location = Location,
                                       dt = Date.Made.Public,
                                       breach_source = Source.of.Breach.Notification,
                                       breach_type = Type)]

dt.orgsindex_final <- unique(dt.orgsindex_final, by = c("org_id"))

dt.master <- merge(merge(merge(dt.prcbreaches_final, dt.catorgs_final, by = c("cat_id", "org_id"), all.x = TRUE),
                   dt.categories_final, by = c("cat_id"), all.x = TRUE),
                   dt.orgsindex_final, by = c("org_id"), all.x = TRUE)

# Fix region
dt.region <- unique(dt.master[!is.na(region)][,list(region, state)])

# Merge it backc on
dt.master[,region := NULL]
dt.master <- merge(dt.master, dt.region, by = c("state"), all.x = TRUE)

# Add unique identifier to each breach
dt.master$breach_id <- 1:nrow(dt.master)

dt.date_format1 <- dt.master[grepl("\\-", dt)]
dt.date_format1$dt <- as.Date(dt.date_format1$dt, format = "%d-%b-%y")

dt.date_format2 <- dt.master[grepl("\\/", dt)]
dt.date_format2$dt <- as.Date(dt.date_format2$dt, format = "%m/%d/%Y")

dt.final <- rbind(dt.date_format1,
                  dt.date_format2)

write.csv(dt.final, glue("{parent_dir}/health_care_data/master.csv"))
# Entity type
# BSO - business other
# BSF - business - financial and insurance services
# BSR - business - retail and merchant
# EDU - education institutions
# GOV - government and military
# MED - healthcare - medical providersr
# NGO - nonprofit organizations





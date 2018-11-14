library(data.table)
library(glue)

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

dt.master

# Entity type
# BSO - business other
# BSF - business - financial and insurance services
# BSR - business - retail and merchant
# EDU - education institutions
# GOV - government and military
# MED - healthcare - medical providersr
# NGO - nonprofit organizations



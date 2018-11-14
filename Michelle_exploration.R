library(data.table)
library(glue)
library(tidyverse)
library(vcd)

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


########################
#Michelle Exploration###
########################

str(dt.master)
table(dt.master$records_breached)

cat_records_breached <- dt.master %>% filter(records_breached <1000000& !is.na(records_breached)) %>% select(records_breached, cat_name)
ggplot(data = cat_records_breached,aes(x = reorder(cat_name,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across Category/Industry") + ylab("# Records Breached") + xlab("")

state_records_breached <-dt.master %>%filter(!is.na(records_breached) & records_breached <100000) %>% select(state, records_breached)
ggplot(data = state_records_breached,aes(x = reorder(state,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across States") + ylab("# Records Breached") + xlab("")

region_records_breached <-dt.master %>%filter(!is.na(records_breached) & records_breached <50000 & region!="" & region!="Puerto Rico") %>% select(region, records_breached, state)
ggplot(data = region_records_breached,aes(x = reorder(region,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across Region") + ylab("# Records Breached") + xlab("")


###################
#Yimin Exploration#
###################

dt.master$total_records<-as.numeric(dt.master$total_records)
str(dt.master)

#Region by Breach_Type Mosaic Plot
counts1 <- dt.master %>% filter(region!= "Argentina" & region!="Puerto Rico") %>% group_by(region, breach_type) %>% summarize(Freq = n()) %>% arrange(desc(Freq))
counts1$breach_type <-factor(counts1$breach_type, levels = c('PORT', 'PHYS','INSD','DISC','HACK','UNKN','STAT'))
mosaic(breach_type~region, counts1,
       rot_labels = c(10,0,0, 30),
       direction = c('v', 'h'))


#Category, Breach_Type Occurence
table(dt.master$category)

counts2 <- dt.master %>% filter(region!= "Argentina" & region!="Puerto Rico") %>% group_by(category, breach_type) %>% summarize(Freq = n()) %>% arrange(desc(Freq))
counts2
ggplot(data = counts2[1:16,], aes(x=reorder(category,-Freq), y = Freq))+ 
  geom_bar(stat = "identity") + xlab("Category") + 
  ylab("Occurences of Breaches") + 
  ggtitle("How Often Breaches Occur to Specific Categories") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Breach_Type breakdown within Physician, Hospital category
counts3 <- counts2 %>% filter(category =="Physician, Hospital")
physician_cat_breach<-ggplot(data = counts3,aes(x = reorder(breach_type,Freq), y =Freq)) + 
  geom_col() + coord_flip() + 
  ggtitle("Frequency of each Breach_Type for Physician, Hospital Category") + 
  ylab("Frequency of Breach Type (Not # records breached)") + xlab("")
physician_cat_breach


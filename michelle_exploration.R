library(data.table)
library(glue)
library(vcd)
library(tidyverse)


########################
#Michelle Exploration###
########################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dt.master <-fread(glue("{parent_dir}/health_care_data/master.csv"))

cat_records_breached <- dt.master %>% filter(records_breached <1000000& !is.na(records_breached)) %>% select(records_breached, cat_name)
ggplot(data = cat_records_breached,aes(x = reorder(cat_name,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across Category/Industry") + ylab("# Records Breached") + xlab("")

state_records_breached <-dt.master %>%filter(!is.na(records_breached) & records_breached <100000) %>% select(state, records_breached)
ggplot(data = state_records_breached,aes(x = reorder(state,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across States") + ylab("# Records Breached") + xlab("")

region_records_breached <-dt.master %>%filter(!is.na(records_breached) & records_breached <50000 & region!="" & region!="Puerto Rico") %>% select(region, records_breached, state)
ggplot(data = region_records_breached,aes(x = reorder(region,records_breached,FUN =median), y =records_breached)) + geom_boxplot() + coord_flip() + ggtitle("Distribution of Records Breached Across Region") + ylab("# Records Breached") + xlab("")

###################
#Missing Data #####
###################
install.packages("mi")
library(mi)
x<-missing_data.frame(dt.master)
image(x)

library(extracat)
visna(dt.master, sort = "r")
###################
#Yimin Exploration#
###################

dt.master$total_records<-as.numeric(dt.master$total_records)
str(dt.master)

#Region by Breach_Type Mosaic Plot
counts1 <- dt.master %>% filter(region!= "Argentina" & region!="Puerto Rico") %>% group_by(region, breach_type) %>% summarize(Freq = n()) %>% arrange(desc(Freq))
counts1$breach_type <-factor(counts1$breach_type, levels = c('UNKN','STAT','INSD','PORT', 'PHYS','DISC','HACK'))
mosaic(breach_type~region, counts1,
       rot_labels = c(10,0,0, 30),
       direction = c('v', 'h'))


#Category, Breach_Type Occurence
table(dt.master$category)

counts2 <- dt.master %>% filter(region!= "Argentina" & region!="Puerto Rico") %>% group_by(category, breach_type) %>% summarize(Freq = n()) %>% arrange(desc(Freq))
ggplot(data = counts2[1:10,], aes(x=reorder(category,-Freq), y = Freq))+ 
  geom_bar(stat = "identity") + xlab("Category") + 
  ylab("Occurences of Breaches") + 
  ggtitle("How Often Breaches Occur to Specific Categories") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_bw(base_size = 15)

counts2


#Breach_Type breakdown within Physician, Hospital category
counts3 <- counts2 %>% filter(category =="Physician, Hospital")
physician_cat_breach<-ggplot(data = counts3,aes(x = reorder(breach_type,Freq), y =Freq)) + 
  geom_col() + coord_flip() + 
  ggtitle("Frequency of each Breach_Type for Physician, Hospital Category") + 
  ylab("Frequency of Breach Type (Not # records breached)") + xlab("")
physician_cat_breach

#Discussion of visualizations: From the first chart we can see that HACK, DISC, PHYS, PORT are the most common types of data breaches across each of the five regions. In the next chart, I grouped by category and breach_type to see how frequent certain combinations occured. We found that discharge data, payer(insurer), physician/hospital, public health, employer, and researcher were the top 5 categories that were impacted by data breaches. However, this does not necessarily tell us anything about the total number of records breached, but just that these categories are typically targeted. Lastly, I went ahead and further explored the Physician, Hospital category to understand the breakdown of the breach_types: PORT, PHYS, INSD, HACK, DISC were the top give breach_types of the total number of breach occurences to the Physican, hospital category.






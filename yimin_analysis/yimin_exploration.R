library(data.table)
library(glue)
library(vcd)
library(tidyverse)

###################
#Yimin Exploration#
###################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
parent_dir <- getwd()
dt.master <-fread(glue("{parent_dir}/health_care_data/master.csv"))

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

### PROJECT:  SCC
### PURPOSE:  Data Checks MSNA 2023
### INPUT:    xlsx file
### OUTPUT:   csv file
### AUTHOR:   Aaron Kipkemoi
### LAST UPDATED: 10th May, 2023
## Data Cleaning Script
rm(list = ls())
today <- Sys.Date()

if (!require("pacman")) install.packages("pacman")
p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       sjmisc)
# load packages
library(tidyverse)
library(lubridate)
library(openxlsx)
library(rpivotTable)
library(cleaninginspectoR)
library(HighFrequencyChecks)
library(rio)
library(koboquest)
library(tidyverse)
library(koboAPI)
library(openxlsx)
library(readxl)
library(kableExtra)
library(plyr)
library(cleaningtools)

# initialize common variables

##################
consent <- "consent"

uuid <- "uuid"
mindur <- 30
maxdur <- 60

#############load datasets
df <- readxl::read_excel("input/SOM2204_CCCM_DSA.xlsx")
district_file <- read.csv("input/idp_list.csv")

df <- df %>% 
  left_join(district_file, by = c("idp_code"))

dashboard <- df %>% select(start,end,audit,today,deviceid,	enum_name,localisation_region,	consent,	localisation_district,
                           district,	localisation_region_label,	localisation_district_label,	idp_code,	idp_code_verification,
                           idp_code_districti_verification,	ki_role,	ki_role_other,	observation_faecalmatter,	observation_shelters_flood,
                           observation_publiclighting,	observation_sufficient_space,	observation_main_secondary_accessroad,
                           observation_gps,	observation_gps,	`_observation_gps_latitude`,`_observation_gps_longitude`,
                           `_observation_gps_altitude`,`_observation_gps_precision`,
                           `_uuid`,	district_name,	IDP_Site
)

################################Data for dashboard
write.xlsx(dashboard,paste("C:\\Users\\aaron.langat\\Documents\\R\\01_DSA\\DSA_Tracker_Dashboard\\input/2023_REACH_SOM_DSA_TESTING.xlsx"))
raw_data <- df %>% select(-c(ki_contact,ki_name,referral_person,referral_name,referral_phone,observation_gps,`_observation_gps_latitude`,`_observation_gps_longitude`,
                             `_observation_gps_altitude`,`_observation_gps_precision`))

write.xlsx(raw_data,paste("C:\\Users\\aaron.langat\\ACTED\\IMPACT SOM - Unit 1 - Intersectoral\\SOM 23 DSA\\04_Data\\03_Data_Cleaning\\00_Tool_Raw_Data/SOM2204_CCCM_DSA.xlsx"))


#Load functions
source("functions/cleaning_functions.R")
################################################################
#Convert  all columns to respective data type format
koboToolPath = "input/tool/REACH_2023_SOM_DSA_Survey_Tool.xlsx"

questions = import(koboToolPath,sheet="survey") %>% filter(!is.na(name))

choices = import(koboToolPath,sheet="choices")


tool.survey<- questions%>%filter(name%in%colnames(df))
#convert columns to respective data types


# # convert integers columns to numeric
cols.integer <- filter(tool.survey, type=="integer")$name
df <- mutate_at(df, cols.integer, as.integer)
# 
cols.gps <- filter(tool.survey, type=="gps")$name
df <- mutate_at(df, cols.gps, as.integer)

#######
# cols.integer <- filter(tool.survey, type=="calculate")$name
# df <- mutate_at(df, cols.integer, as.integer)
# 
# #convert select_one to character
cols.characters <- filter(tool.survey,str_detect(type, "select_one"))$name
df <- mutate_at(df, cols.characters, as.character)
# 

# #convert select_multiple to character
cols.characters <- filter(tool.survey,str_detect(type, "select_multiple"))$name
df <- mutate_at(df, cols.characters, as.character)


## Check 1: survey time taken
df <- time_check(df, time_min = mindur, time_max = maxdur)

gis_data <- df %>% filter(interview_duration>=30 & consent=="yes") %>% select( uuid,start,end,audit,today,enum_name,localisation_region_label,
                                                                              ki_contact,district_name,idp_code,IDP_Site,contains("gps"),more_info)

gis_master <- readxl::read_excel("gis/gis_master.xlsx")


#Compare today cleaning log from the previous one

gis_data  <- filter(gis_data, uuid %!in% gis_master$uuid)
gis_master <- rbind(gis_data,gis_master) 
gis_master <- gis_master [!duplicated(gis_master$uuid),] 

write.xlsx(gis_master,paste("C:\\Users\\aaron.langat\\Documents\\R\\01_DSA\\03_Cleaning\\gis/gis_master.xlsx"))
gis_data <- gis_data [!duplicated(gis_data$uuid),] 

write.xlsx(gis_data,paste("C:\\Users\\aaron.langat\\ACTED\\IMPACT SOM - Unit 1 - Intersectoral\\SOM 23 DSA\\04_Data\\03_Data_Cleaning\\08_GIS/for_spatial_checks",today,".xlsx"))

############Refereall contacts
##################################adding resposible FOs and splitting##########################
referall <-  df[!(is.na(df$referral_phone) | df$referral_phone==""),] %>% select(localisation_region_label,district_name,idp_code, IDP_Site, referral_name,referral_phone,referral_person,uuid)

referall_master <- readxl::read_excel("gis/referall.xlsx")

#Compare today referall contacts log from the previous one
referall  <- filter(referall, uuid %!in% referall_master$uuid)
referall_master <- rbind(referall,referall_master)
referall_master <- referall_master [!duplicated(referall_master$uuid),] 

write.xlsx(referall_master,paste("C:\\Users\\aaron.langat\\Documents\\R\\01_DSA\\03_Cleaning\\gis/referall.xlsx"))


referall$district_name<-tolower(gsub(" ","_",referall$district_name))

##################################adding resposible FOs and splitting##################
f_team<- read_excel("input/FOs/Field team work distribution.xlsx")
f_team$Locations <-tolower(gsub(" ","_",f_team$Locations))
f_team$`Responsible FO`<-gsub("\\/","_",f_team$`Responsible FO`)
referall$FO <-f_team$`Responsible FO`[match(referall$district_name,f_team$Locations)]
referall$regionFO <-paste0("referall_contacts_",referall$localisation_region_label,"_")


spt1<-split(referall,referall$FO)


setDT(referall)

outputdir<-"C:\\Users\\aaron.langat\\ACTED\\IMPACT SOM - Unit 1 - Intersectoral\\SOM 23 DSA\\04_Data\\03_Data_Cleaning\\"
#feferral Abdikani kunow and Abukar
spt1$`Abdikani Kunow_Hassan Abukar`->abdikani
spt11<-split(abdikani,abdikani$regionFO)
purrr::imap(spt11, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\01_Abdikani Kunow_Hassan Abukar\\referall Contacts/",.y,today, '.xlsx')))
#referall for barre
spt1$`Abdirahima Barre`->barre
spt2<-split(barre,barre$regionFO)
purrr::imap(spt2, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\02_Abdirahima Barre\\referall Contacts/",.y,today, '.xlsx')))

#referell contacts hajir abdi
spt1$`Hajir Abdi`->hajir
spt4<-split(hajir,hajir$regionFO)
purrr::imap(spt4, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\03_Hajir Abdi\\referall Contacts/",.y,today, '.xlsx')))

#referell contactsKala
spt1$`Mohamd Kala`->kala
spt5<-split(kala,kala$regionFO)
purrr::imap(spt5, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\04_Mohamed Kala\\referall Contacts/",.y,today, '.xlsx')))

#referell contactsHassan
spt1$`Mohamed Hassan`->hassan
spt6<-split(hassan,hassan$regionFO)
purrr::imap(spt6, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\05_Mohamed Hassan\\referall Contacts/",.y,today, '.xlsx')))
#referell contactsOmAR
spt1$`Omar Abdikarin`->omar
spt7<-split(omar,omar$regionFO)
purrr::imap(spt7, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\06_Omar Abdikarin\\referall Contacts/",.y,today, '.xlsx')))
#referell contacts
spt1$`Suleiman Mohamed`->suleiman
spt8<-split(suleiman,suleiman$regionFO)
purrr::imap(spt8, ~openxlsx::write.xlsx(.x, paste0(outputdir,"\\07_Suleiman Mohamed\\referall Contacts/",.y,today, '.xlsx')))


# checks


# declined consent
check_consent <- df %>% 
  filter(consent == "no") %>% 
  issue_log(question = "consent", issue = "consent declined", action = "d")

# surveys filled with less time[to be deleted!]
less_time_surveys <- df %>% filter(consent == "yes") %>% 
  filter(interview_duration < 30) %>% 
  issue_log(question = "interview_duration", issue = "surveys filled with less time", action = "d")


# surveys filled with more time [flag them to improve for the next days]
long_time_surveys <- df %>% 
  filter(interview_duration > 90) %>% 
  issue_log(question = "interview_duration", issue = "survey filled with more time", action = "f")




#filter out those surveys under 40 minutes and those that declined consent to continue with the other checks
df <- df %>% 
  filter(interview_duration >=30 & consent=="yes")

## Check 2 : check duplicated respondent 
duplicated_res <- df %>% 
  filter(duplicated("ki_contact")) %>% 
  issue_log(question = "ki_contact", issue = "The same respondent contacted twice or more", action = "f")

##duplicated survey
duplicated_surveys <- df %>% 
  filter(duplicated("uuid")) %>% 
  issue_log(question = "uuid", issue = "This entry is apprearing twice in the data", action = "f")

################################################################################################################

## Check 6: ki age
ki_age_check <- df %>% 
  filter(ki_age=="90_above") %>% 
  issue_log(question = "ki_age",
            issue = "ki age is greeter than 90 years, please confirm", action = "f")


## Check 11: ki age and role
ki_age_check_role <- df %>% 
  filter((ki_age=="30_49" | ki_age=="18_29") & ki_role=="elder") %>% 
  issue_log(question = "ki_age",
            issue = "ki age is is less than 50 and his role is an elder", action = "f")

check_site_duration <-df %>% 
  filter((duration_site_established_in_months < 4 & cccm_idps_arrival=="morethansixmonths" | cccm_idps_arrival=="fourtosixmonths")|
           (duration_site_established_in_months < 2 & cccm_idps_arrival=="morethansixmonths" | cccm_idps_arrival=="fourtosixmonths"|cccm_idps_arrival=="onetothreemonths")|
           (duration_site_established_in_months < 6 & cccm_idps_arrival=="morethansixmonths")) %>% 
  issue_log(question = "cccm_idps_arrival",
            issue = "IDPs arrived in this camp way before it was established, please check", action = "f")


## Check 7: ki_role inconsistency 
ki_role_check <- df %>% 
  filter(ki_role %in% c("gatekeeper", "camp_leader", "site_manager") & camp_structure == "no") %>% 
  issue_log(question = "camp_structure",
            issue = "ki has already identifed him/here selft as site manager or gatekeeper, but again reporting no camp structure exist, please confirm",
            action = "c")

## Check 8: ki resident
ki_resident_check <- df %>% 
  filter(ki_role == "site_resident" & ki_resident == "no") %>% 
  issue_log(question = "ki_resident",
            issue = "ki reported in the role as site resident and again reported the ki_resident quesiton as no, please confirm",
            action = "c")

## Check 9: Population Checks
cccm_shelters <- df %>% 
  filter(cccm_populationestimates_shelters < 50) %>% 
  issue_log(question = "cccm_populationestimates_shelters",
            issue = "the number shelters in the camp are less then 50, please confirm with enumerator",
            action = "c")
###########families
cccm_families <- df %>% 
  filter(cccm_populationestimates_families < 100) %>% 
  issue_log(question = "cccm_populationestimates_families",
            issue = "the number of families in the camp are less than 100, please confirm with enumeator",
            action = "c")
###########individuals
cccm_individuals <- df %>% 
  filter(cccm_populationestimates_individuals < 150) %>% 
  issue_log(question = "cccm_populationestimates_individuals",
            issue = "the number of individuals in the camp are less than 150, please confirm with enumerator",
            action = "c")

## Check 3: IDP coverage#########################################################################################################
n_occur <- data.frame(table(df$idp_code))
n_occur <- n_occur[n_occur$Freq > 4,]
over_sampling <- df[df$idp_code %in% n_occur$Var1[n_occur$Freq > 4],] %>% 
  issue_log(question = "idp_code", issue = "idp site interviewed more than 4 times", action = "f")



## Check 4:DUPLICATED Ki role per site
f<-as.data.frame(df %>% group_by(idp_code,ki_role) %>%dplyr::summarise(n=n()))
f[f$n>1,]->f

over_sampling1 <- df[df$idp_code %in%f$idp_code,] %>% 
  issue_log(question = "ki_role", issue = "you have interviewed more than 1 Ki_role in one site,verify", action = "f")


							
## Check 5: Other options
other_options <- df %>%  
  dplyr::select(ends_with("_other"), "localisation_region_label", "district_name", "today","enum_name", "uuid", "ki_contact") %>% 
  pivot_longer(ends_with("_other"),
               names_to = "question",
               values_to = "old_value") %>% 
  filter(!is.na(old_value)) %>% 
  mutate(issue = "please translate other options and recode them in the respective choice if possible",
         new_value = "",
         Reason = "",
         action = "c") %>% 
  select("uuid", "localisation_region_label", "district_name", "today", "enum_name","question", "issue",
         "old_value", "new_value","Reason", "ki_contact","action")
# # renaming column heads to merge with the clogs
names(other_options)[names(other_options) == "enum_name"] <- 'enumerator'
names(other_options)[names(other_options) == "localisation_region_label"] <- 'region'
names(other_options)[names(other_options) == "district_name"] <- 'district'
##Remove setllment other


l4 <- l4 <- questions %>% filter(grepl("(numerical|integer)",type)) %>% pull(name)
l4 <- l4[which(l4 %in% colnames(df))]

l4 <- l4[l4 %!in% c("referral_phone" )]

#Outliers

names(df)[names(df) == "enum_name"] <- 'enumerator'
names(df)[names(df) == "localisation_region_label"] <- 'region'
names(df)[names(df) == "district_name"] <- 'district'
# detect outliers
outliers.sub1 <- df %>%  
  select("uuid", "region","district","today","enumerator","ki_contact", all_of(l4)) %>% 
  detect.outliers(., method="sd-linear", n.sd=3)
outliers.sub2 <- df %>% 
  select("uuid", "region","district","today","enumerator","ki_contact", all_of(l4)) %>% 
  detect.outliers(., method="sd-log", n.sd=3)
outliers.sub3 <- df %>% 
  select("uuid", "region","district","today","enumerator","ki_contact", all_of(l4)) %>% 
  detect.outliers(., method="iqr-linear")
outliers.sub4 <- df %>% 
  select("uuid", "region","district","today","enumerator","ki_contact", all_of(l4)) %>% 
  detect.outliers(., method="iqr-log")
outliers <- rbind(outliers.sub1, outliers.sub2, outliers.sub3, outliers.sub4)
outliers <- outliers %>% mutate(mid=paste0(uuid, issue))
outliers <- outliers[!duplicated(outliers$mid),] %>% select(-mid) 

outliers <- outliers[!(row.names(outliers) %in% c("enumerator")),]
outliers <- outliers %>% filter(question !="enumerator")




cleaning_log <- rbind(check_consent,less_time_surveys,long_time_surveys,duplicated_surveys, duplicated_res,other_options,outliers,ki_age_check,ki_role_check,
                      ki_resident_check,cccm_shelters,cccm_families,cccm_individuals,over_sampling1,over_sampling,ki_age_check_role,check_site_duration)


idp <- df %>% select(uuid,idp_code)
#Compare today cleaning log from the previous one
cleaning_log <- cleaning_log %>% mutate(unique=paste0(uuid,question))
previous <- read_excel("master/master.xlsx")%>% mutate(unique=paste0(uuid,question)) 

clog  <- filter(cleaning_log, unique %!in% previous$unique)%>% left_join(idp, by = c("uuid"))
master <- rbind(clog,previous) %>% select(-unique)

write.xlsx(master,"master/master.xlsx")

clog <- clog [!duplicated(clog$unique),] %>% select(-unique)

# cleaning log files
clog$district<-tolower(gsub(" ","_",clog$district))
#adding resposible FOs and splitting 
f_team<- read_excel("input/FOs/Field team work distribution.xlsx")
f_team$Locations<-tolower(gsub(" ","_",f_team$Locations))
f_team$`Responsible FO`<-gsub("\\/","_",f_team$`Responsible FO`)
clog$FO<-f_team$`Responsible FO`[match(clog$district,f_team$Locations)]
clog$region_FO<-paste0(clog$region,"__",clog$FO)

spt1<-split(clog,clog$FO)
setDT(clog)
outputdir<-"C:\\Users\\aaron.langat\\ACTED\\IMPACT SOM - Unit 1 - Intersectoral\\SOM 23 DSA\\04_Data\\03_Data_Cleaning\\"
#clogs for Abdikani kunow and Abukar
spt1$`Abdikani Kunow_Hassan Abukar`->abdikani
spt11<-split(abdikani,abdikani$region_FO)
purrr::imap(spt11, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/01_Abdikani Kunow_Hassan Abukar/",.y,today, '.xlsx')))
#clogs for barre
spt1$`Abdirahima Barre`->barre
spt2<-split(barre,barre$region_FO)
purrr::imap(spt2, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/02_Abdirahima Barre/",.y,today, '.xlsx')))

#referell contacts hajir
spt1$`Hajir Abdi`->hajir
spt4<-split(hajir,hajir$region_FO)
purrr::imap(spt4, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/03_Hajir Abdi/",.y,today, '.xlsx')))

#referell contacts Kala
spt1$`Mohamd Kala`->kala
spt5<-split(kala,kala$region_FO)
purrr::imap(spt5, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/04_Mohamed Kala/",.y,today, '.xlsx')))

#referell contactsHassan
spt1$`Mohamed Hassan`->hassan
spt6<-split(hassan,hassan$region_FO)
purrr::imap(spt6, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/05_Mohamed Hassan/",.y,today, '.xlsx')))
#referell contactsOmAR
spt1$`Omar Abdikarin`->omar
spt7<-split(omar,omar$region_FO)
purrr::imap(spt7, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/06_Omar Abdikarin/",.y,today, '.xlsx')))
#referell contacts
spt1$`Suleiman Mohamed`->suleiman
spt8<-split(suleiman,suleiman$region_FO)
purrr::imap(spt8, ~openxlsx::write.xlsx(.x, paste0(outputdir,"/07_Suleiman Mohamed/",.y,today, '.xlsx')))
# # anonymise data

# raw_data <- anonymise_dataset(data = df, 
#                               variables_to_remove = c())

# # prepare locations cleaning log files
# 
# awdal_clog <- scc_clog(clog, coverage = c("awdal"))
# banadir_clog <- scc_clog(clog, coverage = c("banadir"))
# bari_clog <- scc_clog(clog, coverage = c("bari"))
# bay_clog <- scc_clog(clog, coverage = c("bay"))
# bakool_clog <- scc_clog(clog, coverage = c("bakool"))
# galgaduud_clog <- scc_clog(clog, coverage = c("galgaduud"))
# nugaal_clog <- scc_clog(clog, coverage= c("nugaal"))
# hiraan_clog<- scc_clog(clog, coverage= c("hiraan"))
# middle_shabelle_clog <- scc_clog(clog, coverage = c("middle_shabelle"))
# lower_juba_clog <- scc_clog(clog, coverage = c("lower_juba"))
# lower_shabelle_clog <- scc_clog(clog, coverage = c("lower_shabelle"))
# sanaag_clog <- scc_clog(clog, coverage = c("sanaag"))
# mudug_clog <- scc_clog(clog, coverage = c("mudug"))
# sool_clog <- scc_clog(clog, coverage = c("sool"))
# gedo_clog <- scc_clog(clog, coverage = c("gedo"))
# togdheer_clog <- scc_clog(clog, coverage = c("togdheer"))
# woqooyi_galbeed<- scc_clog(clog, coverage = c("woqooyi_galbeed"))
outputdir<-"C:\\Users\\rodhiambo\\ACTED\\IMPACT SOM - 02_Research\\01_REACH\\Unit 1 - Intersectoral\\SOM 23 MSNA\\04_Data\\02_Data Cleaning\\"
# clenaing log files
write.xlsx(clog, paste0("output/clog_",today,".xlsx"))
# locations
# write.xlsx(awdal_clog, paste0(outputdir,"16_Awdal/awdal_msna_clog_",today,".xlsx"))
# write.xlsx(banadir_clog, paste0(outputdir,"02_Banadir/banadir_msna_clog_",today,".xlsx"))
# write.xlsx(bari_clog, paste0(outputdir,"12_Bari/bari_msna_clog_",today,".xlsx"))
# write.xlsx(bay_clog, paste0(outputdir,"03_Bay/bay_msna_clog_",today,".xlsx"))
# write.xlsx(bakool_clog, paste0(outputdir,"01_Bakool/bakool_msna_clog_",today,".xlsx"))
# write.xlsx(galgaduud_clog, paste0(outputdir,"04_Galgaduud/galgaduud_msna_clog_",today,".xlsx"))
# write.xlsx(middle_shabelle_clog, paste0(outputdir,"08_Middle Shabelle/middle_shabelle_msna_clog_",today,".xlsx"))
# write.xlsx(lower_juba_clog, paste0(outputdir,"07_Lower Juba/lower_juba_msna_clog_",today,".xlsx"))
# write.xlsx(lower_shabelle_clog, paste0(outputdir,"06_Lowe Shabelle/lower_shabelle_msna_clog_",today,".xlsx"))
# write.xlsx(nugaal_clog, paste0(outputdir,"10_Nugaal/nugaal_msna_clog_",today,".xlsx"))
# write.xlsx(mudug_clog, paste0(outputdir,"09_Muduug/mudug_msnaclog_",today,".xlsx"))
# write.xlsx(hiraan_clog, paste0(outputdir,"05_Hiraan/hiraan_msna_clog_",today,".xlsx"))
# write.xlsx(sanaag_clog, paste0(outputdir,"14_Sanaag/sanaag_msna_clog_",today,".xlsx"))
# write.xlsx(sool_clog, paste0(outputdir,"11_Sool/sool_msna_clog_",today,".xlsx"))
# write.xlsx(gedo_clog, paste0(outputdir,"17_Gedo/gedo_msna_clog_",today,".xlsx"))
# write.xlsx(togdheer_clog, paste0(outputdir,"13_Togdheer/togdheer_msna_clog_",today,".xlsx"))
# write.xlsx(woqooyi_galbeed, paste0(outputdir,"15_Woqooyi Galbeed/woqooyi_galbeed_msna_clog_",today,".xlsx"))



# # 
# clog_input <- list.files(path = "C:\\Users\\aaron.langat\\Documents\\R\\01_DSA\\03_Cleaning\\received",
#                          pattern = "*.xlsx",
#                          full.names = TRUE) %>%
#   lapply(read_excel) %>%rbind.fill
# #Export binded logs for reviewing
# # 
#  write.xlsx(clog_input, paste0("output/merged_clog.xlsx"))
# # 
# clog_input <- read.xlsx("master/cleaning_log.xlsx")
# 
# # inco-operate cleaning log
# 
# # #Convert  all columns to character format
# df <- df %>%
#   mutate(across(everything(), as.character))
# clog_input <- clog_input %>%
#   mutate(across(everything(), as.character))
# 
# 
# get_indices <- function(uuids,uuid,questions,question) {
#   row_number = which(uuids == uuid)
#   col_number = which(questions == question)
#   return(list(row_number = row_number ,col_number = col_number ))
# }
# 
# update_value <- function(df,index_i,index_j,new.value) {
#   df[index_i,index_j] = new.value
#   return(df)
# }
# for (i in 1:nrow(clog_input)) {
#   indices = get_indices(df$uuid,
#                         as.character(clog_input[i,"uuid"]),
#                         colnames(df),
#                         as.character(clog_input[i,"question"]))
#   index_i = indices$row_number
#   index_j = indices$col_number
#   df <- update_value(df,index_i,index_j,clog_input[i,"new.value"])
# }
# 
# 
# #Make changes to the calculate column
# 
# write.xlsx(df, paste0("output/cleaned_data.xlsx"))

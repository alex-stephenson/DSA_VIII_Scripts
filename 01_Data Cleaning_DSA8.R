################## DSA VIII SCRIPT ############## 

# SECTION 1 LOAD PACKAGES AND HARD CODE VARIABLES TEST FROM SULAIMAN'S ACCOUNT


rm(list = ls())
today <- Sys.Date()
date_time_now <- format(Sys.time(), "%b_%d_%Y_%H%M%S")


if (!require("pacman")) install.packages("pacman")
if (!require("robotoolbox")) remotes::install_gitlab("dickoa/robotoolbox") ## install if needed robotoolbox
if (!require("ImpactFunctions")) remotes::install_github("alex-stephenson/ImpactFunctions") ## install some key functions made for SOM REACH

p_load(rio,
       tidyverse,
       koboquest,
       hypegrammaR,
       sjmisc,
       keyring)
# load packages
library(cleaninginspectoR, quietly = T)
library(koboAPI, quietly = T)
library(cleaningtools, quietly = T)
library(robotoolbox, quietly = T)
library(ImpactFunctions)
library(openxlsx)
library(readxl)
library(dplyr)
# library(butteR)
library(data.table)
library(stringr)
library(stringi)
library(lubridate)
library(DT)
library(kableExtra)
library(ggplot2)
library(purrr)
## Hard code values
consent <- "consent"
mindur <- 25
maxdur <- 120

## Set this to TRUE if you want to select the date through the bat file. If FALSE you need to set the date in the script.
dynamic_date <- FALSE ## if false the date must be set in line 184
## set this if you want to access the data from the server. If you are going to choose the file choose TRUE
access_from_server <- TRUE ## if false the file will be manually selected

###################################################
###### STEP 2 - IMPORT DATA AND FUNCTIONS #########
###################################################

#to make the scripts reusable relative file paths should be used
user_login <- Sys.info()[["user"]]
wd_path <- sprintf(r"(C:/Users/%s/ACTED/IMPACT SOM - 02_Research/01_REACH/2024_25/03_DDSU/SOM2204 _DSA VIII 2025/03_Data_Analysis/DSA_VIII_Scripts)", user_login)
setwd(wd_path)


## get data input from the .BAT file we are using to run the script. 
args <- commandArgs(trailingOnly = TRUE)

# Parse args into a named list
arg_list <- list()
for (arg in args) {
  key_value <- strsplit(arg, "=")[[1]]
  if (length(key_value) == 2) {
    arg_list[[key_value[1]]] <- key_value[2]
  }
}

# Assign default values if certain arguments are missing
username <- arg_list[["username"]] %||% "alex_stephenson"
asset_id <- arg_list[["asset_id"]] %||% "asSj6Aq8kDg5FULShCKbjN"
date_selection <- arg_list[["date_selection"]] %||% NA  # Optional, can be NA

# Use these variables in your code
print(paste("Username:", username))
print(paste("Asset ID:", asset_id))
print(paste("Date Selection:", date_selection))

if (access_from_server == TRUE) {
  print("Accessing from server...")
}

## call the function to access the kobo server using the credentials entered in the bat file, or the default ones if running locally.
if (access_from_server == TRUE) {

  df_raw <- get_kobo_data(un = username, asset_id = asset_id)
  df <- df_raw %>%
    dplyr::rename(
      "survey_uuid" = "uuid",
      "uuid" = "_uuid",
      "idp_site" = "localisation_site",
      # `_observation_gps_latitude` = "geopoint_latitude",
      # `_observation_gps_longitude` = "geopoint_longitude",
      # `_observation_gps_altitude` = "geopoint_altitude",
      # `_observation_gps_precision` = "geopoint_precision"
    ) ## the data has slightly different namings when accessed via API, so standardise here so can be run both ways
  
  print("Kobo data successfully accessed from server")
  
} else {
  df <- readxl::read_excel(choose.files()) %>%
    dplyr::rename(
      "uuid" = "_uuid",
      "idp_site" = "localisation_site")
}  

  
df <- df %>%
  mutate(
    idp_code = case_when(
      # Apply the first condition to add "2023-" prefix when date is 12/11/2024 and region is "Banadir"
      today == as.Date("2024-11-12") & localisation_region == "SO22" ~ str_replace(idp_code, "^CCCM-", "CCCM-2023-"),
      
      # Apply the second condition to replace "2023" with "2023(U)" for specified site codes
      idp_code %in% c("CCCM-2023-SO220117-0353", "CCCM-2023-SO220117-0404", 
                                  "CCCM-2023-SO220117-0382", "CCCM-2023-SO220117-0383", 
                                  "CCCM-2023-SO220117-0394", "CCCM-2023-SO220117-0414") ~ str_replace(idp_code, "2023", "2023(U)"),
      
      # Otherwise, retain original values
      TRUE ~ idp_code
    ),
    nfi_access_distance_min_fem = case_when(
      nfi_access_distance_min_fem == "1530" ~ "15_30",
      nfi_access_distance_min_fem == "3160" ~ "31_60",
      TRUE ~ nfi_access_distance_min_fem
    ),
    nfi_access_distance_min_male = case_when(
      nfi_access_distance_min_male == "1530" ~ "15_30",
      nfi_access_distance_min_male == "3160" ~ "31_60",
      TRUE ~ nfi_access_distance_min_male
    ),
    nfi_access_dist_min_int_male = case_when(
      nfi_access_distance_min_male == 'less_15' ~ 0,
      nfi_access_distance_min_male == '15_30' ~ 15,
      nfi_access_distance_min_male == '31_60' ~ 31,
      nfi_access_distance_min_male == 'more_60' ~ 60,
      TRUE ~ 999
    ),
    nfi_access_dist_min_int_female = case_when(
      nfi_access_distance_min_fem == 'less_15' ~ 0,
      nfi_access_distance_min_fem == '15_30' ~ 15,
      nfi_access_distance_min_fem == '31_60' ~ 31,
      nfi_access_distance_min_fem == 'more_60' ~ 60,
      TRUE ~ 999
    )
  )

df$duration_site_established_in_months <- as.integer(df$duration_site_established_in_months)

## read in survey data
#district_file <- read.csv("input/idp_list_OLD_INCORRECT.csv")
koboToolPath = sprintf(r"(C:\Users\%s\ACTED/IMPACT SOM - 02_Research/01_REACH/2024_25/03_DDSU/SOM2204 _DSA VIII 2025\02_Data_Collection\01_Tool\REACH_2024_SOM_DSA_Survey_Tool_VIII.xlsx)", user_login)
questions = import(koboToolPath,sheet="survey") %>% 
  filter(!is.na(name))
choices = import(koboToolPath,sheet="choices")


## read in site level data
site_path <- sprintf(r"(C:\Users\%s\ACTED/IMPACT SOM - 02_Research/01_REACH/2024_25/03_DDSU/SOM2204 _DSA VIII 2025\01_Research_Design\Reference_Data\idp-site-master-list-sep-2024.xlsx)", user_login)
site_data <- read_excel(site_path, sheet = "Sites for Field") %>%
  mutate(District = str_to_title(District))


## read in FO site and locations - this data is used to map the respective field officers to each district. 
fo_base_assingment_str <- sprintf(r"(C:\Users\%s\ACTED/IMPACT SOM - 02_Research/01_REACH/2024_25/03_DDSU/SOM2204 _DSA VIII 2025\03_Data_Analysis\DSA_VIII_Scripts\input/Field team work distribution.xlsx)", user_login)
fo_district_mapping <- read_excel(fo_base_assingment_str) %>%
  mutate(Locations = str_to_title(Locations))

## join site data to field data
field_officer_location <- site_data %>%
  left_join(fo_district_mapping, by = join_by("District" == "Locations")) %>%
  distinct(`CCCM IDP Site Code`, District, Responsible_FO) %>%
  mutate(Responsible_FO = str_replace_all(Responsible_FO, "/", "_"),
         Responsible_FO = ifelse(Responsible_FO == "" | is.na(Responsible_FO), "No_FO", Responsible_FO)) %>%
  filter(!is.na(`CCCM IDP Site Code`))
  
##rename any blank values 
field_officer_location$Responsible_FO <- ifelse(field_officer_location$Responsible_FO == "" | is.na(field_officer_location$Responsible_FO), "No_FO", field_officer_location$Responsible_FO)

## join the district name to the site df (in the responses it just has the district p code)
df <- df %>%
  left_join(choices %>%
              filter(!is.na(region))  %>%
              select(name, `label::English (en)`) %>%
              dplyr::rename(localisation_region_label = `label::English (en)`),
            by = join_by("localisation_region" == "name")) %>%
  relocate(localisation_region_label, .after = "localisation_region")

df <- df %>%
  mutate(referral_yn = case_when(
    str_starts(referral_person, "y") ~ "Yes",
    str_starts(referral_person, "n") ~ "No",
    TRUE ~ NA_character_
  ))


df <- df %>% filter(today >= as.Date("2024-11-12"))  

## of you have selected the dynamic data, it will read it in from the bat input. If else it will be manually selected below. 
# if (dynamic_date) {
# 
#   df_check <- df %>% 
# #    left_join(district_file, by = c("idp_code", "district_name")) %>%
#     mutate(submission_date = format(ymd_hms(`_submission_time`), "%Y-%m-%d")) %>%
#     filter(submission_date == date_selection) 
#   
#   } else {
#     
#     df <- df %>% 
#  #     left_join(district_file, by = c("idp_code", "district_name")) %>%
#       mutate(submission_date = format(ymd_hms(`_submission_time`), "%Y-%m-%d")) %>%
#       filter(submission_date == "2024-11-13") 
#   }
# 
# if (nrow(df) == 0) {
#   stop("Data ingested is empty. Did you select the right date?")
# }
# }
df <- df |> mutate(submission_date = format(ymd_hms(`_submission_time`), "%Y-%m-%d")) %>%
  filter(today >= as.Date("2024-11-12")) 

print("Data successfully filtered and prepared")

Sys.sleep(0.5)

## add a flag for whether there is a referral included for later


tool.survey<- questions%>%
  filter(name%in%colnames(df))

####### CONVERT COLUMNS TO REQUIRED DATA TYPE  #####

cols.integer <- tool.survey %>%
  filter(type == "integer") %>%
  pull(name)


cols.gps <- tool.survey %>%
  filter(type == "gps") %>%
  pull(name)

cols.characters_one <- tool.survey %>%
  filter(str_detect(type, "select_one")) %>%
  pull(name)

cols.characters_multiple <- tool.survey %>%
  filter(str_detect(type, "select_multiple")) %>%
  pull(name)

df <- mutate_at(df, cols.gps, as.integer)
df <- mutate_at(df, cols.integer, as.integer)
df <- mutate_at(df, cols.characters_one, as.character)
df <- mutate_at(df, cols.characters_multiple, as.character)

#######
# cols.integer <- filter(tool.survey, type=="calculate")$name
# df <- mutate_at(df, cols.integer, as.integer)
# 
# #convert select_one to character



########################################################
########### STEP 3 -- CLEAN THE DATA ###################
########################################################

time_check <- function(df, time_min, time_max){
  df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                      CHECK_interview_duration = case_when(
                        interview_duration < time_min ~ "Too short",
                        interview_duration > time_max ~ "Too long",
                        TRUE ~ "Okay"
                      )
  )
  return(df)
}

df <- time_check(df, time_min = mindur, time_max = maxdur)


"%!in%" <- Negate("%in%")

############################## time check by audit file

df <- df |>
  dplyr::rename(
    interview_duration_start_end = interview_duration,
    CHECK_interview_duration_start_end = CHECK_interview_duration
  )

### time check based on audit files ####
audit_file_dir = "C:/Users/sulai/ACTED/IMPACT SOM - KenSom GIS and DATA team - 01_DSA/REACH_SOM_DSA_VIII/audit_files/"

time_min <- 25
time_max <- 120

df <- check_int_duration(df_raw = df, x_uuid="uuid", audit_dir_path = audit_file_dir, today = "today")

df$interview_duration <- as.numeric(df$interview_duration)

if ("interview_duration" %in% names(df)) {
  # classifying interview duration validity using interview duration
  df$interview_duration <- df$interview_duration %>% unlist()
  
  # classifying interview duration into categories
  df <- df %>% 
    mutate(CHECK_interview_duration = case_when(
      interview_duration < time_min ~ "Too short",
      interview_duration > time_max ~ "Too long",
      TRUE ~ "Okay"
    ),
    time_validity = case_when(
      CHECK_interview_duration %in% c("Too short", "Too long") ~ "Deleted",
      TRUE ~ "Valid"
    ),
    
    short_interview = case_when(
      time_validity == "Deleted" & interview_duration < time_min  ~ "Deleted",
      TRUE ~ "Okay"
    ),
    
    long_interview = case_when(
      time_validity == "Deleted" & interview_duration > time_max ~ "Deleted",
      TRUE ~ "Okay"
    ))
  
} else {
  stop("Calculating Interview duration was unsuccessful!")
}

table(df$CHECK_interview_duration)
table(df$CHECK_interview_duration_start_end)
df$validity <- "Okay"

df$validity[df$CHECK_interview_duration == "Too short"] <- "invalid"
df$validity[df$CHECK_interview_duration == "Too long"] <- "invalid"

## this is the dashboard data that we'll use to make the FO dashboard
dashboard <- df %>% 
  left_join(field_officer_location, by = join_by(idp_code == `CCCM IDP Site Code`)) %>%
  select(start,end,today,deviceid, Responsible_FO,	enum_name,	consent,	district_name,	localisation_region_label,	idp_code,	idp_site, idp_code_verification,
         idp_code_districti_verification,	ki_role,	ki_role_other, ki_gender,	observation_faecalmatter,	observation_shelters_flood,
         observation_publiclighting,	observation_sufficient_space,	observation_main_secondary_accessroad,
         ,geopoint, `_geopoint_latitude`, `_geopoint_longitude`, `_geopoint_altitude`, `_geopoint_precision`,
         ,validity,interview_duration_start_end,interview_duration, `uuid`
  )

write_csv(dashboard, sprintf(r"(C:\Users\%s\ACTED\IMPACT SOM - 02_Research\01_REACH\Data Team\Shiny\DSA_VIII_Monitoring\REACH_SOM_DSA_VIII_Field_Dashboard\data/dashboard_data.csv)", user_login))



#############load datasets ##################
print("Dashboard prepped")



########### code logical checks for the check_logical function call ##############

## which IDP site has been visited > times
n_occur_idp_code <- df %>%
  dplyr::count(idp_code) %>%
  filter(n > 4)

## which IDP site has had a KI role > 1

count_ki_roles_site <- df %>% 
  group_by(idp_code,ki_role) %>%
  dplyr::summarise(n=n()) %>%
  filter(n > 1)

## create dataframe of all the checks we'll run

check_list <- data.frame(
  name = c(
    "KI_Consent",
    "KI_too_old",
    "KI_age_and_role",
    "IDP_arrival_time",
    "ki_role_camp_structure",
    "ki_role_resident",
    "small_camp",
    "small_families",
    "low_number_of_individuals",
    "idp_code_count",
    "ki_role_per_site"),
  check = c(
    r"(consent == "no")",
    "ki_age == \"90_above\"",
    r"((ki_age=="30_49" | ki_age=="18_29") & ki_role=="elder")",
    r"((duration_site_established_in_months < 4 & (cccm_idps_arrival=="morethansixmonths" | cccm_idps_arrival=="fourtosixmonths"))|
    (duration_site_established_in_months < 2 & (cccm_idps_arrival=="morethansixmonths" | cccm_idps_arrival=="fourtosixmonths"|cccm_idps_arrival=="onetothreemonths"))|
    (duration_site_established_in_months < 6 & (cccm_idps_arrival=="morethansixmonths")))",
    r"(ki_role %in% c("gatekeeper", "camp_leader", "site_manager") & camp_structure == "no")",
    r"(ki_role == "site_resident" & ki_resident == "no")",
    r"(cccm_populationestimates_shelters < 50)",
    r"(cccm_populationestimates_families < 100)",
    r"(cccm_populationestimates_individuals < 150)",
    r"(idp_code %in% n_occur_idp_code$idp_code)",
    r"(idp_code %in% count_ki_roles_site$idp_code)"
  ),
  description = c(
    "KI has not given consent. Please review why.",
    "KI respondent has said they are older than 90. Please check",
    "KI age is is less than 50 and his role is an elder",
    "IDPs claim they have arrived before the site was set up",
    "KI has already identifed him/here self as site manager or gatekeeper, but is reporting that no camp structure exists, please confirm",
    "KI has reported their role is site resident but separately said they are not a site resident. Please review with the enumerator.",
    "The number shelters in the camp are less then 50, please confirm with enumerator",
    "The number of families in the camp are less than 100, please confirm with enumeator",
    "The number of individuals in the camp is less than 150. Please confirm with the enumerator",
    "The IDP Site has been interviewed more than four times. Please verify.",
    "The same role has been interviewed in an IDP site more than once. Please verify"
  ),
  columns_to_clean = c(
    "consent",
    "ki_age",
    "ki_age, ki_role",
    "duration_site_established_in_months, cccm_idps_arrival",
    "ki_role, camp_structure",
    "ki_role, ki_resident",
    "cccm_populationestimates_shelters",
    "cccm_populationestimates_families",
    "cccm_populationestimates_individuals",
    "idp_code",
    "idp_code"
    
  )
)

############ data cleaning ############

outlier_excluded_questions <- questions %>%
  filter(type != 'integer') %>% 
  pull(name) %>%
  unique()

# intersect between the dataset and the kobo tool questions to make sure we get a clean list
excluded_questions_in_data <- intersect(colnames(df), outlier_excluded_questions)

df <- df %>%
  left_join(field_officer_location, by = join_by(idp_code == `CCCM IDP Site Code`))

df$Responsible_FO <- ifelse(df$Responsible_FO == "" | is.na(df$Responsible_FO), "No_FO", df$Responsible_FO)

# df_no_pii <- df %>%
#   select(-c('enum_name', 'ki_name', 'ki_contact', 'observation_gps', 'referral_name', 'referral_phone', 
#             '_observation_gps_precision',
#             '_observation_gps_latitude', '_observation_gps_longitude', '_observation_gps_altitude'))

yesterday_df <- read_xlsx("../01_DSA_III_Download_Data/raw_data/DSA_2024_REACH_SOM_-_all_versions_-_False_-_2024-11-25-05-24-50.xlsx")

recent_df <- df |> filter(!uuid %in% yesterday_df$`_uuid`)


group_by_fo <- recent_df %>%
  dplyr::group_by(Responsible_FO)

checked_data_by_fo <- group_by_fo %>%
  group_split() %>%
  purrr::map(~ 
               check_duplicate(
                  dataset = .,
                  columns_to_check = c("ki_contact"),
                  log_name = "duplicate phone",
                  uuid_column = "uuid"
                ) %>%
                check_duration(
                  column_to_check = "interview_duration",
                  uuid_column = "uuid",
                  log_name = "duration_log",
                  lower_bound = 25,
                  higher_bound = 120
                ) %>%
                check_duplicate(uuid_column = "uuid") %>%
               # check_soft_duplicates(
               #   dataset = .,
               #   kobo_survey =questions,
               #   uuid_column = "uuid",
               #   idnk_value = "dnk",
               #   sm_separator = "//",
               #   log_name = "soft_duplicate_log",
               #   threshold = 7,
               #   return_all_results = T
               # ) %>%
#                check_pii(element_name = "checked_dataset", uuid_column = "uuid") %>%
                check_others(
                  uuid_column = "uuid",
                  columns_to_check = names(
                    df |>
                      dplyr::select(ends_with("_other")) |>
                      dplyr::select(-contains("/"))
                  )
                ) %>%
                check_value(
                  uuid_column = "uuid",
                  element_name = "checked_dataset",
                  values_to_look = c(999, 9999)
                ) %>%
               # check_outliers(
               #   uuid_column = "uuid",
               #   element_name = "checked_dataset",
               #   kobo_survey = questions,
               #   kobo_choices = choices,
               #   cols_to_add_cleaning_log = NULL,
               #   strongness_factor = 3,
               #   minimum_unique_value_of_variable = 10,
               #   remove_choice_multiple = TRUE,
               #   sm_separator = "/",
               #   columns_not_to_check = c(excluded_questions_in_data, "interview_duration", "CHECK_interview_duration", "_observation_gps_altitude","_observation_gps_precision","_observation_gps_longitude","_observation_gps_latitude","_id")
               #  ) %>%
               check_logical_with_list(list_of_check = check_list,
                                       check_id_column = "name",
                                       check_to_perform_column = "check",
                                       columns_to_clean_column = "columns_to_clean",
                                       description_column = "description"
               ))



## produce cleaning logs
cleaning_log <- checked_data_by_fo %>%
  purrr::map(~ .[] %>%
               create_combined_log() %>% 
               add_info_to_cleaning_log(
                 list_of_log = .,
                 dataset = "checked_dataset",
                 cleaning_log = "cleaning_log",
                 information_to_add = c("idp_code", "enum_name",  "district_name", "ki_contact", "ki_name","Responsible_FO", "Responsible_FO", "today")
#                 information_to_add = c("settlement", "district", "enum_code", "ki_name", "ki_phone_number", "comment")
    
               )
  )

for (k in seq_along(cleaning_log)) {  
  clog <- cleaning_log[[k]][[2]]  
  
  clog$Field_SFO_Comment <- NA
  
  clog$new_value <- as.character(clog$new_value)
  
  cleaning_log[[k]][[2]] <- clog  
}

for (l in seq_along(cleaning_log)) {  
  clog <- cleaning_log[[l]][[2]]  
  
  clog <- clog |> filter(issue %nin% c("outlier (normal distribution)", "outlier (log distribution)"))
  
  cleaning_log[[l]][[2]] <- clog  
} 

## create folders for FOs if they don't exist (useful to automate this so it automatically happens if we add new FOs)

FO_names <- field_officer_location %>% distinct(Responsible_FO) 
FO_names[nrow(FO_names) + 1, ] <- "No_FO"

for (i in 1:nrow(FO_names)) {
  dir_path <- paste0("clogs/", FO_names[i, 1])
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
}

## create the clogs
cleaning_log %>% purrr::map(~ create_xlsx_cleaning_log(.[], 
                                                       cleaning_log_name = "cleaning_log",
                                                       change_type_col = "change_type",
                                                       column_for_color = "check_binding",
                                                       header_front_size = 10,
                                                       header_front_color = "#FFFFFF",
                                                       header_fill_color = "#ee5859",
                                                       header_front = "Arial Narrow",
                                                       body_front = "Arial Narrow",
                                                       body_front_size = 10,
                                                       use_dropdown = F,
                                                       sm_dropdown_type = "numerical",
                                                       kobo_survey = questions,
                                                       kobo_choices = choices,
                                                       output_path = paste0("clogs/",
                                                                            unique(.[]$checked_dataset$Responsible_FO),
                                                                            "/cleaning_log_",
                                                                            unique(.[]$checked_dataset$Responsible_FO),
                                                                            "_",
                                                                            date_time_now,
                                                                            ".xlsx")))

### combine cleaning_logs for the dashboard (the clogs are produced using openxlsx which powerBI does not support the output of. Readxl is fine. )

xlsx_files <- list.files(path = "clogs", pattern = ".xlsx", recursive = TRUE, full.names = F)

read_clogs <- function(file) {
  read_excel(file, sheet = "cleaning_log") %>% 
    mutate(source_file = file)  # Optionally, add a column to track the source file
}

combined_clogs <- bind_rows(lapply(paste0("clogs/", xlsx_files), read_clogs))

combined_clogs_distinct <- combined_clogs %>%
  select(-c(source_file)) %>%
  filter(issue %nin% c("outlier (normal distribution)", "outlier (log distribution)")) |> 
  distinct()

writexl::write_xlsx(combined_clogs_distinct, sprintf(r"(C:\Users\%s\ACTED\IMPACT SOM - 02_Research\01_REACH\Data Team\Shiny\DSA_VIII_Monitoring\REACH_SOM_DSA_VIII_Field_Dashboard\data/combined_clogs.xlsx)", user_login))



#each KI will be asked if they have other potential contacts for the DSA, those are grouped by FO similar to the clogs
contact_data_by_fo <- group_by_fo %>%
  dplyr::group_split() %>%
  purrr::map(~ filter(., referral_yn == "Yes") %>%
               select(Responsible_FO, idp_code, ki_name, referral_name, referral_phone)
               )

# remove the groups that don't have any values (only really needed at the start)
contact_data_by_fo <- Filter(function(x) nrow(x) > 0, contact_data_by_fo)

### make the file directories if required for the referrals
#for (i in 1:nrow(FO_names)) {
#  dir_path <- paste0("referral/", FO_names[i, 1])
#  
#  if (!dir.exists(dir_path)) {
#    dir.create(dir_path, recursive = TRUE)
#  }
#  
#}

# Function to write or append data to Excel
write_or_append_excel <- function(data, fo_name, output_dir) {
  
  # Construct the file path
  file_path <- file.path(output_dir, paste0("referral/", fo_name, "_contact_data.xlsx"))
  
  # If the file exists, append the data
  if (file.exists(file_path)) {
    # Load the existing workbook
    wb <- loadWorkbook(file_path)
    
    # Read the existing data from the first sheet (assumed)
    existing_data <- read.xlsx(file_path, sheet = 1)
    
    # Combine existing data with the new data
    combined_data <- bind_rows(existing_data, data)
    
    # Clear the sheet and write combined data
    removeWorksheet(wb, 1)  # Remove the old sheet
    addWorksheet(wb, "Sheet1")  # Add a new sheet
    writeData(wb, sheet = "Sheet1", combined_data)  # Write combined data
    
    # Save the workbook
    saveWorkbook(wb, file_path, overwrite = TRUE)
    
  } else {
    # If the file doesn't exist, create a new Excel file
    wb <- createWorkbook()
    addWorksheet(wb, "Sheet1")
    writeData(wb, sheet = "Sheet1", data)
    
    # Save the workbook
    saveWorkbook(wb, file_path)
  }
}


# Iterate over the list of data frames and save them
purrr::walk(contact_data_by_fo, ~ {
  fo_name <- unique(.$Responsible_FO)  # Extract the Responsible_FO name
  write_or_append_excel(., fo_name, output_dir = getwd())  # Call the function to write or append to Excel
})



#### Write Geo spatial checks

source("03_Geo_Checks.R") 

### add KI data

# ImpactFunctions::update_ki_database(
#   data = df,
#   date = "today",
#   district = "district_name",
#   region = "localisation_region_label",
#   idp_site = "IDP_Site",
#   ki_name = "ki_name",
#   ki_age = "ki_age",
#   ki_role = "ki_role",
#   ki_status = "ki_status",
#   ki_contact = "ki_contact",
#   FieldOfficer = "Responsible_FO",
#   r_c = "DSA_08"
# )

## script close graphic

#source("05_script_close_graphic.R")
                                                                               
                                                                                                  

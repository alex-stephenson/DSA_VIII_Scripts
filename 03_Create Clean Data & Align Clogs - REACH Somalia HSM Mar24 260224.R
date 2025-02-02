# REACH Somalia HSM Dec 2024 - Clean data combination and check Script

rm(list = ls())
# library(cleaningtools)
library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)
library(cleaninginspectoR)
library(plyr)
library(purrr)

# source("../../../../IMPACTFunctions_GIT/ImpactFunctions/R/clog_check.R")

"%!in%" <- Negate("%in%")
date_time_now <- format(Sys.time(), "%b_%d_%Y_%H%M%S")

# import the kobo tool
kobo_tool_name <- "input/01_Tool/SOM_2024_DSA_R8_Deployed_tool.xlsx"

# importing survey as we only need to run these checks for select_multiple questions
kobo_survey <- read_excel(kobo_tool_name, sheet = "survey")
kobo_choice <- read_excel(kobo_tool_name, sheet = "choices")


######################### Read in the raw datasets and clogs ##########################
master_clog_path <- "input/SOM2204_DSA_Vlll_2025_Data_cleaning_logbook.xlsx"
  
deletion_log <- read_excel(master_clog_path, sheet = "03_deletion log") |> 
  select(1) |> 
  distinct() |> 
  unlist() |> 
  unname()

clogs_path <- "input/combined_clogs_cleaned_final.xlsx"

clogs <- read_excel(clogs_path, sheet = 2)

deletion_from_clogs <- clogs %>% 
  filter(change_type == "remove_survey")

clogs <- clogs |> 
  filter(uuid %!in% deletion_log) |>
  filter(change_type == "change_response") |> 
  dplyr::mutate(
    unique_key = str_c(uuid, question, old_value, sep = "_"),
  ) |> dplyr::arrange(unique_key) #|> 
  # select(-c(file_path, comment, ki_phone_number, ki_name, district, settlement))

# clogss <- clogs |> 
#   dplyr::mutate(
#     is.duplicate = unique_key == lag(unique_key)
#   ) |> filter(!is.duplicate) |> 
#   select(-c(unique_key, is.duplicate))

raw_dataset <- read_excel("input/DSA_2024_REACH_SOM_-_all_versions_-_False_-_2025-01-08-12-00-36.xlsx") |> 
  filter(`_uuid`%!in% deletion_log) #|> 
  # select(-all_of(remove_extra_col))

# clean_dataset <- read_excel("input/data/clean_data/DSA_R8_Dec_2024_clean_data_with_clogs_Jan_14_2025_112740.xlsx")

#######################################################################################
############################## Review the cleaning logs ###############################
#######################################################################################
review_clog <- cleaningtools::review_cleaning_log(raw_dataset = raw_dataset,
                                   raw_data_uuid_column = "_uuid",
                                   cleaning_log = clogs,
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type",
                                   change_response_value = "change_response"
                                   )

#######################################################################################
############################ Create clean data with clogs #############################
#######################################################################################

clean_data <- cleaningtools::create_clean_data(raw_dataset,
                                raw_data_uuid_column = "_uuid",
                                clogs,
                                cleaning_log_uuid_column = "uuid",
                                cleaning_log_question_column = "question",
                                cleaning_log_new_value_column = "new_value",
                                cleaning_log_change_type_column = "change_type",
                                change_response_value = "change_response",
                                NA_response_value = "blank_response",
                                no_change_value = "no_action",
                                remove_survey_value = "remove_survey"
                                ) 

#######################################################################################################
######################## Check for discrepancies between clog and clean data ##########################
#######################################################################################################
review_cleaning <- cleaningtools::review_cleaning(raw_dataset,
                                   raw_dataset_uuid_column = "_uuid",
                                   clean_data,
                                   clean_dataset_uuid_column = "_uuid",
                                   cleaning_log = clogs,
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_change_type_column = "change_type",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_old_value_column = "old_value",
                                   cleaning_log_added_survey_value = "added_survey",
                                   cleaning_log_no_change_value = c("no_action", "no_change"),
                                   deletion_log = deletion_from_clogs,
                                   deletion_log_uuid_column = "uuid",
                                   check_for_deletion_log = T
                                   )

#######################################################################################
############################## re-create parent column ################################
#######################################################################################   there is a bug in the function, it removes the choices name if it has combination as question name
parent_col_log <- cleaningtools::recreate_parent_column(clean_data,
                                                        uuid_column = "_uuid",
                                                        kobo_survey = kobo_survey,
                                                        kobo_choices = kobo_choice,
                                                        sm_separator = ".",
                                                        cleaning_log_to_append = NULL)

parent_log <- parent_col_log[[2]]

####################################################################################
########################### add extra calculation column  ##########################
####################################################################################
backup <- clean_data

clean_data <- clean_data %>%
  dplyr::mutate(
    cccm_populationestimates_twentypercent = round((cccm_populationestimates_individuals * 20 / 100), digits = 0),
    
    cccm_3month_arrive_minus_depart = cccm_idps_arrived - cccm_idps_departed,
    
    duration_site_established_in_months = case_when(
      time_site_established == "years" ~ duration_site_established * 12,
      .default = duration_site_established
    ),
    
    nfi_access_dist_min_int_male = case_when(
      nfi_access_distance_min_male == 'less_15' ~ 0,
      nfi_access_distance_min_male == '1530' ~ 15,
      nfi_access_distance_min_male == '3160' ~ 31,
      nfi_access_distance_min_male == 'more_60' ~ 60,
      TRUE ~ NA_real_
    ),
    nfi_access_dist_min_int_female = case_when(
      nfi_access_distance_min_fem == 'less_15' ~ 0,
      nfi_access_distance_min_fem == '1530' ~ 15,
      nfi_access_distance_min_fem == '3160' ~ 31,
      nfi_access_distance_min_fem == 'more_60' ~ 60,
      TRUE ~ NA_real_
    ),
    
    total_water_prop = rowSums(select(.,c(girls_collect_water, boys_collect_water, men_collect_water, women_collect_water)), na.rm =T),
    
    sanitation_toilets_total = rowSums(select(.,c(sanitation_toilets_male, sanitation_toilets_female, sanitation_toilets_nongendered)), na.rm=T)
  )


####################################################################################
########################### Export the entire workbook #############################
####################################################################################

# combine everything into a nice file
dsa_round_8_2025 <-list("Clean_Data" = clean_data,
                      "Cleaning_Log" = clogs)

# write the data
output_file_name <- "SOM2204_DSA_Vlll_2025_Clean_Data_with_Clogs_"
output_location <- "input/data/clean_data/"

write.xlsx(dsa_round_8_2025, paste0(output_location, output_file_name, date_time_now, ".xlsx"))


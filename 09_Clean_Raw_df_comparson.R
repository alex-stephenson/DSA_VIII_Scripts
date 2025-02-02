library(tidyverse)
library(readxl)
library(openxlsx)

rawDF <- read_excel("../01_DSA_III_Download_Data/raw_data/DSA_2024_REACH_SOM_-_all_versions_-_False_-_2025-01-08-12-00-36.xlsx")
cleanedDF <- read_excel("input/data/clean_data/clean_df_update_night.xlsx") 

cleaning_log_path <- "input/SOM2204_DSA_Vlll_2024_Data_cleaning_logbook.xlsx"

deletions <- read_excel(cleaning_log_path, sheet = "03_deletion log")

extra_columns <- readxl::read_xlsx(cleaning_log_path, sheet = "00_variable_tracker")
extra_columns <- extra_columns[extra_columns[,2] == "Removed", ]

columns_to_exclude <- extra_columns[[1]] 


names(rawDF) <- gsub("/", ".", names(rawDF))
names(cleanedDF) <- sub("/", ".", names(cleanedDF))

rawDF <- rawDF %>% 
  filter(!duplicated(`_uuid`)) %>% 
  filter(intro_consent == "yes") |> 
  type_convert()


rawDF <- rawDF %>% filter(!`_uuid` %in% deletions$uuid)
rawDF <- rawDF |> select(-all_of(columns_to_exclude)) 


rawDF <- rawDF %>% select(-c(start, end, today, deviceid, `_submission_time`, `_index`, ends_with("_other"), any_comments ))

cleanedDF <- cleanedDF %>% select(-c(start, end, today, deviceid,`_submission_time`, `_index`, ends_with("_other"), any_comments))
cleanedDF <- cleanedDF %>% filter(!`_uuid` %in% deletions$uuid) |> 
  type_convert()


raw_col <- as.data.frame(names(rawDF))
cln_col <- as.data.frame(names(cleanedDF))

col <- cbind(raw_col,cln_col) |> 
  mutate(
    match = case_when(
      `names(rawDF)` == `names(cleanedDF)` ~ "matched",
      TRUE ~ "not"
    )
  )

check <- col |> filter(match == "not")
# jnt <- cln_col |> anti_join(raw_col)


generate_log <- function(rawDF, cleanedDF){
  # Create empty vectors
  question.name <- vector()
  old_value <- vector()
  new_value <- vector()
  uuid <- vector()
  
  
  for (j in 1:length(rawDF)) {
    for (rowi in 1:nrow(rawDF)){
      
      value_raw <- rawDF[rowi, j]
      value_clean <- cleanedDF[rowi, j]
      
      # Split the values into individual elements and sort them
      value_raw_split <- sort(unlist(strsplit(as.character(value_raw), " ")))
      value_clean_split <- sort(unlist(strsplit(as.character(value_clean), " ")))
      
      # Convert to character vectors
      value_raw_sorted <- paste(value_raw_split, collapse = " ")
      value_clean_sorted <- paste(value_clean_split, collapse = " ")
      
      # Check if the sorted values are equal
      temp <- value_raw_sorted == value_clean_sorted
      
      # Condition
      if (!isTRUE(temp)){
        value_raw <- rawDF[rowi, j]
        value_clean <- cleanedDF[rowi, j]
        
        # Append values to vectors
        question.name <- c(question.name, names(rawDF[j]))
        old_value <- c(old_value, as.character(value_raw))
        new_value <- c(new_value, as.character(value_clean))
        uuid <- c(uuid, as.character(rawDF[rowi,"_uuid"]))
      }
      
    }
    # Progress
    cat("\014")
    # print(paste("Checking Column", j, "of", length(rawDF)))
    print(paste("Checking Column", j, "of", length(rawDF), "----------", names(rawDF)[j]))
  }
  
  log_df <- data.frame(question.name, old_value, new_value, uuid)
  return(log_df)
}

rawDF <- rawDF %>% arrange(by="_uuid")
cleanedDF <- cleanedDF %>% arrange(by="_uuid")

changed_values <- generate_log(rawDF, cleanedDF)

write.xlsx(changed_values, "output/changed_values_detected_by_script_update_night.xlsx")



# cleaning_logs <- read_excel("../../08_SOM2204_DSA_Vlll_KI_2024_Data_for_Validatation/03_cleaning_log/SOM2204_DSA_Vlll_2024_Data_cleaning_logbook.xlsx", sheet = "03_deletion log")
# cleaning_logs <- cleaning_logs %>% filter(changed=="yes")
# cleaning_logs <- cleaning_logs %>% filter(!uuid %in% deletions$`_uuid`)
# cleaning_logs <- cleaning_logs %>% filter(!uuid %in% extra_deletions$uuid)
# # cleaning_logs$question.name <- gsub(".", "/", cleaning_logs$question.name)
# # difference <- anti_join(cleaning_logs, changed_values, by=join_by(uuid, "question.name"))
# cleaning_logs$key <- paste0(cleaning_logs$uuid, cleaning_logs$question.name)
# changed_values$key <- paste0(changed_values$uuid, changed_values$question.name)
# diff_clean <- anti_join(cleaning_logs, changed_values, by=join_by(key))
# diff_changed <- anti_join(changed_values, cleaning_logs, by =join_by(key))
# 
# changed_values2 <- left_join(changed_values, cleaning_logs, by="key")
# changed3 <- left_join(cleaning_logs, changed_values, by="key")
# 
# write.xlsx(changed_values2, "output/changed_values_detected_by_script_columns_added.xlsx")


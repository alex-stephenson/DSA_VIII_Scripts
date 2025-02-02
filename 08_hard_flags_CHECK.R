#### change logical error
library(tidyverse)
library(readr)
library(readxl)

df <- read_xlsx("../01_DSA_III_Download_Data/raw_data/DSA_2024_REACH_SOM_-_all_versions_-_False_-_2025-01-08-12-00-36.xlsx")

cleaning_log_path <- "input/SOM2204_DSA_Vlll_2024_Data_cleaning_logbook.xlsx"

deletion_log <- read_excel(cleaning_log_path, sheet = "03_deletion log")

exclude_col <- read_excel(cleaning_log_path, sheet = "00_variable_tracker")

clean_df <- df |> 
  filter(!`_uuid` %in% deletion_log$uuid) |> 
  type_convert()

names(clean_df) <- gsub("/",".", names(clean_df))
  
clean_df <- clean_df |> 
  mutate(
    # CHECK_nfi_items
    # CHECK_nfi_items_none
    none = case_when(
      # IF another NFI question has 'all', then none should be changed to 'none'. IF none of the NFI item questions have 'all' 
      # (ie they are all few, some or many) then none should be changed to few.
      none %in% c("all", "dnk") & ((!blanket %in% c("none", "dnk")) | (!sleeping_mat %in% c("none", "dnk")) | 
                                     (!plastic_sheet %in% c("none", "dnk")) | (!kitchen_utensils %in% c("none", "dnk")) |
                                     (!solar_lamp %in% c("none", "dnk")) | (!jerrican %in% c("none", "dnk")) | 
                                     (!proporting_secure_lock %in% c("none", "dnk"))) ~ "few",
      # IF all NFI questions are 'none', then 'none' needs changing to 'all'
      none == "none" & blanket == "none" & sleeping_mat == "none" & plastic_sheet ==  "none" & kitchen_utensils == "none" & 
        solar_lamp == "none" & jerrican == "none" & proporting_secure_lock == "none" ~ "all",
      TRUE ~ none
    ),
    
    # CHECK_water_sources_primary
    water_sources_primary = case_when(
      # water_sources_primary should be nullified
      water_sources_present == 0 & (!is.na(water_sources_primary)) ~ NA_character_,
      TRUE ~ water_sources_primary
    ),
    
    water_sources_domestic = case_when(
      # 0 water sources reported present in the site but the option "Water trucking distribution point" wasn't selected in the previous question
      water_sources_present == 0 & water_sources_domestic != "water_trucking_distrib" ~ "water_trucking_distrib",
      TRUE ~ water_sources_domestic
    ),
    
    water_treatment_proportion = case_when(
      # When the KI has only selected water_treatment_methods/do_not_treat == 1 and all other awater_treatment_methods == 0, then water_treatment_proportion should change to none
      water_treatment_proportion %in% c("all", "few", "many", "some") & water_treatment_methods.do_not_treat == 1 ~ "none",
      TRUE ~ water_treatment_proportion
    ),
    
    # When water_treatment_proportion == "none", water_treatment_method.do_not_treat should be turned to 1 and all other water_treatment_methods should be 0
    water_treatment_methods = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ "do_not_treat",
      TRUE ~ water_treatment_methods
    ),
    
    water_treatment_methods.boiling = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ 0,
      TRUE ~ water_treatment_methods.boiling
    ),
    
    water_treatment_methods.cloth_filter = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ 0,
      TRUE ~ water_treatment_methods.cloth_filter
    ),
    
    water_treatment_methods.other_filter = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ 0,
      TRUE ~ water_treatment_methods.other_filter
    ),
    
    water_treatment_methods.aquatabs = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ 0,
      TRUE ~ water_treatment_methods.aquatabs
    ),
    
    water_treatment_methods.dnk = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ 0,
      TRUE ~ water_treatment_methods.dnk
    ),
    
    water_treatment_methods.other = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ 0,
      TRUE ~ water_treatment_methods.other
    ),
    
    water_treatment_methods_other = case_when(
      water_treatment_proportion %in% c("none", "dnk") & water_treatment_methods.do_not_treat == 0 ~ NA_character_,
      TRUE ~ water_treatment_methods_other
    ),
    
    water_treatment_methods.do_not_treat = case_when(
      water_treatment_proportion %in% c("none", "dnk") ~ 1,
      TRUE ~ water_treatment_methods.do_not_treat
    ),
    
    # if total_water_prop != 100 then X_collect_water should all be nullified and water_collection_check changed to "no"
    men_collect_water = case_when(
      total_water_prop != 100 ~ NA_real_,
      TRUE ~ men_collect_water
    ),
    
    women_collect_water = case_when(
      total_water_prop != 100 ~ NA_real_,
      TRUE ~ women_collect_water
    ),
    
    boys_collect_water = case_when(
      total_water_prop != 100 ~ NA_real_,
      TRUE ~ boys_collect_water
    ),
    
    girls_collect_water = case_when(
      total_water_prop != 100 ~ NA_real_,
      TRUE ~ girls_collect_water
    ),
    
    water_collection_check = case_when(
      total_water_prop != 100 ~ "no",
      TRUE ~ water_collection_check
    ),
    
    water_access_time_men = case_when(
      men_collect_water > 0 ~ water_access_time_men,
      TRUE ~ NA_character_
    ),
    
    water_access_time_women = case_when(
      women_collect_water > 0 ~ water_access_time_women,
      TRUE ~ NA_character_
    ),
    
    water_access_time_boys = case_when(
      boys_collect_water > 0 ~ water_access_time_boys,
      TRUE ~ NA_character_
    ),
    
    water_access_time_girls = case_when(
      girls_collect_water > 0 ~ water_access_time_girls,
      TRUE ~ NA_character_
    ),
    
    # total_number_bath_sanitation = rowSums(select(.,c(bathing_male, bathing_female, bathing_non_gendered))),
    # 
    # hygiene_handwashingfacilities = case_when(
    #   hygiene_handwashingfacilities %in% c("all", "many") & total_number_bath_sanitation == 0 ~ "few",
    #   hygiene_handwashingfacilities == "none" & total_number_bath_sanitation >= 100 ~ "many",
    #   hygiene_handwashingfacilities == "none" & total_number_bath_sanitation >= 50 ~ "some",
    #   hygiene_handwashingfacilities == "none" & total_number_bath_sanitation >= 10 ~ "few",
    #   TRUE ~ hygiene_handwashingfacilities
    # ),
    
    male_sickness = case_when(
      male_health_problems.no_health_issues == 1 ~ "none",
      TRUE ~ male_sickness
    ),
    
    male_wounds = case_when(
      male_health_problems.no_health_issues == 1 ~ "none",
      TRUE ~ male_wounds
    ),
    
    male_mental_health = case_when(
      male_health_problems.no_health_issues == 1 ~ "none",
      TRUE ~ male_mental_health
    ),
    
    # IF the KI lists any specific health issues (ie ANY of male_health_problems.cholera, or male_health_problems.resp_problems = 1, 
    # then male_health_problems.no_health_issues MUST be 0, and male_sickness,male_wounds,male_mental_health should be hard coded as DNK
    male_sickness = case_when(
      (male_sickness == "none" & male_wounds == "none" & male_mental_health == "none") & 
        (male_health_problems.awd_cholera == 1 | male_health_problems.resp_problems == 1) ~ "dnk",
      TRUE ~ male_sickness
    ),
    
    male_wounds = case_when(
      (male_sickness == "none" & male_wounds == "none" & male_mental_health == "none") & 
        (male_health_problems.awd_cholera == 1 | male_health_problems.resp_problems == 1) ~ "dnk",
      TRUE ~ male_wounds
    ),
    
    male_mental_health = case_when(
      (male_sickness == "none" & male_wounds == "none" & male_mental_health == "none") & 
        (male_health_problems.awd_cholera == 1 | male_health_problems.resp_problems == 1) ~ "dnk",
     TRUE ~ male_mental_health
    ),
    
    female_sickness = case_when(
      female_health_problems.no_health_issues == 1 ~ "none",
      TRUE ~ female_sickness
    ),
    
    female_wounds = case_when(
      female_health_problems.no_health_issues == 1 ~ "none",
      TRUE ~ female_wounds
    ),
    
    female_mental_health = case_when(
      female_health_problems.no_health_issues == 1 ~ "none",
      TRUE ~ female_mental_health
    ),
    
    # IF the KI lists any specific health issues (ie ANY of female_health_problems.cholera, or female_health_problems/resp_problems = 1, 
    # then female_health_problems.no_health_issues MUST be 0, and female_sickness,female_wounds,female_mental_health should be hard coded as DNK
    female_sickness = case_when(
      (female_sickness == "none" & female_wounds == "none" & female_mental_health == "none") & 
        (female_health_problems.awd_cholera == 1 | female_health_problems.resp_problems == 1) ~ "dnk",
      TRUE ~ female_sickness
    ),
    
    female_wounds = case_when(
      (female_sickness == "none" & female_wounds == "none" & female_mental_health == "none") & 
        (female_health_problems.awd_cholera == 1 | female_health_problems.resp_problems == 1) ~ "dnk",
      TRUE ~ female_wounds
    ),
    
    female_mental_health = case_when(
      (female_sickness == "none" & female_wounds == "none" & female_mental_health == "none") & 
        (female_health_problems.awd_cholera == 1 | female_health_problems.resp_problems == 1) ~ "dnk",
      TRUE ~ female_mental_health
    ),
    
    # foodsecurity_access = yes and foodsecurity_access_distance_min = more_60 or 31_60
    foodsecurity_access_distance_min = case_when(
      # IF foodsecurity_access == "yes" THEN foodsecurity_access_distance_min needs changing to 15_30
      foodsecurity_access == "yes" & (foodsecurity_access_distance_min %in% c("more_60", "3160")) ~ "1530",
      TRUE ~ foodsecurity_access_distance_min
    ),
    
    foodsecurity_access = case_when(
      foodsecurity_access == "no" & foodsecurity_access_distance_min == "less_15" ~ "yes",
      TRUE ~ foodsecurity_access
    )
  )

clean_df <- clean_df %>% mutate(
  total_number_bath_sanitation = rowSums(select(.,c(bathing_male, bathing_female, bathing_non_gendered))),

  hygiene_handwashingfacilities = case_when(
    hygiene_handwashingfacilities %in% c("all", "many") & total_number_bath_sanitation == 0 ~ "few",
    hygiene_handwashingfacilities == "none" & total_number_bath_sanitation >= 100 ~ "many",
    hygiene_handwashingfacilities == "none" & total_number_bath_sanitation >= 50 ~ "some",
    hygiene_handwashingfacilities == "none" & total_number_bath_sanitation >= 10 ~ "few",
    TRUE ~ hygiene_handwashingfacilities
  ),
  duration_site_established_in_months = case_when(
    time_site_established == "years" ~ duration_site_established*12,
    TRUE ~ duration_site_established
  )
) |> 
  select(-total_number_bath_sanitation)

#### missing values in different version of the tool
diff_var <- readxl::read_xlsx("input/differences_versions.xlsx", sheet = "multiple_select") #|> 
  # select(parent_question) |> 
  # distinct() |> 
  # unname() |> 
  # unlist()

diff_var <- unique(diff_var[["parent_question"]])

# replace missing value of multiple choice question with 0
for (col in diff_var) {  
 
  choice_columns <- grep(paste0(col, "."), names(clean_df), fixed = TRUE, value = TRUE)  
 
    for (i in 1:nrow(clean_df)) {  
 
      if (!is.na(clean_df[[col]][i])) {  

        for (choice in choice_columns) {  
          if (is.na(clean_df[[choice]][i])) {  
            clean_df[[choice]][i] <- 0  
        }  
      }  
    }  
  }  
}  


############################################# Replace 999 of int cols to NA ####
clean_df[] <- lapply(clean_df, function(col) {
  # if (is.character(col) || is.factor(col)) {
  #   col[col == "Yes"] <- "yes"
  # }
  
  if (is.numeric(col)) {
    col[col == 999] <- NA_real_
    
  }
  return(col)
})

################################# normalize the shelter type percentage that do not end to 100 % ####################################
# Shelter type normalize function  
normalize_shelter_perc <- function(shelter_type_perc) {  
  shelter_sum <- sum(shelter_type_perc, na.rm = TRUE)  
  if (shelter_sum > 0 && shelter_sum != 100) {  
    normalized <- (shelter_type_perc / shelter_sum) * 100  
    rounded <- round(normalized, digits = 0)  
  
    discrepancy <- sum(rounded) - 100  
    if (discrepancy != 0) {  
 
      index_to_adjust <- which.max(rounded)  
      rounded[index_to_adjust] <- rounded[index_to_adjust] - discrepancy  
    }  
    return(rounded)  
  }   
  return(shelter_type_perc)  
} 

shelter_columns <- c("sleeping_open", "make_shift", "Tent", "solid_apartment", "unfinished", "buul")

if (!all(unlist(shelter_columns) %in% colnames(clean_df))) {  
  stop("at least one column name might be incorect")  
}

clean_df[shelter_columns] <- t(apply(clean_df[shelter_columns], 1, normalize_shelter_perc)) 

clean_df <- clean_df %>% mutate(
  shelter_sum = rowSums(select(.,c("sleeping_open", "make_shift", "Tent", "solid_apartment", "unfinished", "buul")))
)
########################################################### Skip logic ######### 
SL_depend<-read.csv("input/Skip Logic/DSA_SL_DSA8_updated.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," ")) %>% 
  filter(Include_DSA_8 == "yes") %>%
  select(3:ncol(.))

#import with blanks being NA's
inversed_critera <- SL_depend %>% pull(1)
SL_depend <- SL_depend %>% select(-1)
SL_depend <- SL_depend[!is.na(SL_depend[,1]),]  

for (i in 1:dim(SL_depend)[1]){
  
  dependent_questions_rgx <- sprintf("(%s)",map_chr(SL_depend[i,3:dim(SL_depend)[2]] %>%
                                                      t() %>% as.character() %>%
                                                      .[!is.na(.)], ~sprintf("^%s\\.|^%s$",.x,.x)) 
                                     %>% paste(collapse = "|"))
  
  index_SL_depend <- which(grepl(dependent_questions_rgx,names(clean_df)))
  
  if(inversed_critera[i]){
    index_SL_rows<- which(as.character(clean_df[[SL_depend[i,1]]]) == as.character(SL_depend[i,2]) )
  } else {
    index_SL_rows<- which(as.character(clean_df[[SL_depend[i,1]]]) != as.character(SL_depend[i,2]) )
  }
  clean_df[index_SL_rows,index_SL_depend] <- NA
}

##################### Remove extra column from the raw and clean data set

extra_columns <- readxl::read_xlsx(cleaning_log_path, sheet = "00_variable_tracker")
extra_columns <- extra_columns[extra_columns[,2] == "Removed", ]

columns_to_exclude <- extra_columns[[1]] 

clean_df <- clean_df |> select(-all_of(columns_to_exclude)) 

openxlsx::write.xlsx(clean_df,"input/data/clean_data/clean_df_update_night.xlsx")


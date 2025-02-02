rm(list = ls())
library(tidyverse)
library(openxlsx)
library(readxl)

# deleted interviews to exclude from the data
deletion_log <- read_excel("input/SOM2204_DSA_Vlll_2024_Data_cleaning_logbook.xlsx", sheet = "03_deletion log")[1]

raw_df <- read_excel("../01_DSA_III_Download_Data/raw_data/DSA_2024_REACH_SOM_-_all_versions_-_False_-_2025-01-06-06-06-05.xlsx") |> 
  filter(!`_uuid` %in% deletion_log[[1]])

# cleaning log to extract only the shelter type rows
cleaning_log <- read_excel("Shiny/DSA_VIII_Monitoring/REACH_SOM_DSA_VIII_Field_Dashboard/data/combined_clogs.xlsx")
cleaning_log_shelter_type <- cleaning_log |> select(uuid, question, change_type, old_value, new_value) |> 
  filter(change_type == "change_response") |> 
  filter(!uuid %in% deletion_log[[1]]) |> 
  filter(question %in% c("sleeping_open", "make_shift", "Tent", "solid_apartment", "other_sleeping", "unfinished", "buul")) |> 
  type.convert()

# cleaned valued of shelter type in cleaning log that end up to 100 percent
aggregate_shlt_perc <- cleaning_log_shelter_type |> 
  group_by(uuid) |> 
  summarize(
    total_perc_shlt_type = sum(new_value, na.rm = T)
  ) |> 
  filter(total_perc_shlt_type == 100)

# extracted dataset to use for shelter type percentage fix
df <- raw_df |> 
  select(c("sleeping_open", "make_shift", "Tent", "solid_apartment", "other_sleeping", 
                         "unfinished", "buul", "shelter_sum", "_uuid")) |> 
  filter(shelter_sum != 100 & shelter_sum != 0) |> type.convert()

# re-calculate the total percentage of all shelter type
df <- df %>%
  mutate(
    shelter_sum = rowSums(select(.,c(sleeping_open, make_shift, Tent, solid_apartment, other_sleeping, 
                                     unfinished, buul)), na.rm = T)
  )


# Shelter type normalize function  
normalize_shelter_perc <- function(shelter_type_perc) {  
  shelter_sum <- sum(shelter_type_perc)  
  if (shelter_sum > 0 && shelter_sum != 100) {  
    return(round(((shelter_type_perc / shelter_sum) * 100), digits = 0))
  }   
  return(shelter_type_perc)  
} 


shelter_columns <- names(df)[names(df) %in% c("sleeping_open", "make_shift", "Tent", "solid_apartment", "other_sleeping", "unfinished", "buul")]  
df[shelter_columns] <- t(apply(df[shelter_columns], 1, normalize_shelter_perc)) 





# for jon to bind extra cols
outliers <- read_xlsx("output/outliers/SOM_2024_DSA_Vlll_outlier_data_overall_2025-01-06.xlsx")

col_join <- c("cccm_populationestimates_individuals", "shelter_estimations","cccm_populationestimates_families", "number_of_boys_5", "number_of_boys_18", 
              'number_of_men', 'number_of_male_elders', 'number_of_girls_5', 'number_of_girls_18', "number_of_women", "number_of_women_elders",
              'total_number', 'men_collect_water', 'women_collect_water', 'boys_collect_water', "girls_collect_water", "total_water_prop","sanitation_toilets_male",
              'sanitation_toilets_female', "sanitation_toilets_nongendered", "sanitation_toilets_total", 
              'cccm_idps_departed', 'cccm_idps_arrived', 'primary_schools', "number_schools_opened", "_uuid")

if (!all(col_join %in% colnames(raw_df))) {  
  stop("some column name might be incorect")  
}

jon_col <- raw_df |> 
  select(all_of(col_join))

outliers <- outliers |> 
  left_join(jon_col, by = c("uuid" = "_uuid"))

write.xlsx(outliers, "output/outliers/SOM_2024_DSA_Vlll_outlier_data_overall_2025-01-07.xlsx")

##################### align the individual number based on the number of families - considering the hh avg size 5.4 based on MSNA data

rm(list=ls())
library(tidyverse)
library(readxl)
library(openxlsx)

# clean_df <- read_xlsx("input/data/clean_data/SOM2204_DSA_Vlll_2025_Clean_Data_with_Clogs_Jan_16_2025_160320.xlsx", guess_max = 10000)
# raw_df <- read_xlsx("input/DSA_2024_REACH_SOM_-_all_versions_-_False_-_2025-01-08-12-00-36.xlsx", guess_max = 15000)
imputed_df <- read_xlsx("input/df_subset_population.xlsx", guess_max = 10000)

imputed_df <- imputed_df |> 
  dplyr::mutate(
    pop_ind_prop = cccm_populationestimates_individuals / cccm_populationestimates_families,
    
    cccm_populationestimates_individuals_calc = case_when(
      pop_ind_prop < 3 | pop_ind_prop > 8 ~ round(cccm_populationestimates_families * 5.4, digits = 0),
      .default = cccm_populationestimates_individuals
    )#,.keep = "used"
  )


normalize_individuals <- function(df, individual_group_cols, total_column) {  
  df %>%  
    rowwise() %>%  
    mutate(  
      total_individual_group = sum(c_across(all_of(individual_group_cols)), na.rm = TRUE),  
      adjustment_factor = ifelse(total_individual_group != !!sym(total_column),  
                                 !!sym(total_column) / total_individual_group,  
                                 1),
      across(all_of(individual_group_cols), ~ .x * adjustment_factor)
    ) %>%  
    ungroup() %>%  
    select(-total_individual_group, -adjustment_factor) 
}  

individual_group_cols <- c("number_of_boys_5", "number_of_boys_18", "number_of_men",   
                           "number_of_male_elders", "number_of_girls_5",   
                           "number_of_girls_18", "number_of_women", "number_of_women_elders")  

if (!all(unlist(individual_group_cols) %in% colnames(imputed_df))) {  
  stop("at least one column name might be incorect")  
}

total_column <- "cccm_populationestimates_individuals_calc"  

imputed_df <- normalize_individuals(imputed_df, individual_group_cols, total_column) 

imputed_df[individual_group_cols] <- sapply(imputed_df[individual_group_cols], as.integer)


write.xlsx(imputed_df, "input/df_subset_population_fixed.xlsx")


# rawDF <- read_excel("input/df_subset_population.xlsx")
# cleanedDF <- read_excel("input/df_subset_population_fixed.xlsx")

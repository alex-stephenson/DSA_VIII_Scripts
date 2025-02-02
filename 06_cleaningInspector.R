library("cleaninginspectoR")
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate)

all_data <- df |> 
  filter(validity == "Okay")

inspection <- find_outliers(all_data)
# inspect_all_log <- inspect_all(all_data,uuid.column.name = "uuid")

for(i in 1:nrow(inspection)){
  uuid <- all_data[[inspection[[i,"index"]],"uuid"]]
  inspection[i, "uuid"] <- uuid
}

# for(i in 1:nrow(inspect_all_log)){
#   uuid <- all_data[[inspect_all_log[[i,"index"]],"uuid"]]
#   inspect_all_log[i, "uuid"] <- uuid
# }

inspection <- inspection %>% 
  filter(!variable %in% c("ki_contact", "shelter_sum", "shelter_estimations" ,"cccm_populationestimates_twentypercent", "referral_phone", "_geopoint_altitude", 
                          "_geopoint_latitude", "_geopoint_longitude", "_geopoint_precision","pt_sample_lon", "cccm_3month_arrive_minus_depart",
                          "pt_sample_lat", "distance_to_site", "interview_duration_start_end","interview_duration", "total_number",
                          "total_water_prop", "sanitation_toilets_total", "_id"))
inspection <- inspection |> 
  filter(value != "999")

df_detail <- df |> 
  select(today, enum_name, localisation_region_label, idp_site, idp_code, district_name, Responsible_FO, uuid)
  
inspection <- inspection |> 
  left_join(df_detail)


session <- "SOM_2024_DSA_Vlll_"

write.xlsx(inspection, paste0("output/outliers/", session, "outlier_data_overall_", today(),".xlsx"), overwrite = T)
write.xlsx(all_data, paste0("output/outliers/", session, "all_valid_data_", today(),".xlsx"))
           
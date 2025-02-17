### this file is designed to be called from 01_Data Cleaning_DSA8.R

# 
# gis_data <- df %>% 
#   filter(interview_duration>=30 & interview_duration <= 90 & consent=="yes") %>% 
#   select(uuid,start,end,audit,today,enum_name,localisation_region_label,
#          ki_contact,district_name,idp_code,idp_site,contains("gps"),
#          pt_sample_lat, pt_sample_lon, distance_to_site, any_comments) %>%
#   left_join((field_officer_location %>% select(`CCCM IDP Site Code`, 
#                                                Responsible_FO)),
#             , by = join_by("idp_code" == `CCCM IDP Site Code`))
# 
# 
# write.xlsx(gis_data,paste("gis/spatial_checks",today,".xlsx"))


gis_data <- recent_df %>% 
  # filter(interview_duration>=30 & interview_duration <= 90 & consent=="yes") %>% 
  filter(consent=="yes") %>% 
  select(uuid,start,end,audit,today,enum_name,localisation_region_label,
         ki_contact,district_name,idp_code,idp_site,  `_geopoint_latitude`,
          `_geopoint_longitude`, `_geopoint_altitude`, `_geopoint_precision`, pt_sample_lat, pt_sample_lon, distance_to_site, 
         any_comments, reasons_why_far, Responsible_FO
         ) %>%
  left_join((field_officer_location %>% select(`CCCM IDP Site Code`, 
                                               Responsible_FO)),
            , by = join_by("idp_code" == `CCCM IDP Site Code`))


write.xlsx(gis_data,paste("gis/spatial_checks",today,".xlsx"))
write.xlsx(gis_data,paste("C:/Users/sulai/ACTED/IMPACT SOM - SOM2204 _DSA VIII 2025/03_Data_Analysis/DSA_VIII_Scripts/gis/For GIS Team/spatial_checks",today,".xlsx"))


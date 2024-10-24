### this file is designed to be called from 01_Data Cleaning_DSA8.R


gis_data <- df %>% 
  filter(interview_duration>=30 & interview_duration <= 90 & consent=="yes") %>% 
  select(uuid,start,end,audit,today,enum_name,localisation_region_label,
         ki_contact,district_name,idp_code,idp_site,contains("gps"),more_info) %>%
  left_join((field_officer_location %>% select(`CCCM IDP Site Code`, 
                                               Responsible_FO)),
            , by = join_by("idp_code" == `CCCM IDP Site Code`))


write.xlsx(gis_data,paste("gis/spatial_checks",today,".xlsx"))

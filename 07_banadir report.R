sites_for_field_haj <- read_excel("sites_for_field.xlsx") |> 
  # filter(Region == "Banadir") |> 
  select(Region,District,idp_code = CCCM_IDP_Site_Code, sample_size = `#_Surveys`)

report_banadir <- dashboard |> 
  # filter(localisation_region_label == "Banadir") |> 
  group_by(idp_code) |> 
  summarize(
    all_data = n(),
    accepted = sum(validity == "Okay", na.rm = T),
    deleted = sum(validity == "invalid", na.rm = T),
  )

regional_summary <- sites_for_field_haj |> 
  left_join(report_banadir)

regional_summary$all_data <- if_else(is.na(regional_summary$all_data), 0, regional_summary$all_data)
regional_summary$accepted <- if_else(is.na(regional_summary$accepted), 0, regional_summary$accepted)
regional_summary$deleted <- if_else(is.na(regional_summary$deleted), 0, regional_summary$deleted)


regional_summary <- regional_summary |> 
  select(Region,District,idp_code,sample_size,collected_data=all_data,accepted,deleted) |> 
  mutate(
    remaining = sample_size-accepted
  )

count_ki_roles_site_r <- df %>% 
  filter(validity == "Okay") |> 
  group_by(localisation_region_label,district_name,idp_code,ki_role) %>%
  dplyr::summarise(n=n())

write.xlsx(regional_summary, "Regional_data_collection_details.xlsx")
write.xlsx(count_ki_roles_site_r, "count_ki_roles_site.xlsx")

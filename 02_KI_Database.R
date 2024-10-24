### this file is designed to be called from 01_Data Cleaning_DSA8.R

current_KI_db <- read_xlsx(sprintf(r"(C:\Users\%s\ACTED\IMPACT SOM - 02_Research\01_REACH\Data Team\10_Common files\KI_Database\KI_Contact_List.xlsx)", user_login))

KI_db <- df %>%
  filter(future_rc_consent == "yes") %>%
  select(district_name,
         localisation_region_label,
         idp_site,
         starts_with("ki_"),
         Responsible_FO) %>%
  mutate(date = Sys.Date(),
         research = "DSA"
  )

appended_db <- current_KI_db %>%
  dplyr::bind_rows(KI_db) %>%
  dplyr::distinct()


# Create a new workbook
KI_wb <- createWorkbook()

# Add a worksheet
addWorksheet(KI_wb, "KI_database")

# Write the data frame to the worksheet
writeData(KI_wb, sheet = "KI_database", x = appended_db)

# Apply filters to the data
addFilter(KI_wb, sheet = "KI_database", row = 1, cols = 1:ncol(appended_db))

# Adjust the column widths to fit the content
setColWidths(KI_wb, sheet = "KI_database", cols = 1:ncol(appended_db), widths = "auto")

# Create a style for the column headers
headerStyle <- createStyle(
  fontSize = 12,
  fontColour = "#FFFFFF",   # White text color
  fgFill = "#4F81BD",       # Blue background color
  halign = "center",        # Center align the text
  textDecoration = "bold"   # Bold text
)

# Apply the header style to the first row
addStyle(KI_wb, sheet = "KI_database", style = headerStyle, rows = 1, cols = 1:ncol(appended_db), gridExpand = TRUE)

# Save the workbook to an Excel file
saveWorkbook(KI_wb, sprintf(r"(C:\Users\%s\ACTED\IMPACT SOM - 02_Research\01_REACH\Data Team\10_Common files\KI_Database\KI_Contact_List.xlsx)", user_login), overwrite = TRUE)
# Combining CDC WONDER Data
# By Hannah De los Santos
# Originated on: 6/3/21
# Last updated: 16/11/24 by Gih Ming

library(dplyr)

# Set folder paths
base_folder <- gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive")))
data_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Raw", "CDC_WONDER")
preprocessed_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Preprocessed")

# Filter to only 10 years and map filenames 
selected_files <- list.files(data_folder, pattern = "2011_2020")

# Define column mappings
column_mappings <- list(
  mortality = c(
    "cdc_wonder_alcohol_all_2011_2020_allpop.txt" = "alcoholmortality_pop",
    "cdc_wonder_alcohol_all_2011_2020_race.txt" = "alcoholmortality_black",
    "cdc_wonder_drug_poisonings_all_2011_2020_allpop.txt" = "drugpoisoningmortality_pop",
    "cdc_wonder_drug_poisonings_all_2011_2020_race.txt" = "drugpoisoningmortality_black",
    "cdc_wonder_suicide_all_2011_2020_allpop.txt" = "suicidemortality_pop",
    "cdc_wonder_suicide_all_2011_2020_race.txt" = "suicidemortality_black"
  ),
  population = c(
    "cdc_wonder_alcohol_all_2011_2020_allpop.txt" = "alcoholmortality_denom_pop",
    "cdc_wonder_alcohol_all_2011_2020_race.txt" = "alcoholmortality_denom_black",
    "cdc_wonder_drug_poisonings_all_2011_2020_allpop.txt" = "drugpoisoningmortality_denom_pop",
    "cdc_wonder_drug_poisonings_all_2011_2020_race.txt" = "drugpoisoningmortality_denom_black",
    "cdc_wonder_suicide_all_2011_2020_allpop.txt" = "suicidemortality_denom_pop",
    "cdc_wonder_suicide_all_2011_2020_race.txt" = "suicidemortality_denom_black"
  )
)

# Initialize the dataset with counties and county codes
initialize_dataframe <- function(file) {
  df <- read.delim(file.path(data_folder, file), colClasses = c("County.Code" = "character"))
  df <- df %>% filter(County != "") %>% select(County, County.Code)
  rownames(df) <- df$County.Code
  return(df)
}

full_df <- initialize_dataframe(selected_files[1])

# Process and combine data
for (file in selected_files) {
  df <- read.delim(file.path(data_folder, file), colClasses = c("County.Code" = "character")) %>%
    filter(County != "") %>%
    mutate(
      Deaths = as.numeric(replace(Deaths, Deaths == "Suppressed", 4.5)),
      Population = as.numeric(replace(Population, Population == "Missing", NA))
    )
  
  # Add mortality and population data to the main dataset
  full_df[df$County.Code, column_mappings$mortality[file]] <- df$Deaths
  full_df[df$County.Code, column_mappings$population[file]] <- df$Population
}

# Write combined dataset to CSV
write_csv(full_df, file.path(preprocessed_folder, "CDC_WONDER_County_mortality.csv"), na = "")

# Life Expectancy Race Stratification
# By Hannah De los Santos
# Originated on: 10/13/21

# Load required libraries
library(tidyverse)
library(usmap)
library(readr)
library(dplyr)
library(stringr)

# Helper function to dynamically construct file paths
construct_path <- function(base_folder, ...) {
  file.path(gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive"))), base_folder, ...)
}

# Function to map county names to FIPS codes
map_county_to_fips <- function(cty_df, state_col, county_col, county_fips) {
  cty_df %>%
    mutate(
      State_Lower = str_to_lower(!!sym(state_col)),
      County_Lower = str_replace_all(str_to_lower(!!sym(county_col)), c("st " = "st. ", "saint " = "st. ")),
      FIPS = map2_chr(State_Lower, County_Lower, function(state, county) {
        fips_candidates <- county_fips %>%
          filter(str_detect(str_to_lower(abbr), state) & str_detect(str_to_lower(county), county)) %>%
          pull(fips)
        
        if (length(fips_candidates) > 1) {
          suffix <- ifelse(state == "la", "parish", "county")
          adjusted_county <- ifelse(str_detect(county, "city"), county, paste(county, suffix))
          fips_candidates[which.min(adist(adjusted_county, str_to_lower(county_fips$county[fips_candidates])))]
        } else if (length(fips_candidates) == 0) {
          correction_map <- c(
            "prince georges" = "prince george's", "de kalb" = "dekalb", 
            "la porte" = "laporte", "st. marys" = "st. mary's",
            "matanuska susitna" = "matanuska-susitna", "obrien" = "o'brien",
            "de soto" = "desoto", "la salle" = "lasalle"
          )
          corrected_county <- correction_map[county] %||% county
          county_fips %>%
            filter(str_detect(str_to_lower(abbr), state) & str_detect(str_to_lower(county), corrected_county)) %>%
            pull(fips)
        } else {
          fips_candidates
        }
      })
    ) %>%
    pull(FIPS)
}

# Load data
resource_folder <- construct_path("Health and Social Equity - SJP - BHN Score Creation", "Data", "Resources")
data_folder <- construct_path("Health and Social Equity - SJP - BHN Score Creation", "Data", "Raw", "Life_Expectancy")

county_fips <- read_csv(file.path(resource_folder, "county_name_to_fips.csv"), col_types = cols(fips = col_character()))
cty_le <- read_csv(file.path(data_folder, "CHR_Life_Expectancy_2017-2019.csv"))
ct_le <- read_csv(file.path(data_folder, "USALEEP_Life_Expectancy.CSV"), col_types = cols(
  Tract.ID = col_character(), STATE2KX = col_character(), CNTY2KX = col_character(), TRACT2KX = col_character()
))

# Data cleaning
cty_le <- cty_le %>%
  mutate(across(everything(), ~if_else(. == "100+", "100", .))) %>%
  mutate(across(where(is.character), as.numeric))
  # Map FIPS codes to counties
  mutate(
    ST_ABBR = if_else(State == "District of Columbia", "DC", state.abb[match(State, state.name)]),
    CNTY_NUM = map_county_to_fips(cty_le, "ST_ABBR", "County", county_fips)
  )
  column_to_rownames("CNTY_NUM")

# Update ct_le with additional life expectancy calculations
ct_le <- ct_le %>%
  mutate(
    # Map CNTY_NUM to County values
    CNTY_NUM = str_c(STATE2KX, CNTY2KX),
    
    baseline = cty_le[CNTY_NUM, "County.Value"],
    black_le = cty_le[CNTY_NUM, "Black"],
    modifier = black_le / baseline,
    Life.Expectancy_black = if_else(is.na(estimate * modifier), estimate, estimate * modifier),
    Life.Expectancy_pop = estimate
  )

# Write results
output_folder <- construct_path("Health and Social Equity - SJP - BHN Score Creation", "Data", "Preprocessed")
write_csv(ct_le, file.path(output_folder, "CHR_USALEEP_Life_Expectancy.csv"), na = "")
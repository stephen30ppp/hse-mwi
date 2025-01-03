# Alcohol Outlet Density - Container Methods for CBP Data (County Business Patterns)
# By Karen Jiang
# Originated on: 6/8/21
# Last updated: 16/11/24 by Gih Ming

library(dplyr)
library(tidycensus)
library(censusapi)
library(readxl)
library(readr)
library(stringr)

source("Processing_Pipeline/crosswalk_func.R")

# Set Data Folder
base_folder <- gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive")))
data_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Raw")
resource_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Resources")
preprocessed_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Preprocessed")

# Read grocery states and process alcohol sales data
grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"), skip = 1) %>%
  mutate(across(3:5, ~ . == "Y"),
    any_alc_sales = rowSums(across(3:5), na.rm = TRUE) > 0
  )


# Read in and clean zip code according to common mistakes
# Inputs:
#   file_path: file path (including file name)
#   zip_col: zip code column name
# Outputs: Read and cleaned data frame
read_zips <- function(file_path, zip_col) {
  read_csv(file_path, col_types = cols(.default = "c")) %>%
    mutate(across(all_of(zip_col), ~ str_pad(str_remove_all(., "-"), 5, pad = "0")))
}

# Load US cities data
us_cities <- read_zips(file.path(resource_folder, "US_cities.csv"), "postal.code") %>%
  distinct(postal.code, .keep_all = TRUE) %>%
  column_to_rownames("postal.code")

addl_us_cities <- read_zips(file.path(resource_folder, "Additional_ZIP_US_Cities.csv"), "ZIP_CODE") %>%
  column_to_rownames("ZIP_CODE")


# Load in CBP data ----------
# NAICS Codes https://www.naics.com/code-search/?naicstrms=liquor
# Beer, Wine, and Liquor Stores = 445310
# Convenience Stores = 445120
# Convenience Stores & Gas Stations = 447110
# Grocery Stores = 445110

# Drinking Places On Premise (not used) = 772410

get_zbp <- function(naics, column_name) {
  getCensus(
    name = "zbp",
    key = Sys.getenv("CENSUS_API_KEY"),
    vintage = 2018,
    vars = c("ESTAB", "ZIPCODE"),
    NAICS2017 = naics
  ) %>%
    # Grabbing the max establishment size from each ZIP Code group.
    # Duplicate values are establishment counts by employee size.
    # Double checked with the original data and was an exact match.
    group_by(ZIPCODE) %>%
    slice_max(ESTAB, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(!!column_name := ESTAB)
}

# Fetch CBP data
bwl <- get_zbp(445310, "bwl")
convenience <- get_zbp(445120, "convenience")
convenience_gas <- get_zbp(447110, "convenience_gas")
grocery <- get_zbp(445110, "grocery")

# Filter grocery stores for allowed states
allowed_states <- grocery_states %>%
  filter(any_alc_sales) %>%
  pull(Abbrev)
zip_cw <- read_csv(file.path(resource_folder, "ZIP_Crosswalk.csv")) # Assuming zip crosswalk file exists
allowed_zips <- zip_cw %>% filter(STATE %in% allowed_states)
grocery <- grocery %>% filter(ZIPCODE %in% allowed_zips$ZIP_CODE)


# Combine outlets together into ESTAB count
all_outlets <- list(bwl, convenience, convenience_gas, grocery) %>%
  reduce(full_join, by = "ZIPCODE") %>%
  mutate(ESTAB = rowSums(select(., -ZIPCODE), na.rm = TRUE)) %>%
  select(ZIPCODE, ESTAB)


# Using crosswalk function from zip to zcta, sums up many to one relationships.
cbp <- zip_to_zcta(all_outlets, geoid_col = "ZIPCODE", meas_col = "ESTAB", use_mean = FALSE)

# Fetch ACS population data
pop <- get_acs(
  geography = "zcta",
  year = 2019,
  survey = "acs5",
  variables = "B01001_001",
  output = "wide",
  geometry = FALSE
) %>%
  select(ZCTA = GEOID, Population = B01001_001E)

# Merge CBP and population data
cbp <- cbp %>%
  left_join(pop, by = "ZCTA") %>%
  mutate(
    City = us_cities[ZCTA, "place.name"],
    State = us_cities[ZCTA, "state.code"]
  )

# Calculate alcohol outlet density
cbp_city <- cbp %>%
  group_by(City, State) %>%
  summarize(
    ESTAB = sum(ESTAB, na.rm = TRUE),
    Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate the measure value: number of outlets per population in ZCTA
  # Set NA alcohol outlets to 0
  # Set a maximum value for areas with more than 100 outlets per 100,000 population (10 Cities)
  mutate(
    alcoholoutlet_pop = pmin(ESTAB / Population, 0.01, na.rm = TRUE)
  )

final <- cbp %>%
  left_join(cbp_city, by = c("City", "State")) %>%
  select(ZCTA, alcoholoutlet_pop)

# Write output
write_csv(final, file.path(preprocessed_folder, "CBP_ZCTA_AlcoholOutlet.csv"), na = "")

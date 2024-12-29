# Alcohol Outlet Density - Container Methods for CBP Data (County Business Patterns)

# Author: Karen Jiang
# Version: 2021-12-27
# Last updated: 16/11/24 by Gih Ming

library(dplyr)
library(tidycensus)
library(censusapi)
library(readxl)

source("Processing_Pipeline/crosswalk_func.R")

# Set Data Folder
base_folder <- gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive")))
data_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Raw")
resource_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Resources")
preprocessed_folder <- file.path(base_folder, "Health and Social Equity - SJP - BHN Score Creation", "Data", "Preprocessed")


# Load state grocery laws
grocery_states <- read_xlsx(file.path(resource_folder, "BWL_Grocery_Laws.xlsx"), skip = 1) %>%
  mutate(across(3:5, ~ . == "Y"), # Convert "Y" to TRUE
    any_alc_sales = rowSums(across(3:5), na.rm = TRUE) > 0
  )

# Function to get cbp code from api, and return df with county FIPS and number of establishments
get_cbp <- function(naics, name) {
  getCensus(
    name = "cbp",
    key = Sys.getenv("CENSUS_API_KEY"),
    vintage = "2020",
    vars = "ESTAB",
    region = "county:*",
    NAICS2017 = naics
  ) %>%
    transmute(fips = paste0(state, county), !!name := ESTAB)
}

# Pull all counts for BWL, convenience stores, and convenience and gas stores
bwl <- get_cbp(445310, "bwl") # 1470 counties
convenience <- get_cbp(445120, "convenience") # 1215 counties
convenience_gas <- get_cbp(447110, "convenience_gas") # 2942 counties
grocery <- get_cbp(445110, "grocery") # 2354 counties

# Pull counts for convenience, convenience/gas, grocery stores within allowed states
allowed_states <- grocery_states %>%
  filter(any_alc_sales) %>%
  pull(Abbrev)
state_fips <- unique(fips_codes[, 1:2])

allowed_counties <- county_cw %>%
  left_join(state_fips, by = c("STATE" = "state_code")) %>%
  filter(state %in% allowed_states) %>%
  pull(GEOID)

# Filter store data by allowed counties
filter_allowed <- function(data) data %>% filter(fips %in% allowed_counties)

bwl <- filter_allowed(bwl)
convenience <- filter_allowed(convenience)
convenience_gas <- filter_allowed(convenience_gas)
grocery <- filter_allowed(grocery)

# Combine and calculate total establishments
all <- list(bwl, convenience, convenience_gas, grocery) %>%
  reduce(full_join, by = "fips") %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  rowwise() %>%
  mutate(ESTAB = sum(c_across(-fips))) %>%
  ungroup() %>%
  select(fips, ESTAB)

# Retrieve population data
pop <- get_acs(
  geography = "county",
  output = "wide",
  year = 2020,
  survey = "acs5",
  variables = "B01001_001"
) %>%
  select(GEOID, population = B01001_001E)

# Join population denominators by county
# Merge and calculate alcohol outlet density
cbp <- all %>%
  left_join(pop, by = c("fips" = "GEOID")) %>%
  mutate(
    alcoholoutlet_pop = ESTAB / population,
    # counties with NA establishment set to 0
    alcoholoutlet_pop = replace_na(alcoholoutlet_pop, 0)
  ) %>%
  select(fips, alcoholoutlet_pop)


# write out ----
data_folder <- file.path(
  gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive"))),
  "Health and Social Equity - SJP - BHN Score Creation",
  "Data", "Preprocessed"
)

# Write to CSV
write_csv(cbp, file.path(preprocessed_folder, "CBP_County_AlcoholOutlet.csv"), na = "")

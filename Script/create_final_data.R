library(tidyr)
install.packages("janitor")
library(dplyr)
library(tidyr)

### a) Prepare World Bank data

maternal_mortality <- read.csv("data/raw/maternal_mortality.csv")

maternal_long <- maternal_mortality |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    names_transform = list(year = as.integer),
    values_to = "maternal_mortality"
  )

library(tidyr)

# Function to convert mortality data from wide to long format
prepare_mortality <- function(data) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      names_transform = list(year = as.integer),
      values_to = "mortality"
    )
}

# Read the four data sets
maternal <- read.csv("data/raw/maternal_mortality.csv")
infant   <- read.csv("data/raw/infant_mortality.csv")
neonatal <- read.csv("data/raw/neonatal_mortality.csv")
under5   <- read.csv("data/raw/under5_mortality.csv")

# Apply the function to each data set
maternal_long <- prepare_mortality(maternal)
infant_long   <- prepare_mortality(infant)
neonatal_long <- prepare_mortality(neonatal)
under5_long   <- prepare_mortality(under5)



### Prepare disaster data
library(janitor)

disaster <- read.csv("data/raw/disaster.csv") |>
  clean_names()

# Check the cleaned column names
names(disaster)

disaster <- disaster |>
  dplyr::filter(
    year >= 2000,
    year <= 2019,
    disaster_type %in% c("Earthquake", "Drought")
  )

# c)
disaster <- disaster |>
  dplyr::select(year, iso, disaster_type)

# d) disaster <- disaster |>
disaster <- disaster |>
  dplyr::group_by(iso, year) |>
  dplyr::summarise(
    drought = as.integer(any(disaster_type == "Drought")),
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    .groups = "drop"
  )
# e) disaster <- disaster |>
  disaster <- disaster |>
  dplyr::select(year, iso, earthquake, drought)

### Prepare Conflicting Data
conflict <- read.csv("data/raw/conflict.csv")

# Inspect the first few rows
head(conflict)

conflict <- conflict |>
  dplyr::group_by(iso, year) |>
  dplyr::summarise(
    armed_conflict = as.integer(any(best >= 25)),
    .groups = "drop"
  ) |>
  dplyr::mutate(year = year + 1)

### Merge all the data
covariates <- read.csv("data/raw/covariates.csv")

# Keep the joining variables and rename each mortality measure
maternal_long <- maternal_long |>
  select(iso, year, maternal_mortality = mortality)

infant_long <- infant_long |>
  select(iso, year, infant_mortality = mortality)

neonatal_long <- neonatal_long |>
  select(iso, year, neonatal_mortality = mortality)

under5_long <- under5_long |>
  select(iso, year, under5_mortality = mortality)

# Use covariates as the base country–year data
final_data <- covariates |>
  left_join(maternal_long, by = c("iso", "year")) |>
  left_join(infant_long, by = c("iso", "year")) |>
  left_join(neonatal_long, by = c("iso", "year")) |>
  left_join(under5_long, by = c("iso", "year")) |>
  left_join(disaster, by = c("iso", "year")) |>
  left_join(conflict, by = c("iso", "year")) |>
  mutate(
    drought = replace_na(drought, 0L),
    earthquake = replace_na(earthquake, 0L),
    armed_conflict = replace_na(armed_conflict, 0L)
  )

# Create the output folder and save
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write.csv(
  final_data,
  "data/processed/final_data.csv",
  row.names = FALSE
)
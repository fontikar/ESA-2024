install.packages(c("tidyverse", "remotes"))
remotes::install_github("traitecoevo/austraits")

library(tidyverse)
library(austraits)

# Load in database
austraits <- load_austraits(version = "6.0.0")

# Extract data for Allocasuarina littoralis
a_littoralis_all_data <- austraits |>
  extract_taxa(taxon_name = "Allocasuarina littoralis")

# Summarise the counts for traits and export as a .csv
a_littoralis_all_data$traits |>
  count(trait_name) |>
  arrange(-n) |>
  write_csv("Allocasuarina_littoralis_available_traits_austraits_6.0.0.csv")

# Flatten database and export as a .csv
a_littoralis_all_data |>
  flatten_database() |>
  write_csv("Allocasuarina_littoralis_all_data_austraits_6.0.0.csv")

# Extract traits containing the word "fire"
a_littoralis_fire <- a_littoralis_all_data |>
  extract_trait(trait_names = "fire")

# Summarise traits containing the word "fire" and export as a .csv
a_littoralis_fire$traits |>
  count(trait_name) |>
  arrange(-n) |>
  write_csv("Allocasuarina_littoralis_available_traits_containing_fire_austraits_6.0.0.csv")

# Flatten "fire" database and export as a .csv
a_littoralis_fire |>
  flatten_database() |>
  write_csv("Allocasuarina_littoralis_data_containing_fire_austraits_6.0.0.csv")

# Map of all of Allocasuarina_littoralis "fire" data
a_littoralis_fire |>
  plot_locations()

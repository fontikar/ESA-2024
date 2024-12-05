library(austraits)

# Load data
get_versions(path = "data/austraits")
# austraits <- load_austraits(version = "6.0.0", path = "../austraits/ignore/data/austraits")
austraits <- load_austraits(version = "6.0.0", path = "data/austraits")

# Look up stuff
summarise_database(austraits, "trait_name")

austraits |> lookup_trait("seed")
austraits |> lookup_context_property("fire")
austraits |> lookup_location_property("fire")

# Extract data
austraits |> extract_trait("seed")
austraits |> extract_taxa("Banksia")
austraits |> extract_dataset("ABRS")
austraits |> extract_data(table = "trait", col = "basis_of_record", col_value = "field_experiment")
austraits |> extract_data(table = "trait", col = "lifestage", col_value = "sapling")

# Combine two extracted databases
bind_databases(austraits |> extract_trait("seed"), austraits |> extract_taxa("Banksia"))

# Generate one big, wide table
austraits |> flatten_database()

# Visualise data
austraits |>
  extract_trait("seed") |>
  plot_locations(feature = "trait_name")

austraits |>
  extract_trait("seed") |>
  plot_trait_distribution_beeswarm(trait_name = "seed_length", y_axis_category = "family")





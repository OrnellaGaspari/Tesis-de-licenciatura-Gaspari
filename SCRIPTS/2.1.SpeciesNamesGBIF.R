## Name: 2.1SpeciesNamesGBIF.R
## Author: Ornella Gaspari
## Goal: Extract unique species names from GBIF datasets and summarize 
##       the number of records from 1980 onwards for each group.

source(here("SCRIPTS", "0.libraries.R"))

# Mammals:

# Import dataset
Mammals1_GBIF <- read_csv(here("DATA", "Raw data", "trait_datasets", "GBIF_mammals1_30+occurrences_speciesTest.csv"))

#Filter the unique values
Mammals1_unique_values_GBIF <- as.data.frame(unique(Mammals1_GBIF$species))

# Change the column name so it's the same in both datasets and I can merge them in one
# list of mammals
# colnames(dataframe)[columnnumber] <- "column name"
colnames(Mammals1_unique_values_GBIF)[1] <- "unique_species"

# Import dataset
Mammals2_GBIF <- read_csv(here("DATA", "Raw data", "trait_datasets", "GBIF_mammals2_30+occurrences_speciesTest.csv"))

#Filter the unique values
Mammals2_unique_values_GBIF <- as.data.frame(unique(Mammals2_GBIF$species))

# Change the column name so it's the same in both datasets and I can merge them in one
# list of mammals
colnames(Mammals2_unique_values_GBIF)[1] <- "unique_species"

# Merge both dataframes to get only a list of mammals
merged <- rbind(Mammals2_unique_values_GBIF, Mammals1_unique_values_GBIF)

write.csv(merged, here("DATA", "Processed data", "IUCN data", "Mammals_species_list_GBIF_filtered.csv"), row.names = FALSE)

# Actually I would like to know how many records do I have for each species from 1980 
# onwards

# I will filter the species that have data from 1980 onwards
species_summary1 <- as.data.frame(Mammals1_GBIF %>%
                                    filter(year >= 1980) %>%            # Filter for records from 1980 onwards
                                    group_by(species) %>%               # Group by species
                                    summarise(n_records = n()))      # Count records per species

species_summary2 <- as.data.frame(Mammals2_GBIF %>%
                                    filter(year >= 1980) %>%            
                                    group_by(species) %>%               
                                    summarise(n_records = n()))      

unique_list <- rbind(species_summary1, species_summary2)
write.csv(unique_list, here("DATA", "Processed data", "IUCN data", "Mammals_species_list_GBIF_no_of_records.csv"), row.names = FALSE)

#Amphibians

# Import dataset
Amphibians1_GBIF <- read_csv(here("DATA", "Raw data", "trait_datasets", "GBIF_Amphibians1_30+occurrences_speciesTest.csv"))

#Filter the unique values
Amphibians1_unique_values_GBIF <- as.data.frame(unique(Amphibians1_GBIF$species))

# Change the column name so it's the same in both datasets and I can merge them in one
# list of mammals
# colnames(dataframe)[columnnumber] <- "column name"
colnames(Amphibians1_unique_values_GBIF)[1] <- "unique_species"

# Import dataset
Amphibians2_GBIF <- read_csv(here("DATA", "Raw data", "trait_datasets", "GBIF_Amphibians2_30+occurrences_speciesTest.csv"))

#Filter the unique values
Amphibians2_unique_values_GBIF <- as.data.frame(unique(Amphibians2_GBIF$species))

# Change the column name so it's the same in both datasets and I can merge them in one
# list of amphibians
colnames(Amphibians2_unique_values_GBIF)[1] <- "unique_species"

# Merge both dataframes to get only a list of amphibians
merged <- rbind(Amphibians2_unique_values_GBIF, Amphibians1_unique_values_GBIF)

write.csv(merged, here("DATA", "Processed data", "IUCN data", "Amphibians_species_list_GBIF_filtered.csv"), row.names = FALSE)

# Filter the species that have data from 1980 onwards
species_summary1 <- as.data.frame(Amphibians1_GBIF %>%
                                    filter(year >= 1980) %>%            
                                    group_by(species) %>%               
                                    summarise(n_records = n()))      

species_summary2 <- as.data.frame(Amphibians2_GBIF %>%
                                    filter(year >= 1980) %>%            
                                    group_by(species) %>%               
                                    summarise(n_records = n()))      

unique_list <- rbind(species_summary1, species_summary2)
write.csv(unique_list, here("DATA", "Processed data", "IUCN data", "Amphibians_species_list_GBIF_no_of_records.csv"), row.names = FALSE)

#Birds

# Import dataset
Birds_GBIF <- read_csv(here("DATA", "Raw data", "trait_datasets", "GBIF_Birds_30+occurrences_speciesTest.csv"))

#Filter the unique values
Birds_unique_values_GBIF <- as.data.frame(unique(Birds_GBIF$species))

write.csv(Birds_unique_values_GBIF, here("DATA", "Processed data", "IUCN data", "Bird_species_list_GBIF_filtered.csv"), row.names = FALSE)

# Filter the species that have data from 1980 onwards
species_summary <- as.data.frame(Birds_GBIF %>%
                                   filter(year >= 1980) %>%            
                                   group_by(species) %>%               
                                   summarise(n_records = n()))     

write.csv(species_summary, here("DATA", "Processed data", "IUCN data", "Birds_species_list_GBIF_no_of_records.csv"), row.names = FALSE)
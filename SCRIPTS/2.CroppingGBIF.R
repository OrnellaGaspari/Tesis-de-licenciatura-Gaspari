## Name: 2_croppingGBIF.R
## Author: Ornella Gaspari
## Modified from: ## Taxa occurrences by Andre P. Silva, Afonso Barrocal & Inês Silva ##
## Goal: Cropping GBIF with the species lists I got in 1. Cropping IUCN RedList.R

source(here("SCRIPTS", "0.libraries.R"))

# --- Directories ---
input_dir <- here("DATA", "Processed data", "IUCN data")
output_dir <- here("DATA", "Raw data", "trait_datasets")

# Before running load your own GBIF credentials
keys <- yaml::read_yaml(here("config", "api_keys.yml"))

# NOTE: input data is within "Processed data" folder because it was where IUCN 
# data folder was saved originally.

# Use IUCN species names and ranges
# downloaded manually, later find a way to download automatically through R
#IUCN_mammals <- sf::st_read("./trait_datasets/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
#mammal_sps <- unique(IUCN_mammals$sci_name)

species_df <- read.csv("mammals1_status_area.csv", stringsAsFactors = FALSE)
mammal_sps <- unique(species_df$sci_name)

# for testing only
#speciesTest <- c("Alces alces", "Canis lupus")
#mammal_sps <- as.data.frame(mammal_sps) %>% dplyr::filter(mammal_sps %in% speciesTest)

# Extract occurrences available in GBIF (e.g. mammals). Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(mammal_sps) %>%
  pull("mammal_sps") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys

# to download datasets from gbif credentials are necessary.
# Register at https://www.gbif.org/user/profile

test <- occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = keys$gbif_user , # ADD USERNAME HERE
  pwd = keys$gbif_pwd, # ADD PASSWORD HERE
  email = keys$gbif_email) # ADD EMAIL ASSOCIATE WITH ACCOUNT HERE

# To see what´s the download key 
print(test)

# check if download is finished

occ_download_wait('0066322-251120083545085')
invisible(gc())

# retrieve the download from GBIF to my computer
# first I create the directory
#dir.create("./trait_datasets", showWarnings = FALSE, recursive = TRUE)

d <- occ_download_get(
  key = '0066322-251120083545085',
  path = "./trait_datasets"
)

# with the download key we can go directly to gbif and download the folder with
# the data without running the script again

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_mammal_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_mammal_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  file.path(output_dir, "GBIF_mammals1_30+occurrences_speciesTest.csv"),
  row.names = FALSE
)
invisible(gc())

#MAMMALS1 citation: GBIF.org (26 December 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.wjmz6u

################################# MAMMALS2 #####################################

species_df <- read.csv("mammals2_status_area.csv", stringsAsFactors = FALSE)
mammal_sps <- unique(species_df$sci_name)

# Extract occurrences available in GBIF. Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(mammal_sps) %>%
  pull("mammal_sps") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys


test <- occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = keys$gbif_user , 
  pwd = keys$gbif_pwd, 
  email = keys$gbif_email) 

# To see what´s the download key 
print(test)

# check if download is finished
occ_download_wait('0041633-250525065834625')
invisible(gc())

# retrieve the download from GBIF to my computer

d <- occ_download_get(
  key = '0041633-250525065834625',
  path = "./trait_datasets"
)

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_mammal_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_mammal_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  file.path(output_dir, "GBIF_mammals2_30+occurrences_speciesTest.csv"),
  row.names = FALSE
)
invisible(gc())

# MAMMALS 2 citation: GBIF.org (12 June 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.5vnw49

################################### BIRDS ######################################

species_df <- read.csv("birds_filtered_with_guide.csv", stringsAsFactors = FALSE)
birds_sps <- unique(species_df$sci_name)

# Extract occurrences available in GBIF. Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(birds_sps) %>%
  pull("birds_sps") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys

# credentials

test <- occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = keys$gbif_user , 
  pwd = keys$gbif_pwd, 
  email = keys$gbif_email)

# To see what´s the download key 
print(test)

# check if download is finished
occ_download_wait('0031098-250827131500795')
invisible(gc())

# retrieve the download from GBIF to my computer

d <- occ_download_get(
  key = '0031098-250827131500795',
  path = "./trait_datasets"
)

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_bird_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  filter(year >= 1980) %>%   
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_bird_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  file.path(output_dir,"GBIF_Birds_30+occurrences_speciesTest.csv"),
  row.names = FALSE
)
invisible(gc())

# BIRDS citation: GBIF.org (5 September 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.znf82d

############################### AMPHIBIANS 1 ###################################

species_df <- read.csv("Amphibians1_status_area.csv", stringsAsFactors = FALSE)
amphibians_sps <- unique(species_df$sci_name)

# Extract occurrences available in GBIF. Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(amphibians_sps) %>%
  pull("amphibians_sps") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys

# credentials
test <- occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = keys$gbif_user , 
  pwd = keys$gbif_pwd, 
  email = keys$gbif_email) 

# To see what´s the download key 
print(test)

# check if download is finished
occ_download_wait('0061253-250525065834625')
invisible(gc())

# retrieve the download from GBIF to my computer

d <- occ_download_get(
  key = '0061253-250525065834625',
  path = "./trait_datasets"
)

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_amphibian_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_amphibian_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  file.path(output_dir,"GBIF_Amphibians1_30+occurrences_speciesTest.csv"),
  row.names = FALSE
)
invisible(gc())

# Amphibians citation: GBIF.org (21 June 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.gcnat8

############################## AMPHIBIANS 2 ####################################

species_df <- read.csv("Amphibians2_status_area.csv", stringsAsFactors = FALSE)
amphibians_sps <- unique(species_df$sci_name)

# Extract occurrences available in GBIF. Filter species >30 records
gbif_taxon_keys <-
  as.data.frame(amphibians_sps) %>%
  pull("amphibians_sps") %>% #use the sps names from the file
  name_backbone_checklist() %>% #match to backbone
  filter(!matchType == "NONE") %>% #get matched names
  pull(usageKey) #get the GBIF taxon keys

# credentials
test <- occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  format = "SIMPLE_CSV",
  user = keys$gbif_user , 
  pwd = keys$gbif_pwd, 
  email = keys$gbif_email) 

# To see what´s the download key 
print(test)

# check if download is finished
occ_download_wait('0061258-250525065834625')
invisible(gc())

# retrieve the download from GBIF to my computer

d <- occ_download_get(
  key = '0061258-250525065834625',
  path = "./trait_datasets"
)

# import download to current session
gbif_data <- occ_download_import(d)

GBIF_amphibian_sps <- 
  gbif_data %>%
  # remove occurrences without coordinates
  drop_na(c(decimalLatitude, decimalLongitude)) %>% 
  group_by(species) %>%
  # filter individuals with more than 30 occurrences
  dplyr::filter(n() > 30)

# Write species occurences, with the subselection of variables
write.csv(
  GBIF_amphibian_sps[, c("species", "decimalLatitude", "decimalLongitude", "year")],
  file.path(output_dir,"GBIF_Amphibians2_30+occurrences_speciesTest.csv"),
  row.names = FALSE
)
invisible(gc())

# Amphibians2 citation: GBIF.org (21 June 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.6kecpg

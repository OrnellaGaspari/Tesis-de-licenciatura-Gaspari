## Name: 4.Filter_occurrence_data_by_year_and_cell.R
## Author: Ornella Gaspari
## Date: 18th March 2026
## Goal: Filter occurrence data by year (2010-2020) and keep one record per raster cell 
##       for each species, excluding points outside the distribution.
## The main output of this script is All_species_data.csv, the csv with the 
## occurrences for all the species that will be the input for the SDMs.

## NOTE: The script is the same as it was to get the occurrence data, it could be
## refactored to get a simplified version.

source(here("SCRIPTS", "0.libraries.R"))

############################## Filter by date ##################################
#######    AMPHIBIANS
# C. ornata
# Read csv
data <- read.csv(here("DATA", "Processed data", "C_ornata_Merged_base.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "C_ornata_2010_2020.csv"), row.names = FALSE)

# B. pulchella
# Read csv
data <- read.csv(here("DATA", "Processed data", "B_pulchella_Merged_base.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "B_pulchella_2010_2020.csv"), row.names = FALSE)

# R. dorbignyi
# Read csv
data <- read.csv(here("DATA", "Processed data", "R_dorbignyi_Merged_base.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "R_dorbignyi_2010_2020.csv"), row.names = FALSE)

# O. asper
# Read csv
data <- read.csv(here("DATA", "Processed data", "O_asper_Merged_base.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "O_asper_2010_2020.csv"), row.names = FALSE)


#######    BIRDS
# E. platensis
# Read csv
data <- read.csv(here("DATA", "Processed data", "Embernagra_platensis_data.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "E_platensis_2010_2020.csv"), row.names = FALSE)

# P. virescens
# Read csv
data <- read.csv(here("DATA", "Processed data", "Pseudoleistes_virescens_data.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "P_virescens_2010_2020.csv"), row.names = FALSE)

# R. americana
# Read csv
data <- read.csv(here("DATA", "Processed data", "Rhea_americana_data.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "R_americana_2010_2020.csv"), row.names = FALSE)

# X. flavus
# Read csv
data <- read.csv(here("DATA", "Processed data", "Xanthopsar_flavus_data.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "X_flavus_2010_2020.csv"), row.names = FALSE)


#######      MAMMALS
# O. bezoarticus
# Read csv
data <- read.csv(here("DATA", "Processed data", "O_bezoarticus_GBIF_extraido.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "O_bezoarticus_2010_2020.csv"), row.names = FALSE)

# C. australis
# Read csv
data <- read.csv(here("DATA", "Processed data", "C_australis_Merged_base.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)
# When applying this filter I only get 20 records for the period, I will work with the
# full dataset for this species

# Save
#write.csv(filtered_data, here("DATA", "Processed data", "C_australis_2010_2020.csv"), row.names = FALSE)

# D. hybridus
# Read csv
data <- read.csv(here("DATA", "Processed data", "D_hybridus_Merged_base.csv"))

# Filter 2010-2020
filtered_data <- data %>%
  filter(year >= 2010 & year <= 2020)

# Save
write.csv(filtered_data, here("DATA", "Processed data", "D_hybridus_2010_2020.csv"), row.names = FALSE)

########################## Get one record per cell #############################
# Filter only one record per cell. The code that I use chooses the first record  
# that appears in the table for each cell.

# Load a SEALS land-use layer
seals_ref <- rast(here("DATA", "Processed data", "lulc_esa_seals7_2015.tif"))

#######    AMPHIBIANS
# C. ornata
Species_points <- read.csv(here("DATA", "Processed data", "C_ornata_2010_2020.csv"))

# Convert coordinates into a spatial object
points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

# Make sure crs matches
points_sp <- project(points_sp, crs(seals_ref))

# Extract the projected coordinates as a matrix
coords <- geom(points_sp)[, c("x", "y")]

# Identify each cell number
Species_points$cell_id <- cellFromXY(seals_ref, coords)

# Filter one point/ cell
final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

# Save
write.csv(final_data, here("DATA", "Processed data", "C_ornata_one_record_cell.csv"), row.names = FALSE)

# B. pulchella
Species_points <- read.csv(here("DATA", "Processed data", "B_pulchella_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "B_pulchella_one_record_cell.csv"), row.names = FALSE)

# R. dorbignyi
Species_points <- read.csv(here("DATA", "Processed data", "R_dorbignyi_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "R_dorbignyi_one_record_cell.csv"), row.names = FALSE)

# O. asper
Species_points <- read.csv(here("DATA", "Processed data", "O_asper_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "O_asper_one_record_cell.csv"), row.names = FALSE)

#######    BIRDS
# E. platensis
Species_points <- read.csv(here("DATA", "Processed data", "E_platensis_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "E_platensis_one_record_cell.csv"), row.names = FALSE)

# P. virescens
Species_points <- read.csv(here("DATA", "Processed data", "P_virescens_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "P_virescens_one_record_cell.csv"), row.names = FALSE)

# R. americana
Species_points <- read.csv(here("DATA", "Processed data", "R_americana_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "R_americana_one_record_cell.csv"), row.names = FALSE)

# X. flavus
Species_points <- read.csv(here("DATA", "Processed data", "X_flavus_2010_2020.csv"))

points_sp <- vect(Species_points, geom=c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")

points_sp <- project(points_sp, crs(seals_ref))

coords <- geom(points_sp)[, c("x", "y")]

Species_points$cell_id <- cellFromXY(seals_ref, coords)

final_data <- Species_points %>%
  distinct(cell_id, .keep_all = TRUE)

write.csv(final_data, here("DATA", "Processed data", "X_flavus_one_record_cell.csv"), row.names = FALSE)

#######      MAMMALS
# Only for mammals I will filter out the points in the water using mask, I'm afraid if
# I do it in birds and amphibians I might loose relevant points
seals_land_mask <- seals_ref
seals_land_mask[seals_land_mask == 6] <- NA

process_species_records <- function(file_path,
                                    seals_ref,
                                    seals_land_mask) {
  
  # 1. Read data
  Species_points <- read.csv(file_path)
  
  raw_n <- nrow(Species_points)
  
  # Detect species name from column
  species_name <- unique(Species_points$species)
  
  # 2. Convert to SpatVector
  points_sp <- vect(
    Species_points,
    geom = c("decimalLongitude", "decimalLatitude"),
    crs = "EPSG:4326",
    keepgeom = TRUE
  )
  
  # 3. Project
  points_projected <- project(points_sp, crs(seals_ref))
  
  # 4. Extract land mask values
  habitat_check <- terra::extract(seals_land_mask, points_projected)
  
  # 5. Keep only land points
  points_land <- points_projected[!is.na(habitat_check[, 2]), ]
  land_n <- nrow(points_land)
  
  # 6. Calculate cell ID
  points_land$cell_id <- cellFromXY(seals_ref, crds(points_land))
  
  # 7. Keep one record per cell
  final_data <- as.data.frame(points_land) %>%
    distinct(cell_id, .keep_all = TRUE) %>%
    select(species, decimalLatitude, decimalLongitude, year, Source, cell_id)
  
  unique_cells_n <- nrow(final_data)
  
  # 8. Print summary
  message(
    species_name, ": ",
    raw_n, " raw records → ",
    land_n, " land → ",
    unique_cells_n, " unique cells"
  )
  
  return(final_data)
}

# O. bezoarticus
O_bezoarticus_data <- process_species_records(
  here("DATA", "Processed data", "O_bezoarticus_2010_2020.csv"),
  seals_ref,
  seals_land_mask
)

# Ozotoceros bezoarticus: 177 raw records → 171 land → 170 unique cells

write.csv(O_bezoarticus_data, here("DATA", "Processed data", "O_bezoarticus_one_record_cell.csv"), row.names = FALSE)

# C. australis
C_australis_data <- process_species_records(
  here("DATA", "Processed data", "C_australis_Merged_base.csv"), # I use the full database because for the period 2010-2020 
  seals_ref,                     # there are only 20 something records
  seals_land_mask
)

# Ctenomys australis: 243 raw records → 224 land → 81 unique cells

write.csv(C_australis_data, here("DATA", "Processed data", "C_australis_one_record_cell_from_full_dataset.csv"), row.names = FALSE)

# D. hybridus
D_hybridus_data <- process_species_records(
  here("DATA", "Processed data", "D_hybridus_2010_2020.csv"),
  seals_ref,
  seals_land_mask
)

# Dasypus hybridus: 282 raw records → 280 land → 255 unique cells

write.csv(D_hybridus_data, here("DATA", "Processed data", "D_hybridus_one_record_cell.csv"), row.names = FALSE)


##### Exclude points outside the distribution and draw the occurrences maps #####

#Base map 
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Argentine provinces
prov <- geodata::gadm(
  country = "ARG",
  level = 1,
  path = tempdir()
)

provincias_sf <- st_as_sf(prov) %>%
  st_transform(4326)

# Load and transform pampa shapefile
pampa_sf <- st_read(here("DATA", "Processed data", "subregiones.shp")) %>% 
  st_transform(crs = 4326)

########      AMPHIBIANS
# C. ornata
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "C_ornata_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE #to keep the csv columns and be able to save changes after filtering
)

# Filter records that don't correspond to the specie's distribution
# mapview() allows me to click the points and identify them so I can then exclude the 
# ones that don´t correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(350, 347), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "C_ornata_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "#A4DE02", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Ceratophrys ornata",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "C_ornata_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# B. pulchella
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "B_pulchella_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE 
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)

Species_sf <- Species_sf[-c(701, 703, 599, 606, 601, 605, 602, 604, 600, 882, 801, 914), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "B_pulchella_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "#76BA1B", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Boana pulchella",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "B_pulchella_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# R. dorbignyi
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "R_dorbignyi_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "#556B2F", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Rhinella dorbignyi",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "R_dorbignyi_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# O. asper
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "O_asper_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)

Species_sf <- Species_sf[-c(102, 101, 106, 103, 104, 105, 92, 93), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "O_asper_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "#8B4513", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Odontophrynus asper",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "O_asper_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# Draw the figure for all the amphibian species together
# Define the function to draw the map to avoid repeating

make_species_map <- function(Species_sf, color, title, world_sf, provincias_sf, pampa_sf){
  
  bbox <- st_bbox(Species_sf)
  total_points <- nrow(Species_sf)
  
  ggplot() +
    geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
    geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
    geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3) +
    geom_sf(data = Species_sf, color = color, size = 1.5, alpha = 0.6) +
    coord_sf(
      xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1),
      ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1)
    ) +
    theme_minimal() +
    labs(
      title = title,
      subtitle = paste0("n = ", total_points, " | Data filtered by cell (SEALS 300m resolution)")
    )
}

# C. ornata
Cornata <- read.csv(here("DATA", "Processed data", "C_ornata_one_record_cell_distribution_filtered.csv"))
Cornata_sf <- st_as_sf(
  Cornata, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p1 <- make_species_map(Cornata_sf,"#A4DE02", "Ceratophrys ornata", world_sf, provincias_sf, pampa_sf)

# B. pulchella
Bpulchella <- read.csv(here("DATA", "Processed data", "B_pulchella_one_record_cell_distribution_filtered.csv"))
Bpulchella_sf <- st_as_sf(
  Bpulchella, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p2 <- make_species_map(Bpulchella_sf, "#76BA1B", "Boana pulchella", world_sf, provincias_sf, pampa_sf)

# R. dorbignyi
Rdorbignyi <- read.csv(here("DATA", "Processed data", "R_dorbignyi_one_record_cell.csv"))
Rdorbignyi_sf <- st_as_sf(
  Rdorbignyi, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p3 <- make_species_map(Rdorbignyi_sf, "#556B2F", "Rhinella dorbignyi", world_sf, provincias_sf, pampa_sf)

# O. asper
Oasper <- read.csv(here("DATA", "Processed data", "O_asper_one_record_cell_distribution_filtered.csv"))
Oasper_sf <- st_as_sf(
  Oasper, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p4 <- make_species_map(Oasper_sf, "#8B4513", "Odontophrynus asper", world_sf, provincias_sf, pampa_sf)

final_figure <- (p1 | p2) /
  (p3 | p4)
ggsave(
  here("OUTPUT", "Amphibian_species_maps_combined.png"),
  final_figure,
  width = 14,
  height = 12,
  dpi = 300
)

# bind all the amphibian databases
amphibians_all <- bind_rows(
  Cornata,
  Bpulchella,
  Rdorbignyi,
  Oasper
)%>%
  select(
    species,
    decimalLatitude,
    decimalLongitude,
    year,
    Source
  )  

write.csv(amphibians_all, here("DATA", "Processed data", "Amphibians_data.csv"), row.names = FALSE)

#######    BIRDS
# E. platensis
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "E_platensis_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(9636, 821, 3648, 6966, 1496, 2064, 9827), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "E_platensis_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# I decided to avoid deleting points that fall outside but nearby the distribution 
# because I think they might have relevant information. However, I deleted the isolated
# points

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = Species_sf, color = "#BDB76B", size = 1.5, alpha = 0.6) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Embernagra platensis",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "E_platensis_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# P. virescens 
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "P_virescens_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE 
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(2713, 2278, 3189, 2225, 3540, 2354, 1350, 4056), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "P_virescens_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = Species_sf, color = "#FFD700", size = 1.5, alpha = 0.6) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Pseudoleistes virescens",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "P_virescens_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# R. americana
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "R_americana_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE 
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(4820, 4184, 1829, 2547, 4170, 3590, 380, 883, 3755, 4539, 3054, 2918, 1537, 2736, 2018, 507, 3746, 2437, 3509, 4631, 4559, 793, 780, 218, 3137, 935, 1949, 4861, 4811, 4819, 4457, 4741, 4509, 4449, 4731, 4404, 4930, 4751, 1665, 3319, 1786, 4606, 1996, 4038, 1817, 360, 3121, 4010, 2760, 2771, 3271, 2027, 630, 1890, 2911, 2466, 1880, 2086, 874, 4425, 2228, 2876, 821, 154, 3350, 4162, 4012, 1454, 2778), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "R_americana_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = Species_sf, color = "#777B7E", size = 1.5, alpha = 0.6) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Rhea americana",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "R_americana_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# X. flavus
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "X_flavus_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE 
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(1), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "X_flavus_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = Species_sf, color = "#F4C430", size = 1.5, alpha = 0.6) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Xantopsar flavus",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "X_flavus_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# Draw the figure for all the bird species together
# Define the function to draw the map to avoid repeating
# I make one function specific for birds because the pampean map is the last thing to draw, 
# there are too many points and if I draw the pampas below I can't see the map
make_birds_map <- function(Species_sf, color, title, world_sf, provincias_sf, pampa_sf){
  
  bbox <- st_bbox(Species_sf)
  total_points <- nrow(Species_sf)
  
  ggplot() +
    geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
    geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
    geom_sf(data = Species_sf, color = color, size = 1.5, alpha = 0.6) +
    geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3) +
    coord_sf(
      xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1),
      ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1)
    ) +
    theme_minimal() +
    labs(
      title = title,
      subtitle = paste0("n = ", total_points, " | Data filtered by cell (SEALS 300m resolution)")
    )
}

# E. platensis
Eplatensis <- read.csv(here("DATA", "Processed data", "E_platensis_one_record_cell_distribution_filtered.csv"))
Eplatensis_sf <- st_as_sf(
  Eplatensis, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p1 <- make_birds_map(Eplatensis_sf,"#BDB76B", "Embernagra platensis", world_sf, provincias_sf, pampa_sf)

# P. virescens
Pvirescens <- read.csv(here("DATA", "Processed data", "P_virescens_one_record_cell_distribution_filtered.csv"))
Pvirescens_sf <- st_as_sf(
  Pvirescens, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p2 <- make_birds_map(Pvirescens_sf, "#FFD700", "Pseudoleistes virescens", world_sf, provincias_sf, pampa_sf)

# R. americana
Ramericana <- read.csv(here("DATA", "Processed data", "R_americana_one_record_cell_distribution_filtered.csv"))
Ramericana_sf <- st_as_sf(
  Ramericana, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p3 <- make_birds_map(Ramericana_sf, "#777B7E", "Rhea americana", world_sf, provincias_sf, pampa_sf)

# X. flavus
Xflavus <- read.csv(here("DATA", "Processed data", "X_flavus_one_record_cell_distribution_filtered.csv"))
Xflavus_sf <- st_as_sf(
  Xflavus, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p4 <- make_birds_map(Xflavus_sf, "#F4C430", "Xantopsar flavus", world_sf, provincias_sf, pampa_sf)


final_figure <- (p1 | p2) /
  (p3 | p4)
ggsave(
  here("OUTPUT", "Bird_species_maps_combined.png"),
  final_figure,
  width = 14,
  height = 12,
  dpi = 300
)

# bind all the bird databases
birds_all <- bind_rows(
  Eplatensis,
  Pvirescens,
  Ramericana,
  Xflavus
)%>%
  mutate(Source = "GBIF")%>%
  select(
    species,
    decimalLatitude,
    decimalLongitude,
    year,
    Source
  )  

write.csv(birds_all, here("DATA", "Processed data", "Birds_data.csv"), row.names = FALSE)

#######      MAMMALS
# O. bezoarticus
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "O_bezoarticus_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE 
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(84, 130), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "O_bezoarticus_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "#8B4513", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Ozotoceros bezoarticus",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "O_bezoarticus_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# C. australis
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "C_australis_one_record_cell_from_full_dataset.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326,
  remove = FALSE 
)

# Filter records that don't correspond to the specie's distribution
mapview(Species_sf)
Species_sf <- Species_sf[-c(62, 67), ]

# Save csv without points outside the distribution
Species_final_csv <- st_drop_geometry(Species_sf)
write.csv(Species_final_csv, here("DATA", "Processed data", "C_australis_one_record_cell_distribution_filtered.csv"), row.names = FALSE)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "#A0522D", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Ctenomys australis",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period full dataset"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "C_australis_one_record_cell_from_full_dataset_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# D. hybridus
# Convert data into spatial object 
Species <- read.csv(here("DATA", "Processed data", "D_hybridus_one_record_cell.csv"))
Species_sf <- st_as_sf(
  Species, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

mapview(Species_sf)

# Count points
total_points <- nrow(Species_sf)

# Define the displayed area
bbox <- st_bbox(Species_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray80") +
  geom_sf(data = provincias_sf, fill = NA, color = "gray70", size = 0.3) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.3, size = 0.5) +
  geom_sf(data = Species_sf, color = "slategray4", size = 1.5, alpha = 0.6) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Presence records: Dasypus hybridus",
    subtitle = paste0("Total records: n = ", total_points, " Data filtered by cell (SEALS 300m resolution) | Period 2010-2020"),
    x = "Longitude",
    y = "Latitude"
  ) 

# Save
ggsave(
  filename = here("OUTPUT", "D_hybridus_one_record_cell_map.png"),
  width = 8,
  height = 7,
  dpi = 300
)

# Draw the map for all the mammal species
# O. bezoarticus
Obezoarticus <- read.csv(here("DATA", "Processed data", "O_bezoarticus_one_record_cell_distribution_filtered.csv"))
Obezoarticus_sf <- st_as_sf(
  Obezoarticus, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p1 <- make_species_map(Obezoarticus_sf,"#8B4513", "Ozotoceros bezoarticus", world_sf, provincias_sf, pampa_sf)

# C. australis
Caustralis <- read.csv(here("DATA", "Processed data", "C_australis_one_record_cell_distribution_filtered.csv"))
Caustralis_sf <- st_as_sf(
  Caustralis, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p2 <- make_species_map(Caustralis_sf, "#A0522D", "Ctenomys australis", world_sf, provincias_sf, pampa_sf)

# D. hybridus
Dhybridus <- read.csv(here("DATA", "Processed data", "D_hybridus_one_record_cell.csv"))
Dhybridus_sf <- st_as_sf(
  Dhybridus, 
  coords = c("decimalLongitude", "decimalLatitude"), 
  crs = 4326
)

p3 <- make_species_map(Dhybridus_sf, "slategray4", "Dasypus hybridus", world_sf, provincias_sf, pampa_sf)


final_figure <- p1 | p2 | p3

ggsave(
  here("OUTPUT", "Mammal_species_maps_combined.png"),
  final_figure,
  width = 18,
  height = 6,
  dpi = 300
)

# bind all the mammal databases WITHOUT C australis!!

mammals_all <- bind_rows(
  Obezoarticus,
  Dhybridus,
)%>%
  select(
    species,
    decimalLatitude,
    decimalLongitude,
    year,
    Source
  )  

write.csv(mammals_all, here("DATA", "Processed data", "Mammals_data.csv"), row.names = FALSE)

#################### Create a database for all the species ##################### 
Amphibians <- read.csv(here("DATA", "Processed data", "Amphibians_data.csv"))
Birds <- read.csv(here("DATA", "Processed data", "Birds_data.csv"))
Mammals <- read.csv(here("DATA", "Processed data", "Mammals_data.csv"))

all_species <- bind_rows(
  Amphibians,
  Birds,
  Mammals,
)

write.csv(all_species, here("DATA", "Processed data", "All_species_data.csv"), row.names = FALSE)
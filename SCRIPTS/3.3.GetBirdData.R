## Name: 3.3GetBirdData.R
## Author: Ornella Gaspari
## Goal: Filter data of recommended bird species to work with, select bird species  
##       for the projet and extract data for those selected species

source(here("SCRIPTS", "0.libraries.R"))

# Filter recommended species
Datos_aves <- read.csv(here("DATA", "Processed data", "GBIF_Birds_30+occurrences_speciesTest.csv"))
head(Datos_aves)

especies_preseleccionadas <- c("Ammodramus humeralis", "Embernagra platensis", "Nothura maculosa", "Pseudoleistes virescens", "Rhea americana", "Rhynchotus rufescens", "Spartonoica maluroides", "Syrigma sibilatrix", "Xanthopsar flavus")

Datos_filtrados <- Datos_aves %>% 
  filter(species %in% especies_preseleccionadas)

write.csv(Datos_filtrados,
          here("DATA", "Processed data", "Aves_recomendadas.csv"),
          row.names = FALSE)

# Get data for the species chosen for the project
Datos_aves <- read.csv(here("DATA", "Processed data", "GBIF_Birds_30+occurrences_speciesTest.csv"))
head(Datos_aves)

Selected_bird_species <- c("Embernagra platensis", "Pseudoleistes virescens", "Rhea americana", "Xanthopsar flavus")

Selected_bird_species_data <- Datos_aves %>% 
  filter(species %in% Selected_bird_species)

write.csv(Selected_bird_species_data,
          here("DATA", "Processed data", "Selected_bird_species_data.csv"),
          row.names = FALSE)

head(Selected_bird_species_data)

# CSV for each species, filtering out duplicates
#E. platensis
Embernagra_platensis <- c("Embernagra platensis")

Embernagra_platensis_data <- Datos_aves %>% 
  filter(species %in% Embernagra_platensis)

Embernagra_platensis_data_nodup <- Embernagra_platensis_data |> 
  dplyr::distinct()

write.csv(Embernagra_platensis_data_nodup,
          here("DATA", "Processed data", "Embernagra_platensis_data.csv"),
          row.names = FALSE)

head(Embernagra_platensis_data)

#P. virescens
Pseudoleistes_virescens <- c("Pseudoleistes virescens")

Pseudoleistes_virescens_data <- Datos_aves %>% 
  filter(species %in% Pseudoleistes_virescens)

Pseudoleistes_virescens_data_nodup <- Pseudoleistes_virescens_data |> 
  dplyr::distinct()

write.csv(Pseudoleistes_virescens_data_nodup,
          here("DATA", "Processed data", "Pseudoleistes_virescens_data.csv"),
          row.names = FALSE)

head(Pseudoleistes_virescens_data)

#R. americana
Rhea_americana <- c("Rhea americana")

Rhea_americana_data <- Datos_aves %>% 
  filter(species %in% Rhea_americana)

Rhea_americana_data_nodup <- Rhea_americana_data |> 
  dplyr::distinct()

# Filter southern hemisfere for Rhea americana data
Rhea_americana_data_nodup <- Rhea_americana_data_nodup %>%
  filter(decimalLatitude < 0)

write.csv(Rhea_americana_data_nodup,
          here("DATA", "Processed data", "Rhea_americana_data.csv"),
          row.names = FALSE)

head(Rhea_americana_data)

#X. flavus
Xanthopsar_flavus <- c("Xanthopsar flavus")

Xanthopsar_flavus_data <- Datos_aves %>% 
  filter(species %in% Xanthopsar_flavus)

# Delete duplicates and correct points with swapped coordinates
Xanthopsar_flavus_data_nodup <- Xanthopsar_flavus_data %>%
  dplyr::distinct() %>%
  mutate(
    # temporary columns to avoid loosing data 
    lat_old = decimalLatitude,
    lon_old = decimalLongitude,
    # If latitude is too negative it might be longitude
    decimalLatitude = ifelse(lat_old < -45, lon_old, lat_old),
    decimalLongitude = ifelse(lat_old < -45, lat_old, lon_old)
  ) %>%
  # delete temporary columns
  select(-lat_old, -lon_old) 

write.csv(Xanthopsar_flavus_data_nodup,
          here("DATA", "Processed data", "Xanthopsar_flavus_data.csv"),
          row.names = FALSE)

head(Xanthopsar_flavus_data)

#--- Draw occurrences maps
# Base map
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Pampean region map
pampa_sf <- st_read(here("DATA", "Processed data", "subregiones.shp"))
pampa_sf <- st_transform(pampa_sf, crs = 4326)

#-- Embernagra platensis
# Turn the data into sf
Embernagra_platensis_data <- read.csv(here("DATA", "Processed data", "Embernagra_platensis_data.csv"))

Embernagra_platensis_data <- st_as_sf(
  Embernagra_platensis_data_nodup,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Limit the extension to the occupied area
bbox <- st_bbox(Embernagra_platensis_data)

ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = Embernagra_platensis_data, color = "darkgreen", size = 2) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = unique(Embernagra_platensis_data$species)
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "Embernagra_platensis_pampasmap.png"),
  width = 7,
  height = 6,
  dpi = 300
)


#-- Pseudoleistes virescens
# Turn the data into sf
Pseudoleistes_virescens_data <- read.csv(here("DATA", "Processed data", "Pseudoleistes_virescens_data.csv"))

Pseudoleistes_virescens_data<- st_as_sf(
  Pseudoleistes_virescens_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Graph the species
# Limit the extension to the occupied area
bbox <- st_bbox(Pseudoleistes_virescens_data)

ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = Pseudoleistes_virescens_data, color = "yellow", size = 2) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = unique(Pseudoleistes_virescens_data$species)
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "Pseudoleistes_virescens_pampasmap.png"),
  width = 7,
  height = 6,
  dpi = 300
)

#-- Rhea americana
# Turn the data into sf
Rhea_americana_data <- read.csv(here("DATA", "Processed data", "Rhea_americana_data.csv"))

Rhea_americana_data <- st_as_sf(
  Rhea_americana_data_nodup,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Graph the species
# Limit the extension to the occupied area
bbox <- st_bbox(Rhea_americana_data)

ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = Rhea_americana_data, color = "royalblue", size = 2) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = unique(Rhea_americana_data$species)
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "Rhea_americana_pampasmap.png"),
  width = 7,
  height = 6,
  dpi = 300
)

#-- Xanthopsar flavus
# Turn the data into sf
Xanthopsar_flavus_data <- read.csv(here("DATA", "Processed data", "Xanthopsar_flavus_data.csv"))

Xanthopsar_flavus_data <- st_as_sf(
  Xanthopsar_flavus_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Limit the extension to the occupied area
bbox <- st_bbox(Xanthopsar_flavus_data)

ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = Xanthopsar_flavus_data, color = "orangered", size = 2) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = unique(Xanthopsar_flavus_data$species)
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "Xanthopsar_flavus_pampasmap.png"),
  width = 7,
  height = 6,
  dpi = 300
)
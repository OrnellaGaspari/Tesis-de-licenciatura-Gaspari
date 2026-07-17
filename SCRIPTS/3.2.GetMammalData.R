## Name: 3.2.GetMammalData.R
## Author: Ornella Gaspari
## Goal: Transform mammal field data into the right format, merge with
##       GBIF data and generate a dataset for each species.

source(here("SCRIPTS", "0.libraries.R"))

#                           Ctenomys australis

# First I'll work with the sheet with recent data
recent <- read_excel(here("DATA", "Processed data", "Datos c. australis.xlsx"), sheet = "Posiciones nuevas C. australis")

recent_clean <- recent %>%
  mutate(
    Source = "Matias Mora / Planilla australis" 
  ) %>%
  select(
    species = Especie, 
    decimalLatitude = Latitud, 
    decimalLongitude = Longitud, 
    year = Año, 
    Source
  )

# Now I clean the sheet with older data
# It doesn´t have the best format so I have to be carefull when reading the excel 
old <- read_excel(here("DATA", "Processed data", "Datos c. australis.xlsx"), 
                     sheet = "Posiciones viejas C. australis ", 
                     skip = 2,
                     col_names = c("Especie", "Dia", "Year", "Lugar", 
                                   "Lat_G", "Lat_M", "Lat_S", 
                                   "Lon_G", "Lon_M", "Lon_S"))

old_clean <- old %>%
  # Convert to number
  mutate(across(c(Lat_G, Lat_M, Lat_S, Lon_G, Lon_M, Lon_S), as.numeric)) %>%
  
  # Filter the rows that don't have a date
  filter(!is.na(Year)) %>% 
  
  mutate(
    Source = "Matias Mora / Planilla australis vieja",
    # Convert DMS into decimal
    decimalLatitude = (Lat_G + (Lat_M / 60) + (Lat_S / 3600)) * -1,
    decimalLongitude = (Lon_G + (Lon_M / 60) + (Lon_S / 3600)) * -1
  ) %>%
  select(species = Especie, decimalLatitude, decimalLongitude, year = Year, Source)

# Merge datasets
C_australis_final <- bind_rows(recent_clean, old_clean)

write_csv(C_australis_final, here("DATA", "Processed data", "C_australis_field.csv"))

# Eco registros dataset
df_eco <- read.csv(here("DATA", "Processed data", "Ctenomys australis (Tucu Tucu del Sur_ Dune Tuco-tuco, ID Especie_ 5618).csv"), sep = ";")

# Process 
df_limpio <- df_eco %>%
  separate(Coordenadas, into = c("decimalLatitude", "decimalLongitude"), sep = ", ") %>%
  mutate(
    decimalLatitude = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude),
    year = year(dmy(Fecha)),
    Source = Fuente
  ) %>%
  
  # Select the ones we want to use
  select(
    species = Especie,
    decimalLatitude,
    decimalLongitude,
    year,
    Source
  )

# Save ecoregistros csv
write.csv(df_limpio, here("DATA", "Processed data", "C_australis_Ecorregistros.csv"), row.names = FALSE)

# Bind databases
df_ecorregistros <- read.csv(here("DATA", "Processed data", "C_australis_Ecorregistros.csv"))
df_field <- read.csv(here("DATA", "Processed data", "C_australis_field.csv"))

df_total <- bind_rows(df_field, df_ecorregistros)

# Clean
df_total_final <- df_total %>%
  # Delete points without coordinates
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
  # Delete duplicates
  distinct(species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE)

# Save merged database
write_csv(df_total_final, here("DATA", "Processed data", "C_australis_Merged_base.csv"))

# Draw the map
# Load the pampean region shapefile
pampa_sf <- st_read(here("DATA", "Processed data", "subregiones.shp"))

# Make sure the pampean region shapefile is on the same SRC as the data
pampa_sf <- st_transform(pampa_sf, crs = 4326)

# Load species data
data_C_australis <- read_csv(here("DATA", "Processed data", "C_australis_Merged_base.csv"))
C_australis_sf <- st_as_sf(
  data_C_australis,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)
  
# Base map
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Define the displayed area
bbox <- st_bbox(C_australis_sf)

#--- Draw the occurrences map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  geom_sf(data = C_australis_sf, color = "sienna4", size = 2, alpha = 0.7) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Occurrence records - Ctenomys australis",
    subtitle = unique(C_australis_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "C_australis_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)


#                           Ozotoceros bezoarticus

# Load GBIF data

gbif_data <- read_csv(here("DATA", "Processed data", "GBIF_mammals2_30+occurrences_speciesTest.csv"))

# Filter O bezoarticus data
df_bezoarticus_gbif <- gbif_data %>%
  filter(species == "Ozotoceros bezoarticus") %>%
  mutate(Source = "GBIF")

# Filter out duplicates
df_bezoarticus_gbif_nodup <- df_bezoarticus_gbif |> 
  dplyr::distinct()

# Saave data
write_csv(df_bezoarticus_gbif_nodup, here("DATA", "Processed data", "O_bezoarticus_GBIF_extraido.csv"))

# Points
data_O_bezoarticus <- read_csv(here("DATA", "Processed data", "O_bezoarticus_GBIF_extraido.csv"))
O_bezoarticus_sf <- st_as_sf(
  data_O_bezoarticus,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

mapview(O_bezoarticus_sf)

# Define displaed area
bbox <- st_bbox(O_bezoarticus_sf)

#--- Draw the occurrences map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  geom_sf(data = O_bezoarticus_sf, color = "gold3", size = 2, alpha = 0.7) +
    coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Occurrence records",
    subtitle = unique(O_bezoarticus_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "O_bezoarticus_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)

#                             Dasypus hybridus

# Load field data
D_hybridus_field <- read_excel(here("DATA", "Processed data", "D. s. hybridus BA con fecha.xlsx"))
D_hybridus_field_clean <- D_hybridus_field  %>%
  select(
    species = Species, 
    decimalLatitude = Lat, 
    decimalLongitude = Long, 
    year = Year,
    Source = Source
  )

# Save
write_csv(D_hybridus_field_clean, here("DATA", "Processed data", "D_hybridus_field.csv"))

# Load GBIF data
gbif_data <- read_csv(here("DATA", "Processed data", "GBIF_mammals1_30+occurrences_speciesTest.csv"))

# Filter Dasypus hybridus data

df_hybridus_gbif <- gbif_data %>%
  filter(species == "Dasypus hybridus") %>%
  mutate(Source = "GBIF")

# Save extracted data
write_csv(df_hybridus_gbif, here("DATA", "Processed data", "D_hybridus_GBIF_extraido.csv"))

# Eco registros dataset
df_eco <- read.csv(here("DATA", "Processed data", "Dasypus hybridus (Mulita Pampeana_ Southern Long-nosed Armadillo, ID Especie_ 537).csv"), sep = ";")

# Process 
df_limpio <- df_eco %>%
  separate(Coordenadas, into = c("decimalLatitude", "decimalLongitude"), sep = ", ") %>%
  mutate(
    decimalLatitude = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude),
    year = year(dmy(Fecha)),
    Source = Fuente
  ) %>%
  
  # Select the ones we want to use
  select(
    species = Especie,
    decimalLatitude,
    decimalLongitude,
    year,
    Source
  )

# Save ecoregistros csv
write.csv(df_limpio, here("DATA", "Processed data", "D_hybridus_Ecorregistros.csv"), row.names = FALSE)

# Read databases
df_field <- read_csv(here("DATA", "Processed data", "D_hybridus_field.csv"))
df_gbif <- read_csv(here("DATA", "Processed data", "D_hybridus_GBIF_extraido.csv"))
df_ecorregistros <- read_csv(here("DATA", "Processed data", "D_hybridus_Ecorregistros.csv"))

# Bind databases
df_total <- bind_rows(df_field, df_gbif, df_ecorregistros)

# Clean
df_total_final <- df_total %>%
  # Delete points without coordinates
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
  # Delete duplicates
  distinct(species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE)

# Save merged database
write_csv(df_total_final, here("DATA", "Processed data", "D_hybridus_Merged_base.csv"))

#--- Draw the occurrences map
data_D_hybridus<- read_csv(here("DATA", "Processed data", "D_hybridus_Merged_base.csv"))
D_hybridus_sf <- st_as_sf(
  data_D_hybridus,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Define the displayed area
bbox <- st_bbox(D_hybridus_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  geom_sf(data = D_hybridus_sf, color = "slategray4", size = 2, alpha = 0.7) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), 
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Occurrence records",
    subtitle = unique(D_hybridus_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "D_hybridus_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)



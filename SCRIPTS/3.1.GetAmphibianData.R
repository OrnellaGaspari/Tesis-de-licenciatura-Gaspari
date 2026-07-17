## Name: 3.1.GetAmphibianData.R
## Author: Ornella Gaspari
## Goal: Transform amphibian field data into the right format, merge with
##       GBIF data and generate a dataset for each species.

source(here("SCRIPTS", "0.libraries.R"))

#######                    Ceratophrys ornata

# Read the excel file
df <- read_excel(here("DATA", "Processed data", "C ornata database 2025.xlsx"), sheet = "Datos Gigante Año")

# Transform DMS into decimal
gms_a_decimal <- function(coord_str) {
  if (is.na(coord_str) || coord_str == "" || coord_str == "NA") return(NA)
  
  coord_str <- str_trim(coord_str)
  coord_str <- str_replace_all(coord_str, '""', '"')
  
  # If already decimal
  if (!str_detect(coord_str, "[°'\"SWNE]")) {
    return(as.numeric(str_replace(coord_str, ",", ".")))
  }
  
  # if DMS
  partes <- str_match(coord_str, "(\\d+)[°\\s]+(\\d+)'\\s*([\\d\\.]+)\"\\s*([NSEW])")
  if (all(is.na(partes))) return(NA)
  
  grados <- as.numeric(partes[2]); minutos <- as.numeric(partes[3])
  segundos <- as.numeric(partes[4]); hemi <- partes[5]
  
  res <- grados + (minutos / 60) + (segundos / 3600)
  if (hemi %in% c("S", "W", "O")) res <- res * -1
  return(res)
}

# Final processing
df_final <- df %>%
  mutate(
    species = "Ceratophrys ornata",
    Source = "Camila Deutsch"
  ) %>%
  rename(year = `Año`) %>%
  # Split the GD colmn and convert coordenates
  separate(GD, into = c("lat_raw", "lon_raw"), 
           sep = "(?<=[NSEW\\d]),\\s*|(?<=[NSEW])\\s+(?=\\d)", 
           extra = "merge", fill = "right") %>%
  rowwise() %>%
  mutate(
    decimalLatitude  = suppressWarnings(gms_a_decimal(lat_raw)),
    decimalLongitude = suppressWarnings(gms_a_decimal(lon_raw))
  ) %>%
  ungroup() %>%
  select(species, decimalLatitude, decimalLongitude, year, Source)

# Save the clean database
print(head(df_final))
write_csv(df_final, here("DATA", "Processed data", "C_ornata_Base_Final.csv"))

# Merge GBIF and field databases

# Load GBIF data
gbif_data <- read_csv(here("DATA", "Processed data", "GBIF_Amphibians2_30+occurrences_speciesTest.csv"))

# Filter Ceratophrys ornata data

df_ornata_gbif <- gbif_data %>%
  filter(species == "Ceratophrys ornata") %>%
  mutate(Source = "GBIF")

# Save extracted data
write_csv(df_ornata_gbif, here("DATA", "Processed data", "C_ornata_GBIF_extraido.csv"))

# Read both databases

df_field <- read_csv(here("DATA", "Processed data", "C_ornata_Base_Final.csv"))
df_gbif <- df_gbif <- read_csv(here("DATA", "Processed data", "C_ornata_GBIF_extraido.csv"))


# Bind databases
df_total <- bind_rows(df_field, df_gbif)

# Clean
df_total_final <- df_total %>%
  # Delete points without coordinates
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
  # Delete duplicates
  distinct(species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE)

# Save merged database
write_csv(df_total_final, here("DATA", "Processed data", "C_ornata_Merged_base.csv"))

#--- Draw the occurrences map 

# Read pampean region shapefile
pampa_sf <- st_read(here("DATA", "Processed data", "subregiones.shp"))

# Make sure the pampas SRC is the same as the database
pampa_sf <- st_transform(pampa_sf, crs = 4326)

# Load merged database
data_C_ornata <- read_csv(here("DATA", "Processed data", "C_ornata_Merged_base.csv"))
C_ornata_sf <- st_as_sf(
  data_C_ornata,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Base map
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Define the displayed area
bbox <- st_bbox(C_ornata_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = C_ornata_sf, color = "green2", size = 2, alpha = 0.7) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1), # Add margin (+/- 1 degree)
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  
  theme_minimal() +
  labs(
    title = "Occurrence records - Ceratophrys ornata",
    subtitle = unique(C_ornata_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = filename = here("OUTPUT", "C_ornata_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)

#######                    Boana pulchella

# Read the excel file
df <- read_excel(here("DATA", "Processed data", "Boana_pulchella con fechas 2016-2025.xlsx"))

# Get the right column names
field_clean <- df %>%
  mutate(
    Source = "Gabriela Agostini", 
    species = "Boana pulchella"
  ) %>%
  select(
    species,
    decimalLatitude = Y, 
    decimalLongitude = X, 
    year = Año, 
    Source,
  )

# Save the clean database
print(head(field_clean))
write_csv(field_clean, here("DATA", "Processed data", "B_pulchella_Base_Final.csv"))

# Merge GBIF and field databases

# Load GBIF data
gbif_data <- read_csv(here("DATA", "Processed data", "GBIF_Amphibians2_30+occurrences_speciesTest.csv"))

# Filter Boana pulchella data
df_pulchella_gbif <- gbif_data %>%
  filter(species == "Boana pulchella") %>%
  mutate(Source = "GBIF")

# Save extracted data
write_csv(df_pulchella_gbif, here("DATA", "Processed data", "B_pulchella_GBIF_extraido.csv"))

# Read both databases
df_field <- read_csv(here("DATA", "Processed data", "B_pulchella_Base_Final.csv"))
df_gbif  <- read_csv(here("DATA", "Processed data", "B_pulchella_GBIF_extraido.csv"))

# Bind databases
df_total <- bind_rows(df_field, df_gbif)

# Clean
df_total_final <- df_total %>%
  # Delete points without coordinates or year
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
  # Delete duplicates
  distinct(species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE)

# Save merged database
write_csv(df_total_final, here("DATA", "Processed data", "B_pulchella_Merged_base.csv"))

#--- Draw the occurrences map 

# Load merged database
data_B_pulchella <- read_csv(here("DATA", "Processed data", "B_pulchella_Merged_base.csv"))
B_pulchella_sf <- st_as_sf(
  data_B_pulchella,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Define the displayed area
bbox <- st_bbox(B_pulchella_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = B_pulchella_sf, color = "green4", size = 2, alpha = 0.7) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1),
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Occurrence records",
    subtitle = unique(B_pulchella_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "B_pulchella_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)

#######                     Rhinella dorbignyi

# Read the excel file
df <- read_excel(here("DATA", "Processed data", "Rhinella dorbignyi 2016 2025 Conserva.xlsx"))

# Get the right column names
field_clean <- df %>%
  # Separate the longitude and latitude data in two different columns
  separate_wider_delim(
    cols = `Coordenadas (lat, lon)`, 
    delim = ",", 
    names = c("decimalLatitude", "decimalLongitude")
  ) %>%
  # separate reads the data as text so I need to convert to number
  mutate(
    decimalLatitude  = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude),
    Source  = "Gabriela Agostini",
    species = "Rhinella dorbignyi"
  ) %>%
  select(
    species,
    decimalLatitude,
    decimalLongitude,
    year = Año,
    Source
  ) 

# Save the clean database
print(head(field_clean))
write_csv(field_clean, here("DATA", "Processed data", "R_dorbignyi_Base_Final.csv"))

# Merge GBIF and field databases

# Load GBIF data
gbif_data <- read_csv(here("DATA", "Processed data", "GBIF_Amphibians2_30+occurrences_speciesTest.csv"))

# Filter Rhinella dorbignyi data

# Rhinella dorbignyi used to be Rhinella fernandezae. Rhinella fernandezae still exists,
# but some group changed its name to Rhinella dorbignyi, I will only extract Rhinella
# dorbignyi data to avoid mistakes.
df_dorbignyi_gbif <- gbif_data %>%
  filter(species == "Rhinella dorbignyi") %>%
  mutate(Source = "GBIF")

# Save extracted data
write_csv(df_dorbignyi_gbif, here("DATA", "Processed data", "R_dorbignyi_GBIF_extraido.csv"))

# Read both databases
df_field <- read_csv(here("DATA", "Processed data", "R_dorbignyi_Base_Final.csv"))
df_gbif  <- read_csv(here("DATA", "Processed data", "R_dorbignyi_GBIF_extraido.csv"))

# Bind databases
df_total <- bind_rows(df_field, df_gbif)

# Clean
df_total_final <- df_total %>%
  # Delete points without coordinates or date
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
  # Delete duplicates
  distinct(species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE)

# Save merged database
write_csv(df_total_final, here("DATA", "Processed data", "R_dorbignyi_Merged_base.csv"))

#--- Draw the map 

# Load merged database
data_R_dorbignyi <- read_csv(here("DATA", "Processed data", "R_dorbignyi_Merged_base.csv"))
R_dorbignyi_sf <- st_as_sf(
  data_R_dorbignyi,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Define the displayed area
bbox <- st_bbox(R_dorbignyi_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = R_dorbignyi_sf, color = "olivedrab4", size = 2, alpha = 0.7) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1),
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Occurrence records",
    subtitle = unique(R_dorbignyi_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "R_dorbignyi_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)

#######             Odontophrynus asper

# Read the excel file
df <- read_excel(here("DATA", "Processed data", "Odonto 2016_2025.xlsx"))

# Get the right column names
field_clean <- df %>%
  # Separate the longitude and latitude data in two different columns
  separate_wider_delim(
    cols = `Coordenadas (lat, lon)`, 
    delim = ",", 
    names = c("decimalLatitude", "decimalLongitude")
  ) %>%
  # separate reads the data as text so I need to convert to number
  mutate(
    decimalLatitude  = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude),
    Source  = "Gabriela Agostini",
    species = "Odontophrynus asper"
  ) %>%
  select(
    species,
    decimalLatitude,
    decimalLongitude,
    year = Año,
    Source
  ) 

# Save the clean database
print(head(field_clean))
write_csv(field_clean, here("DATA", "Processed data", "O_asper_Base_Final.csv"))

# Merge GBIF and field databases
# There is no data for Odontophrynus asper in GBIF. However, there's data for
# Odontophrynus americanus which was the previous name of the species.

# Load GBIF data
gbif_data <- read_csv(here("DATA", "Processed data", "GBIF_Amphibians1_30+occurrences_speciesTest.csv"))

# Filter Odontophrynus asper data
df_asper_gbif <- gbif_data %>%
  filter(species == "Odontophrynus americanus") %>%
  mutate(Source  = "GBIF",
         species = "Odontophrynus asper")

# Save extracted data
write_csv(df_asper_gbif, here("DATA", "Processed data", "O_asper_GBIF_extraido.csv"))

# Read both databases
df_field <- read_csv(here("DATA", "Processed data", "O_asper_Base_Final.csv"))
df_gbif  <- read_csv(here("DATA", "Processed data", "O_asper_GBIF_extraido.csv"))

# Bind databases
df_total <- bind_rows(df_field, df_gbif)

# Clean
df_total_final <- df_total %>%
  # Delete points without coordinates or year
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(year)) %>%
  # Delete duplicates
  distinct(species, decimalLatitude, decimalLongitude, year, .keep_all = TRUE)

# Save merged database
write_csv(df_total_final, here("DATA", "Processed data", "O_asper_Merged_base.csv"))

#--- Draw the occurrences map 

# Load merged database
data_O_asper <- read_csv(here("DATA", "Processed data", "O_asper_Merged_base.csv"))
O_asper_sf <- st_as_sf(
  data_O_asper,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# Define the displayed area
bbox <- st_bbox(O_asper_sf)

# Draw the map
ggplot() +
  geom_sf(data = world_sf, fill = "gray95", color = "gray70") +
  geom_sf(data = O_asper_sf, color = "slategray", size = 2, alpha = 0.7) +
  geom_sf(data = pampa_sf, fill = "orange", color = "darkorange", alpha = 0.2, size = 0.8) +
  coord_sf(
    xlim = c(bbox["xmin"] - 1, bbox["xmax"] + 1),
    ylim = c(bbox["ymin"] - 1, bbox["ymax"] + 1),
    expand = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Occurrence records",
    subtitle = unique(O_asper_sf$species),
    x = "Longitud",
    y = "Latitud"
  )

# Save the map
ggsave(
  filename = here("OUTPUT", "O_asper_mapa_pampeano.png"),
  width = 8,
  height = 7,
  dpi = 300
)
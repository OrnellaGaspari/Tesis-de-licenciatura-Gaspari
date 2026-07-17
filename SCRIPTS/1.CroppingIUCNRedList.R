## Name: 1_croppingIUCN.R
## Author: Ornella Gaspari
## Goal: Crop IUCN Red List spatial ranges to the Argentine Pampas region
##       and extract species overlap area and conservation status.

source(here("SCRIPTS", "0.libraries.R"))

# Load study area shapefile
pampas_shp <- st_read(here("DATA", "Processed data", "IUCN data", "Pampa.shp"))

# Load the IUCN Red List spatial data
iucn_shp <- st_read(here("DATA", "Processed data", "IUCN data", "MAMMALS_PART1.shp"))

# Make sure both are in the same CRS
if (st_crs(pampas_shp) != st_crs(iucn_shp)) {
  pampas_shp <- st_transform(pampas_shp, st_crs(iucn_shp))
}

# Convert sf to SpatVector for terra operations
pampas_vect <- vect(pampas_shp)
iucn_vect <- vect(iucn_shp)

# Fix invalid geometries (if necessary)
#iucn_vect <- terra::makeValid(iucn_vect)
#pampas_vect <- terra::makeValid(pampas_vect)

# Perform intersection (clip IUCN ranges to Pampas)
iucn_crop <- terra::intersect(iucn_vect, pampas_vect)

# Calculate area of overlap (in hectares)
iucn_crop$overlap_ha <- terra::expanse(iucn_crop, unit = "ha")

#  Filter for native and extant populations
# (origin == 1 for native, presence == 1 for extant, per IUCN shapefile documentation)
if ("origin" %in% names(iucn_crop) && "presence" %in% names(iucn_crop)) {
  iucn_crop <- iucn_crop[iucn_crop$origin == 1 & iucn_crop$presence == 1, ]
} else {
  warning("origin/presence columns not found; skipping native/extant filtering.")
}

# Filter out marine mammals (keep only non-marine species)
if ("marine" %in% names(iucn_crop)) {
  iucn_crop <- iucn_crop[iucn_crop$marine == "false", ]
} else {
  warning("marine column not found; skipping marine filtering.")
}


# names(iucn_crop)
# head(iucn_crop)

# Build the data frame with scientific name, conservation status, and area. The common 
# name is not on the IUCN data
result_df <- data.frame(
  sci_name = iucn_crop$sci_name,
  category = iucn_crop$category,
  overlap_ha = iucn_crop$overlap_ha
)


# Remove duplicated species (keep the largest overlap if multiple polygons per species)
result_df <- result_df %>%
  group_by(sci_name, category) %>%
  summarise(overlap_ha = sum(overlap_ha), .groups = "drop")

# Save to CSV
write.csv(result_df, here("DATA", "Processed data", "IUCN data", "mammals1_status_area.csv"), row.names = FALSE)

######### MAMMALS PART 2
iucn_shp <- st_read(here("DATA", "Processed data", "IUCN data", "MAMMALS_PART2.shp"))

# Make sure both are in the same CRS
if (st_crs(pampas_shp) != st_crs(iucn_shp)) {
  pampas_shp <- st_transform(pampas_shp, st_crs(iucn_shp))
}

#Convert sf to SpatVector for terra operations
iucn_vect <- vect(iucn_shp)

#Crop
iucn_crop <- terra::intersect(iucn_vect, pampas_vect)

# Calculate area of overlap (in hectares)
iucn_crop$overlap_ha <- terra::expanse(iucn_crop, unit = "ha")

# Filter for native and extant populations
# (origin == 1 for native, presence == 1 for extant, per IUCN shapefile documentation)
if ("origin" %in% names(iucn_crop) && "presence" %in% names(iucn_crop)) {
  iucn_crop <- iucn_crop[iucn_crop$origin == 1 & iucn_crop$presence == 1, ]
} else {
  warning("origin/presence columns not found; skipping native/extant filtering.")
}

# Filter out marine mammals (keep only non-marine species)
if ("marine" %in% names(iucn_crop)) {
  iucn_crop <- iucn_crop[iucn_crop$marine == "false", ]
} else {
  warning("marine column not found; skipping marine filtering.")
}


# Build the data frame with scientific name, conservation status, and area. 
result_df <- data.frame(
  sci_name = iucn_crop$sci_name,
  category = iucn_crop$category,
  overlap_ha = iucn_crop$overlap_ha
)

# Remove duplicated species (keep the largest overlap if multiple polygons per species)
result_df <- result_df %>%
  group_by(sci_name, category) %>%
  summarise(overlap_ha = sum(overlap_ha), .groups = "drop")

# Save to CSV
write.csv(result_df, here("DATA", "Processed data", "IUCN data", "mammals2_status_area.csv"), row.names = FALSE)

######## AMPHIBIANS PART 1
iucn_shp <- st_read(here("DATA", "Processed data", "IUCN data", "AMPHIBIANS_PART1.shp"))

# Make sure both are in the same CRS
if (st_crs(pampas_shp) != st_crs(iucn_shp)) {
  pampas_shp <- st_transform(pampas_shp, st_crs(iucn_shp))
}

#Convert sf to SpatVector for terra operations
iucn_vect <- vect(iucn_shp)

#Crop
iucn_crop <- terra::intersect(iucn_vect, pampas_vect)

# Calculate area of overlap (in hectares)
iucn_crop$overlap_ha <- terra::expanse(iucn_crop, unit = "ha")

# Filter for native and extant populations
# (origin == 1 for native, presence == 1 for extant, per IUCN shapefile documentation)
if ("origin" %in% names(iucn_crop) && "presence" %in% names(iucn_crop)) {
  iucn_crop <- iucn_crop[iucn_crop$origin == 1 & iucn_crop$presence == 1, ]
} else {
  warning("origin/presence columns not found; skipping native/extant filtering.")
}

# Build the data frame with scientific name, conservation status, and area. 
result_df <- data.frame(
  sci_name = iucn_crop$sci_name,
  category = iucn_crop$category,
  overlap_ha = iucn_crop$overlap_ha
)

# Remove duplicated species (keep the largest overlap if multiple polygons per species)
result_df <- result_df %>%
  group_by(sci_name, category) %>%
  summarise(overlap_ha = sum(overlap_ha), .groups = "drop")

# Save to CSV
write.csv(result_df, here("DATA", "Processed data", "IUCN data", "Amphibians1_status_area.csv"), row.names = FALSE)

######## AMPHIBIANS PART 2
iucn_shp <- st_read(here("DATA", "Processed data", "IUCN data", "AMPHIBIANS_PART2.shp"))

# Make sure both are in the same CRS
if (st_crs(pampas_shp) != st_crs(iucn_shp)) {
  pampas_shp <- st_transform(pampas_shp, st_crs(iucn_shp))
}

#Convert sf to SpatVector for terra operations
iucn_vect <- vect(iucn_shp)

#Crop
iucn_crop <- terra::intersect(iucn_vect, pampas_vect)

# Calculate area of overlap (in hectares)
iucn_crop$overlap_ha <- terra::expanse(iucn_crop, unit = "ha")

# Filter for native and extant populations
# (origin == 1 for native, presence == 1 for extant, per IUCN shapefile documentation)
if ("origin" %in% names(iucn_crop) && "presence" %in% names(iucn_crop)) {
  iucn_crop <- iucn_crop[iucn_crop$origin == 1 & iucn_crop$presence == 1, ]
} else {
  warning("origin/presence columns not found; skipping native/extant filtering.")
}

# Build the data frame with scientific name, conservation status, and area. 
result_df <- data.frame(
  sci_name = iucn_crop$sci_name,
  category = iucn_crop$category,
  overlap_ha = iucn_crop$overlap_ha
)

# Remove duplicated species (keep the largest overlap if multiple polygons per species)
result_df <- result_df %>%
  group_by(sci_name, category) %>%
  summarise(overlap_ha = sum(overlap_ha), .groups = "drop")

# Save to CSV
write.csv(result_df, here("DATA", "Processed data", "IUCN data", "Amphibians2_status_area.csv"), row.names = FALSE)

####### BIRDS

# Step 1: Read study area
study_area <- st_read(here("DATA", "Processed data", "IUCN data", "Pampa.shp"))
bbox <- st_bbox(study_area)

# Convert bbox to terra extent
ext <- ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])

# Step 2: Read only birds in that bounding box
IUCN_birds <- vect(here("DATA", "Processed data", "IUCN data", "BOTW_2024_2.gpkg"),
                   extent = ext)
#this layer has spatial data but not much more info, such as the IUCN status, so I need 
# to include the information on the other layer

# Step 3: Project birds layer to match study area CRS 
IUCN_birds_proj <- project(IUCN_birds, crs(study_area))

# Step 4: Crop the IUCN list
cropped_Birds <- crop(IUCN_birds_proj, study_area)

# Step 5: Calculate area of overlap (in hectares)
cropped_Birds$overlap_ha <- expanse(cropped_Birds, unit = "ha")

# Step 6: Include the other layer

# For spatial layer (species ranges)
#names(terra::vect("BOTW_2024_2.gpkg", layer = "all_species"))

# For the attribute table (likely has IUCN status, English name, etc.)
#df_attr <- sf::st_read("BOTW_2024_2.gpkg", layer = "main_BL_HBW_Checklist_V9")
#names(df_attr)
#head(df_attr)

# Read spatial layer (species ranges)
ranges <- as.data.frame(cropped_Birds)

# Read attribute table 
df_attr <- sf::st_read(here("DATA", "Processed data", "IUCN data", "BOTW_2024_2.gpkg"), layer = "main_BL_HBW_Checklist_V9")

# Perform a left join by scientific name
df_joined <- left_join(ranges, df_attr, by = c("sci_name" = "ScientificName"))

# Check the names of the categories
names(df_joined)
#[1] "OBJECTID"                   "sisid"                      "sci_name"                  
#[4] "presence"                   "origin"                     "seasonal"                  
#[7] "source"                     "compiler"                   "data_sens"                 
#[10] "sens_comm"                  "dist_comm"                  "tax_comm"                  
#[13] "generalisd"                 "citation"                   "yrcompiled"                
#[16] "yrmodified"                 "version"                    "Seq"                       
#[19] "Order_"                     "FamilyName"                 "Family"                    
#[22] "Subfamily"                  "Tribe"                      "CommonName"                
#[25] "Authority"                  "IUCN_RedList_Category_2024" "Synonyms"                  
#[28] "AlternativeCommonNames"     "TaxonomicSources"           "SISRecID"                  
#[31] "SpcRecID"             

# Step 7: Filter for native and extant populations
if ("origin" %in% names(df_joined) && "presence" %in% names(df_joined)) {
  df_joined <- df_joined[df_joined$origin == 1 & df_joined$presence == 1, ]
} else {
  warning("origin/presence columns not found in birds data; skipping native/extant filtering.")
}

# Step 8: Summarise by scientific name
out_df <- df_joined %>%
  group_by(sci_name) %>%
  summarise(
    overlap_ha = sum(overlap_ha),
    IUCN_RedList_Category_2024 = first(IUCN_RedList_Category_2024),
    CommonName = first(CommonName),
    .groups = "drop"
  )

# Step 8: Save or use the result
write.csv(out_df, here("DATA", "Processed data", "IUCN data", "birds_pampas_overlap.csv"), row.names = FALSE)

# List of 50 species from "Guía de Bolsillo ARGENTINA Aves y Plantas de los Pastizales 
# Naturales del Cono Sur de Sudamérica Argentina, Brasil, Paraguay y Uruguay"

pampas_sci_names <- c(
  "Rhea americana", "Rhynchotus rufescens", "Nothura maculosa", "Bubulcus ibis",
  "Circus buffoni", "Syrigma sibilatrix", "Buteo swainsoni", "Buteogallus meridionalis",
  "Caracara plancus", "Milvago chimango", "Falco sparverius", "Vanellus chilensis",
  "Pluvialis dominica", "Bartramia longicauda", "Tryngites subruficollis", "Gallinago paraguaiae",
  "Calidris melanotos", "Myiopsitta monachus", "Guira guira", "Podager nacunda",
  "Athene cunicularia", "Asio flammeus", "Colaptes campestris", "Colaptes melanochloros",
  "Furnarius rufus", "Spartonoica maluroides", "Limnoctites rectirostris", "Xolmis irupero",
  "Xolmis dominicanus", "Alectrurus risora", "Progne tapera", "Tachycineta leucorrhoa",
  "Anthus correndera", "Anthus furcatus", "Cistothorus platensis", "Mimus saturninus",
  "Ammodramus humeralis", "Sicalis luteola", "Sicalis flaveola", "Zonotrichia capensis",
  "Emberizoides ypiranganus", "Embernagra platensis", "Sporophila palustris", "Sporophila cinnamomea",
  "Carduelis magellanica", "Dolichonyx oryzivorus", "Xanthopsar flavus", "Sturnella superciliaris",
  "Sturnella defilippii", "Pseudoleistes virescens"
)

# Filter your data frame to keep only Pampas grassland species
filtered_df <- out_df %>%
  filter(sci_name %in% pampas_sci_names)

write.csv(filtered_df, here("DATA", "Processed data", "IUCN data", "birds_filtered_with_guide.csv"), row.names = FALSE)

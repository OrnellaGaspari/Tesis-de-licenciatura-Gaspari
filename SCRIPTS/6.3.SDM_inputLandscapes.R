## Name: 6.3.SDM_inputLandscapes.R
## Author: Inês Silva, Ornella Gaspari
## Date: 22th March 2026
## Goal: Prepare multi-species single/ensemble training, current and future SDM 
##       input landscapes, manage bioclim stacks, VIF filtering, and Forest layer removal.

# load necessary libraries
source(here("SCRIPTS", "0.libraries.R"))

### PREPARING THE TRAINING LANDSCAPE -------------------------------------------

##########
# STEP 1 # Define target sps and import range shapefiles 
##########

target_sps <- c(# amphibians
  "Ceratophrys ornata", 
  "Boana pulchella",
  "Rhinella dorbignyi",
  "Odontophrynus asper",
  "Odontophrynus americanus",
  # birds
  "Embernagra platensis",
  "Pseudoleistes virescens",
  "Rhea americana",
  "Xanthopsar flavus",
  # mammals
  "Ozotoceros bezoarticus",
  "Dasypus hybridus",
  "Ctenomys australis")

# IUCN mammals shapefile
mammal_ranges1 <- st_read(here("DATA", "Processed data", "IUCN data", "MAMMALS_PART1.shp"))
mammal_ranges2 <- st_read(here("DATA", "Processed data", "IUCN data", "MAMMALS_PART2.shp"))
# combine mammals shapefiles
mammal_ranges <- bind_rows(mammal_ranges1, mammal_ranges2)
invisible(gc())

# BirdLife Shapefile
bird_ranges <- st_read(here("DATA", "Processed data", "IUCN data", "BOTW_2024_2.gpkg"))
invisible(gc())

# IUCN amphibian shapefile (two parts)
amphib_ranges1 <- st_read(here("DATA", "Processed data", "IUCN data", "AMPHIBIANS_PART1.shp"))
amphib_ranges2 <- st_read(here("DATA", "Processed data", "IUCN data", "AMPHIBIANS_PART2.shp"))
# combine amphibians shapefiles
amphib_ranges <- bind_rows(amphib_ranges1, amphib_ranges2)
invisible(gc())

# combine all taxa
all_ranges <- bind_rows(mammal_ranges, bird_ranges, amphib_ranges)

rm(mammal_ranges1, mammal_ranges2, mammal_ranges, bird_ranges, amphib_ranges1, amphib_ranges2, amphib_ranges)


# filter combined ranges for our target species
target_ranges <- all_ranges %>%
  filter(sci_name %in= target_sps) 

## fix problems between the geom and geometry columns
target_ranges <- target_ranges %>%
  dplyr::mutate(geometry = purrr::map2(geometry, geom,
                                       ~ if (st_is_empty(.x)) .y else .x
  ))
# restore sf geometry column
st_geometry(target_ranges) <- "geometry"
# remove old column
target_ranges <- target_ranges %>%
  dplyr::select(-geom) 

# re-set Coordinate system
st_crs(target_ranges) = 4326 #WGS 84

# ----------------------- ## CHECKING WHAT HAPPENED ## ----------------------- #
# plot each species range to visualise better
ggplot(target_ranges) +
  geom_sf() +
  facet_wrap(~sci_name) +
  theme_minimal() +
  theme(strip.text = element_text(face = "italic"))

# plot everyone on top of each other to see overlapping ranges
ggplot(target_ranges) +
  geom_sf(aes(fill = sci_name), color = NA, alpha = 0.6) + 
  theme_minimal()
# ----------------------- ## CHECKING WHAT HAPPENED ## ----------------------- #

##########
# STEP 2 # Project to add buffer of x km
##########

# project to South America equal-area projection
target_proj <- st_transform(target_ranges, 5880)  # South America Albers (m)

areas <- target_proj %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1000000) %>%
  group_by(sci_name) %>%
  summarise(total_area = sum(area_km2))

# print output 
areas

# get the largest of them all
largest_sp <- areas %>%
  arrange(desc(total_area)) %>%
  slice(1)

# print data for sps with largest range area
largest_sp

largest_geom <- target_proj %>%
  filter(sci_name == largest_sp$sci_name) %>%
  st_union()

rAmericana_bodySize <- 23 # Kg (= 23000 g, from SRIT Database)
rAmerican_maxDispersalDist <- 36.4*rAmericana_bodySize^0.14 # km

# buffer value
buffer_km <- rAmerican_maxDispersalDist * 3

# build the area with the buffer
largest_buffer <- st_buffer(largest_geom, dist = buffer_km * 1000)
invisible(gc())

# ----------------------- ## CHECKING WHAT HAPPENED ## ----------------------- #
plot(largest_buffer, col = "red")
plot(largest_geom, add = TRUE, col = "blue")
# ----------------------- ## CHECKING WHAT HAPPENED ## ----------------------- #

##########
# STEP 3 # Crop landscape by the extent of the largest range
##########

# load raster
myExpl_world <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_southAmerica_1km.tif"))

# match CRS
largest_buffer_vect <- vect(st_transform(largest_buffer, crs(myExpl_world)))

# crop by the extent (do not use mask here)
r_crop <- crop(myExpl_world, largest_buffer_vect)

# check result
plot(r_crop)

# save cropped raster
writeRaster(r_crop, here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped.tif"), overwrite = TRUE)

# load CHELSA bioclim variables (already cropped to training landscape extent)
bio03 <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "CHELSA_bio3_1981-2010_cropped.tif"))
bio15 <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "CHELSA_bio15_1981-2010_cropped.tif"))

# resample to match land-use raster exactly (same res, extent, origin)
exclude_NA <- r_crop[[1]]
bio03 <- resample(bio03, r_crop, method = "bilinear") |> mask(exclude_NA)
bio15 <- resample(bio15, r_crop, method = "bilinear") |> mask(exclude_NA)

# stack land-use + bioclim
r_crop_bioclim <- c(r_crop, bio03, bio15)
names(r_crop_bioclim) <- make.names(names(r_crop_bioclim))

# save
writeRaster(r_crop_bioclim,
            here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped_bioclim.tif"),
            overwrite = TRUE)

# check results 
r_orig <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_southAmerica_1km.tif"))
r_crop <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped.tif"))
r_bio  <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped_bioclim.tif"))

cat("Training checking cells:\nOrig:", ncell(r_orig), "| Crop:", ncell(r_crop), "| Bio:", ncell(r_bio), "\n")

plot(r_orig, main = "Original raster")
plot(r_crop, main = "Cropped raster")
plot(r_bio, main = "Cropped bioclim raster")

### PREPARING THE CURRENT LANDSCAPE -------------------------------------------

# load raster
myExpl_world <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_southAmerica_1km.tif"))

# load pampas shapefile
pampa_sf <- sf::st_read(here("DATA", "Processed data", "subregiones.shp"))

# crop + mask by pampas region
pampa_sf <- st_transform(pampa_sf, crs(myExpl_world))
r_crop <- crop(myExpl_world, pampa_sf)
r_mask <- mask(r_crop, pampa_sf)

# check result
plot(r_mask)

# save cropped raster
writeRaster(r_mask, here("DATA", "Landscapes", "ProcessedLandscapes", "CurrentLandscapes_2015_1km_pampa_cropped.tif"), overwrite = TRUE)

# load CHELSA bioclim variables and crop to pampas extent
bio03 <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "CHELSA_bio3_1981-2010_cropped.tif"))
bio15 <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "CHELSA_bio15_1981-2010_cropped.tif"))

bio03_pampa <- crop(bio03, pampa_sf) |> mask(pampa_sf)
bio15_pampa <- crop(bio15, pampa_sf) |> mask(pampa_sf)

# resample to match land-use raster exactly (same res, extent, origin)
bio03_pampa <- resample(bio03_pampa, r_mask, method = "bilinear")
bio15_pampa <- resample(bio15_pampa, r_mask, method = "bilinear")

# stack land-use + bioclim
r_mask_bioclim <- c(r_mask, bio03_pampa, bio15_pampa)
names(r_mask_bioclim) <- make.names(names(r_mask_bioclim))

# save
writeRaster(r_mask_bioclim,
            here("DATA", "Landscapes", "ProcessedLandscapes", "CurrentLandscapes_2015_1km_pampa_cropped_bioclim.tif"),
            overwrite = TRUE)

# check results 
r_orig <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_southAmerica_1km.tif"))
r_crop <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "CurrentLandscapes_2015_1km_pampa_cropped.tif"))
r_bio  <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "CurrentLandscapes_2015_1km_pampa_cropped_bioclim.tif"))

cat("Current checking cells:\nOrig:", ncell(r_orig), "| Crop:", ncell(r_crop), "| Bio:", ncell(r_bio), "\n")

plot(r_orig, main = "Original raster")
plot(r_crop, main = "Cropped raster")
plot(r_bio, main = "Cropped bioclim raster")

### PREPARING THE FUTURE LANDSCAPES --------------------------------------------

##########
# STEP 1 # Load future landscapes and Pampas region .shp
##########

# list all future landscapes
future_files <- list.files(here("DATA", "Landscapes", "ProcessedLandscapes"),
                           pattern = ".*_ssp.*.tif$",
                           full.names = TRUE)

#  Pampean Region shapefile
pampa_sf <- sf::st_read(here("DATA", "Processed data", "subregiones.shp"))

# match CRS
pampa_sf <- st_transform(pampa_sf, crs(myExpl_world))


##########
# STEP 2 # loop over all future landscapes to crop and aggregate to specific resolution
##########

for (future in future_files) {
  
  r <- rast(future)
  
  # crop + mask by pampas region
  r_crop <- crop(r, pampa_sf)
  r_mask <- mask(r_crop, pampa_sf)
  
  # load CHELSA bioclim variables and crop to pampas extent
  bio03 <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "CHELSA_bio3_1981-2010_cropped.tif"))
  bio15 <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "CHELSA_bio15_1981-2010_cropped.tif"))
  
  bio03_pampa <- crop(bio03, pampa_sf) |> mask(pampa_sf)
  bio15_pampa <- crop(bio15, pampa_sf) |> mask(pampa_sf)
  
  # resample to match land-use raster exactly
  bio03_pampa <- resample(bio03_pampa, r_mask, method = "bilinear")
  bio15_pampa <- resample(bio15_pampa, r_mask, method = "bilinear")
  
  # stack land-use + bioclim
  r_mask_bioclim <- c(r_mask, bio03_pampa, bio15_pampa)
  names(r_mask_bioclim) <- make.names(names(r_mask_bioclim))
  
  # build output name: replace "_southAmerica_" with "_bioclim_"
  base_name <- tools::file_path_sans_ext(basename(future))
  base_name <- gsub("_southAmerica_", "_bioclim_", base_name)
  outfile <- here("DATA", "Landscapes", "ProcessedLandscapes", paste0(base_name, ".tif"))
  
  # save
  writeRaster(r_mask_bioclim, outfile, overwrite = TRUE)
  
  message("Saved: ", outfile)
  invisible(gc())
}

######################### Test variables correlation ###########################
# Load training landscape
training_landscape <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped_bioclim.tif"))

# Take a random sample of raster cells
set.seed(123)
n_sample    <- 10000
cell_sample <- spatSample(training_landscape,
                          size   = n_sample,
                          method = "random",
                          na.rm  = TRUE,
                          as.df  = TRUE)

# Calculate VIF for all variables
vif_result <- vif(cell_sample)
print(vif_result)

# Stepwise VIF-based variable selection 
vif_selected <- vifstep(cell_sample, th = 10)
print(vif_selected)

# Show which variables were excluded 
excluded <- setdiff(names(cell_sample), vif_selected@results$Variables)
if (length(excluded) > 0) {
  message("Variables excluded due to high VIF (> 10):")
  message(paste(" ", excluded, collapse = "\n"))
} else {
  message("No variables excluded - all VIF values below threshold")
}

# Keep only selected variables
selected_vars <- vif_selected@results$Variables
message("Variables retained: ", paste(selected_vars, collapse = ", "))

############# Create final SDM input landscapes without forest #################

remove_forest_layer <- function(input_path) {
  
  # load raster
  r <- rast(input_path)
  
  # check if Forest layer exists
  if (!"Forest" %in% names(r)) {
    message("No 'Forest' layer found in: ", basename(input_path), " — skipping.")
    return(invisible(NULL))
  }
  
  # remove Forest layer
  r_nf <- subset(r, subset = names(r) != "Forest")
  
  # build output path: add _NF before the extension
  output_path <- sub("\\.tif$", "_NF.tif", input_path)
  
  # save
  terra::writeRaster(r_nf, output_path, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  message("Saved: ", basename(output_path),
          " | Layers: ", paste(names(r_nf), collapse = ", "))
  
  return(invisible(r_nf))
}

# Apply to all the landscapes
tif_files <- list.files(
  here("DATA", "Landscapes", "ProcessedLandscapes"),
  pattern = "(?i)bioclim.*\\.tif$",
  full.names = TRUE
)

lapply(tif_files, remove_forest_layer)
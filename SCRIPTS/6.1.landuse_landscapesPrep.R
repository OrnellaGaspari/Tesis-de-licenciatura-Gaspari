## Name: 6.1.landuse_landscapesPrep.R
## Authors: Jorinde-M. Rieger
## Date: March 31th 2026
## Goal: Reclassify, crop and process baseline and future LULC rasters to 1km 
##       target resolution for South America using custom classification functions.

# Settings & libraries -------------------------------------------
source(here("SCRIPTS", "0.libraries.R")) 
source(here("SCRIPTS", "5.customFunctions.R")) 

# Input variables -------------------------------------------
baseline_year <- 2015

# Define the target resolution
target_resolution <- 0.008333333 # 1km resolution

############################# FOLDER ORGANISATION ############################## 

# Define the file paths using relative tracking
basePathLandUse <- here("DATA", "Landscapes", "OriginalLandscapes")
if (!dir.exists(basePathLandUse)) {
  dir.create(basePathLandUse, recursive = TRUE)
}

outputPathLandscapes <- here("DATA", "Landscapes", "ProcessedLandscapes")
if (!dir.exists(outputPathLandscapes)) {
  dir.create(outputPathLandscapes, recursive = TRUE)
}

###############################################
# CREATE LAND-USE INPUT RASTER FOR LANDSCAPES #
###############################################

# Simplify and define ESA LULC types (39) to the 7 (SEALS) LULC types
LULC_Types <- 1:7
LULC_Types_names <- c(
  "Urban",
  "Cropland",
  "Pasture_Grassland",
  "Forest",
  "Nonforest_vegetation",
  "Water",
  "Barren_other")

# To reclassify LandUse classes create a matrix
value_to_landUse <- c(
  190, 1,  # Urban
  10, 2, # Cropland
  11, 2,
  12, 2,
  20, 2,
  30, 2,
  40, 2,
  130, 3,  # Pasture/Grassland
  50, 4, # Forest
  60, 4,
  61, 4,
  62, 4, 
  70, 4, 
  71, 4,
  72, 4, 
  80, 4,
  81, 4, 
  82, 4,
  90, 4, 
  100, 4,
  151, 4,
  160, 4,
  170, 4,
  110, 5, # Non-forest vegetation
  120, 5,
  121, 5,
  122, 5, 
  140, 5,
  150, 5,
  152, 5,
  153, 5,
  180, 5,
  210, 6, # Water
  200, 7, # Barren or Other
  201, 7,
  202, 7,
  220, 7)

landUsematrix <- matrix(value_to_landUse, ncol = 2, byrow = TRUE )

# read baseline year landscape
baseline_raster <- rast(here("DATA", "Landscapes", "OriginalLandscapes", "lulc_esa_seals7_2015.tif"))
plot(baseline_raster)

# crop the baseline raster to the area we want
continents <- sf::st_read(here("DATA", "continent", "Continents.shp"))
# filter for south america
south_america <- continents %>% 
  dplyr::filter(CONTINENT == "South America")
extent <- "southAmerica"
extent_crs <- sf::st_transform(south_america, crs = crs(baseline_raster))
extent_sp <- terra::vect(extent_crs)

# crop
baseline_raster_extent <- terra::crop(baseline_raster, south_america)
plot(baseline_raster_extent)


# Apply LULC type mapping and save the mapped baseline raster
mapped_baseline <- terra::classify(baseline_raster_extent,
                                   landUsematrix,
                                   include.lowest = TRUE)

# saved mapped LULC raster
output_file <- file.path(outputPathLandscapes,
                         paste0("MappedLandUse_base_", baseline_year, "_", gsub(" ", "_", extent), ".tif"))
terra::writeRaster(mapped_baseline,
                   output_file,
                   overwrite = TRUE)

# apply calculateRasterClass to the baseline raster to create raster classes for the LULC types
trainingLandscapesLandUse <- calculateRasterClass(
  OriginalRaster = mapped_baseline,
  extent = extent_sp, 
  target_resolution = target_resolution
)
gc()

# Replace LULC numbers with names
trainingLandscapesLandUse <- replace_numbers_with_names(trainingLandscapesLandUse,
                                                        LULC_Types,
                                                        LULC_Types_names)
gc()

# Save the processed baseline raster
output_file <- file.path(outputPathLandscapes,
                         paste0("trainingLandscapes_", baseline_year, "_", gsub(" ", "_", extent), "_1km.tif"))
terra::writeRaster(trainingLandscapesLandUse, output_file, overwrite = TRUE)
gc()

### PREPARING FUTURE LANDSCAPES ------------------------------------------------
# list all future landscape files
future_files <- list.files(here("DATA", "Landscapes", "OriginalLandscapes"),
                           pattern = ".*_ssp.*.tif$",
                           full.names = TRUE)

# load spatial data 
continents <- sf::st_read(here("DATA", "continent", "Continents.shp"))
south_america <- continents %>% 
  dplyr::filter(CONTINENT == "South America")
extent <- "southAmerica"
extent_crs <- sf::st_transform(south_america, crs = crs(baseline_raster))
extent_sp <- terra::vect(extent_crs)

# loop over future landscapes
for (future_file in future_files) {
  
  scenario_name <- tools::file_path_sans_ext(basename(future_file))
  message("Processing: ", scenario_name)
  
  future_raster <- rast(future_file)
  future_raster_extent <- terra::crop(future_raster, south_america)
  
  mapped_future <- terra::classify(future_raster_extent,
                                   landUsematrix,
                                   include.lowest = TRUE)
  
  # apply calculateRasterClass
  futureLandscapesLandUse <- calculateRasterClass(
    OriginalRaster = mapped_future,
    extent = extent_sp,
    target_resolution = target_resolution
  )
  gc()
  
  # replace LULC numbers with names
  futureLandscapesLandUse <- replace_numbers_with_names(futureLandscapesLandUse,
                                                        LULC_Types,
                                                        LULC_Types_names)
  gc()
  
  # save final processed raster (1km, named LULC classes)
  output_file <- file.path(outputPathLandscapes,
                           paste0("futureLandscape_",
                                  scenario_name, "_",
                                  gsub(" ", "_", extent), "_1km.tif"))
  terra::writeRaster(futureLandscapesLandUse, output_file, overwrite = TRUE)
  message("Saved: ", basename(output_file))
  
  rm(future_raster, future_raster_extent, mapped_future, futureLandscapesLandUse)
  invisible(gc())
}


### CHECK UP OF FUTURE RASTERS INTEGRITY --------------------------------------
# list all processed future landscape files to check differences
processed_future_files <- list.files(outputPathLandscapes,
                                     pattern = ".*_ssp.*.tif$",
                                     full.names = TRUE)

if (length(processed_future_files) >= 2) {
  r1 <- rast(processed_future_files[1])
  r2 <- rast(processed_future_files[2])
  message("Check R1 vs R2 identicality: ", all(values(r1) == values(r2), na.rm = TRUE))
  gc()
}
if (length(processed_future_files) >= 4) {
  r3 <- rast(processed_future_files[3])
  r4 <- rast(processed_future_files[4])
  message("Check R3 vs R4 identicality: ", all(values(r3) == values(r4), na.rm = TRUE))
  gc()
}
if (length(processed_future_files) >= 6) {
  r5 <- rast(processed_future_files[5])
  r6 <- rast(processed_future_files[6])
  message("Check R5 vs R6 identicality: ", all(values(r5) == values(r6), na.rm = TRUE))
  gc()
}
if (length(processed_future_files) >= 3) {
  message("Check R1 vs R3 identicality: ", all(values(r1) == values(r3), na.rm = TRUE))
  gc()
}
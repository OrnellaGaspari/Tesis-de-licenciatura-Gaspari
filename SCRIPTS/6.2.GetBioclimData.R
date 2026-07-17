## Name: 6.2.GetBioclimData.R
## Author: Ornella Gaspari
## Date: 18th March 2026
## Goal: Download bio03 and bio15 from CHELSA V2.1 via VSI virtual file system, 
##       crop them to the training landscape extension, and verify resolutions.

# Libraries
source(here("SCRIPTS", "0.libraries.R"))

# Reference raster (training landscape)
raster_ref <- here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped.tif")  

# Define output folder
output_dir <- here("DATA", "Landscapes", "OriginalLandscapes")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Read reference raster
ref <- terra::rast(raster_ref)

# Check CRS
ref_wgs84 <- if (!terra::same.crs(ref, "EPSG:4326")) {
  terra::project(ref, "EPSG:4326")
} else {
  ref
}

ext_recorte <- terra::ext(ref_wgs84)
cat("Cropping extension (WGS84):\n")
print(ext_recorte)

# Climatology 1981-2010 (bio03 y bio15 pre-calculated)
# Download from URL because rchelsa library had errors 
base_url_clim <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/"

vars_clim <- c("bio3", "bio15")

for (v in vars_clim) {
  url_global <- paste0(base_url_clim, "CHELSA_", v, "_1981-2010_V.2.1.tif")
  url_vsi    <- paste0("/vsicurl/", url_global)
  
  cat("  Reading:", url_global, "\n")
  
  r_global  <- terra::rast(url_vsi)
  r_crop    <- terra::crop(r_global, ext_recorte)
  
  output_file <- file.path(output_dir,
                           paste0("CHELSA_", v, "_1981-2010_cropped.tif"))
  terra::writeRaster(r_crop, output_file, overwrite = TRUE)
  cat("  Saved:", output_file, "\n\n")
}

# Check if the bioclim and the land-use rasters have the same resolution and number of cells
cat("--- INTEGRITY CHECKS ---\n")

r <- rast(file.path(output_dir, "CHELSA_bio3_1981-2010_cropped.tif"))
cat("Bio3 - Resolution:", res(r), "| N cells:", ncell(r), "\n")

s <- rast(file.path(output_dir, "CHELSA_bio15_1981-2010_cropped.tif"))
cat("Bio15 - Resolution:", res(s), "| N cells:", ncell(s), "\n")

cat("Reference - Resolution:", res(ref), "| N cells:", ncell(ref), "\n")
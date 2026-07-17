## Name: 7.SDM_run.R
## Author: Inês Silva, Ornella Gaspari
## Date: 23th March 2026
## Goal: Run single and ensemble Species Distribution Models (SDMs) for 
##       target species across multiple climate change scenarios (SSP1, SSP2, SSP5).

source(here("SCRIPTS", "0.libraries.R"))
source(here("SCRIPTS", "5.customFunctions.R"))

# species occurrence data
speciesData <- read.csv(here("DATA", "Processed data", "All_species_data.csv"))

# output_folder
output_folder <- here("OUTPUT", "SDMOutputs no Forest")

# maxent_source
maxent_source <- here("SDMtesting", "maxent", "maxent.jar")

# myExpl_full - training landscape
myExpl_full <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "trainingLandscapes_2015_1km_cropped_bioclim_NF.tif"))

# myExpl_Current - current conditions
myExplCurrent <- rast(here("DATA", "Landscapes", "ProcessedLandscapes", "CurrentLandscapes_2015_1km_pampa_cropped_bioclim_NF.tif"))

# Future conditions SSP2
myExplFuture_ssp2 <- list(
  # ssp2 - 2050
  rast(here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp45_ssp2_2050_no_policy_bioclim_1km_NF.tif")),
  # ssp2 - 2100
  rast(here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp45_ssp2_2100_no_policy_bioclim_1km_NF.tif")))
# named list
names(myExplFuture_ssp2) <- c("ssp2_2050", "ssp2_2100")

# Future conditions SSP1
myExplFuture_ssp1 <- list(
  # ssp1 - 2050
  rast(here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp26_ssp1_2050_no_policy_bioclim_1km_NF.tif")),
  # ssp1 - 2100
  rast(here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp26_ssp1_2100_no_policy_bioclim_1km_NF.tif")))
# named list 
names(myExplFuture_ssp1) <- c("ssp1_2050", "ssp1_2100")

# Future conditions SSP5
myExplFuture_ssp5 <- list(
  # ssp5 - 2050
  rast(here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp85_ssp5_2050_no_policy_bioclim_1km_NF.tif")),
  # ssp5 - 2100
  rast(here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp85_ssp5_2100_no_policy_bioclim_1km_NF.tif")))
# named list 
names(myExplFuture_ssp5) <- c("ssp5_2050", "ssp5_2100")

###################################
# APPPLYING THE SDM MULTI-SPECIES #
###################################
# Target species. First I only run the sps that use a 0.7 TSS threshold.
targetsps <- c("Boana pulchella", "Ceratophrys ornata", "Odontophrynus asper", "Rhinella dorbignyi", "Pseudoleistes virescens", "Xanthopsar flavus", "Dasypus hybridus")

# Run for SSP2
species_times <- list()   
SDM_NatPoke <- lapply(targetsps, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleMultiSpecies(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = list(myExplCurrent),
      myExplFuture = myExplFuture_ssp2,
      extent = "pampa",
      output_folder = output_folder, 
      maxent_source = maxent_source, 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

# Run for SSP1
species_times <- list()   
SDM_NatPoke <- lapply(targetsps, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleMultiSpecies(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = list(myExplCurrent),
      myExplFuture = myExplFuture_ssp1,
      extent = "pampa",
      output_folder = output_folder, 
      maxent_source = maxent_source, 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

# Run for SSP5
species_times <- list()   
SDM_NatPoke <- lapply(targetsps, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleMultiSpecies(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = list(myExplCurrent),
      myExplFuture = myExplFuture_ssp5,
      extent = "pampa",
      output_folder = output_folder, 
      maxent_source = maxent_source, 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

###### RUN FOR O. bezoarticus AND E. platensis

# Target species
targetsps <- c("Embernagra platensis", "Ozotoceros bezoarticus")

# Run for SSP2
species_times <- list()   
SDM_NatPoke <- lapply(targetsps, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleObezoarticus(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = list(myExplCurrent),
      myExplFuture = myExplFuture_ssp2,
      extent = "pampa",
      output_folder = output_folder, 
      maxent_source = maxent_source, 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

# Run for SSP1
species_times <- list()   
SDM_NatPoke <- lapply(targetsps, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleObezoarticus(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = list(myExplCurrent),
      myExplFuture = myExplFuture_ssp1,
      extent = "pampa",
      output_folder = output_folder, 
      maxent_source = maxent_source, 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

# Run for SSP5
species_times <- list()   
SDM_NatPoke <- lapply(targetsps, function(sp) {
  tryCatch({
    # measure start time
    start_time <- Sys.time()
    
    result <- SDMensembleObezoarticus(
      targetSpecies = sp,
      speciesData = speciesData,
      myExpl_full = myExpl_full,
      myExplCurrent = list(myExplCurrent),
      myExplFuture = myExplFuture_ssp5,
      extent = "pampa",
      output_folder = output_folder, 
      maxent_source = maxent_source, 
      ncoresToUse = 6)
    
    # measure time end
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "mins")
    
    message("✅ Finished ", sp, " in ", round(elapsed, 2), " minutes")
    
    # store timing
    species_times[[sp]] <<- elapsed
    
    return(result)
    
  }, error = function(e) {
    message(paste("⚠️ Skipping", sp, "due to error:", e$message))
    species_times[[sp]] <<- NA   # store NA if failed
    return(NULL)
  })
})

###### RUN FOR R. americana

# Run SSP2
result <- SDMensembleRamericana(
  targetSpecies = c("Rhea americana"),
  speciesData = speciesData,
  myExpl_full = myExpl_full,
  myExplCurrent = list(myExplCurrent),
  myExplFuture = myExplFuture_ssp2,
  extent = "pampa",
  output_folder = output_folder, 
  maxent_source = maxent_source, 
  ncoresToUse = 6)

# Run SSP1
result <- SDMensembleRamericana(
  targetSpecies = c("Rhea americana"),
  speciesData = speciesData,
  myExpl_full = myExpl_full,
  myExplCurrent = list(myExplCurrent),
  myExplFuture = myExplFuture_ssp1,
  extent = "pampa",
  output_folder = output_folder, 
  maxent_source = maxent_source, 
  ncoresToUse = 6)

# Run SSP5
result <- SDMensembleRamericana(
  targetSpecies = c("Rhea americana"),
  speciesData = speciesData,
  myExpl_full = myExpl_full,
  myExplCurrent = list(myExplCurrent),
  myExplFuture = myExplFuture_ssp5,
  extent = "pampa",
  output_folder = output_folder, 
  maxent_source = maxent_source, 
  ncoresToUse = 6)
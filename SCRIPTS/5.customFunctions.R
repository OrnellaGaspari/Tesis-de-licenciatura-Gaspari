## Name: 5.customFunctions.R
## Authors: Inês Silva, Ornella Gaspari
## Goal: Loads all developed customised functions for raster classification, 
##       naming LULC types, and executing multi-species single/ensemble SDMs.

# Function to create binary maps and classes of land-use types
calculateRasterClass <- function(OriginalRaster, extent, target_resolution) {
  # Crop and mask the raster to the biome's boundary
  raster <- mask(crop(OriginalRaster, extent), extent)
  
  # Define the unique land-use classes and remove NAs
  land_use_classes <- terra::freq(raster)[,2]
  land_use_classes <- na.omit(land_use_classes)
  
  # Function to create binary raster for each land-use class
  create_binary_raster <- function(raster, land_use_class) {
    binary_raster <- app(raster, fun = function(x) {
      ifelse(x == land_use_class, 1, 0)
    })
    return(binary_raster)
  }
  
  # Create a list to store binary rasters
  binary_rasters <- list()
  
  # Loop through each land-use class and create binary rasters
  for (class in land_use_classes) {
    binary_rasters[[as.character(class)]] <- create_binary_raster(raster, class)
  }
  
  # Calculate the aggregation factor based on the target resolution
  input_resolution <- res(raster)[1]  # Assuming square cells, take the resolution of the first dimension
  aggregation_factor <- round(target_resolution / input_resolution)
  
  # Aggregate each binary raster by aggregation factor
  aggregated_rasters <- list()
  for (class in names(binary_rasters)) {
    aggregated_raster <- aggregate(binary_rasters[[class]], fact = aggregation_factor, fun = function(x) sum(x > 0, na.rm = TRUE)/length(x))
    masked_raster <- terra::mask(terra::crop(aggregated_raster, extent), extent)
    aggregated_rasters[[class]] <- masked_raster
  }
  
  # Convert the list of rasters to a SpatRaster stack
  aggregated_rasters_stack <- terra::rast(aggregated_rasters)
  return(aggregated_rasters_stack)
}

# Function to replace numbers of LULC Types with names (for baseline raster)
replace_numbers_with_names <- function(raster_list, types, names) {
  # Replace the names of the raster layers with the corresponding land-use names
  names(raster_list) <- names[match(names(raster_list), types)]
  return(raster_list)
}

# Function to create the SDM response plots
# Plots response curves only for models that passed the TSS threshold
save_response_plot <- function(bm.out, bm.em, sp.name, output_folder,
                               tss_threshold = 0.7) { # threshold 0.5 for R. americana; 0.6 for O. bezoarticus and E. platensis
  
  # --- Individual models: filter by TSS threshold ---
  eval_ind     <- get_evaluations(bm.out)
  valid_models <- eval_ind %>%
    filter(metric.eval == "TSS",
           validation  >= tss_threshold) %>%
    pull(full.name)
  
  if (length(valid_models) == 0) {
    message("  > No models passed TSS threshold for ", sp.name, ". Skipping plot.")
    return(invisible(NULL))
  }
  
  rp_ind <- bm_PlotResponseCurves(
    bm.out        = bm.out,
    models.chosen = valid_models,
    fixed.var     = 'mean',
    do.plot       = FALSE,
    do.progress   = FALSE
  )
  
  # average response curve per algorithm across all valid runs
  tab_ind_avg <- rp_ind$tab %>%
    mutate(
      pred.name = as.character(pred.name),
      algo      = pred.name %>% strsplit('_') %>% sapply(function(x) tail(x, 1))
    ) %>%
    group_by(algo, expl.name, expl.val) %>%
    summarise(pred.val = mean(pred.val, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "individual")
  
  # --- Ensemble model ---
  rp_em <- bm_PlotResponseCurves(
    bm.out        = bm.em,
    models.chosen = get_built_models(bm.em),
    fixed.var     = 'mean',
    do.plot       = FALSE,
    do.progress   = FALSE
  )
  
  tab_em <- rp_em$tab %>%
    mutate(
      pred.name = as.character(pred.name),
      algo      = "Ensemble",
      source    = "ensemble"
    )
  
  # --- Combine both datasets ---
  tab_all <- bind_rows(tab_ind_avg, tab_em)
  
  # --- Plot ---
  p <- ggplot() +
    # one averaged line per algorithm
    geom_line(
      data = tab_all %>% filter(source == "individual"),
      aes(x = expl.val, y = pred.val, colour = algo, group = algo),
      linewidth = 0.9
    ) +
    # ensemble mean as thick dashed grey line
    geom_line(
      data = tab_all %>% filter(source == "ensemble"),
      aes(x = expl.val, y = pred.val, group = pred.name),
      colour    = "grey30",
      linewidth = 1.2,
      linetype  = "dashed"
    ) +
    facet_wrap(~ expl.name, scales = 'free_x') +
    scale_y_continuous(limits = c(0, 1), name = 'Probability of occurrence') +
    scale_x_continuous(name = '') +
    scale_colour_brewer(palette = 'Set1', name = 'Model type') +
    labs(
      title    = paste('Response curves -', sp.name),
      subtitle = paste0('Algorithm means (TSS >= ', tss_threshold,
                        ') + ensemble mean (dashed)')
    ) +
    theme_bw() +
    theme(
      legend.position  = 'bottom',
      strip.background = element_rect(fill = 'grey90')
    )
  
  ggsave(
    filename = file.path(output_folder,
                         paste0(sp.name, "_response_curves_combined.png")),
    plot   = p,
    width  = 12, height = 7, dpi = 300
  )
  
  message("  > Response plot saved: combined")
}

##############################
# Multi species SDM function #
##############################

## This function runs multiple SDMs and saves results for multiple species at a time

SDMensembleMultiSpecies <- function(targetSpecies, # vector of target species names
                                    speciesData, # target species occurrences file from GBIF
                                    myExpl_full, # training landscape (whole world)
                                    myExplCurrent, # current environment landscape (cropped to biome)
                                    myExplFuture, # future environment landscapes (cropped to biome)
                                    extent, # extent name for files' names (e.g. tropical OR boreal)
                                    output_folder, # folder path to save outputs
                                    maxent_source, # path to maxent.jar file
                                    ncoresToUse # n cores to use in parallelization jobs
) {
  
  ##########
  # STEP 1 # Setup & Folder Prep
  ##########
  
  # create output folder
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  if (file.exists(maxent_source)) {
    file.copy(from = maxent_source,
              to = file.path(output_folder, "maxent.jar"),
              overwrite = TRUE)
  } else {
    warning("maxent.jar not found at: ", maxent_source,
            "\nDownload it or place it in this folder before running.")
  }
  
  invisible(gc())
  
  ##########
  # STEP 2 # Filter Occurrence Data & Prepare Presence/Pseudo-absence data
  ##########
  
  # print starting message
  message(paste0("Starting for ", targetSpecies))
  
  # Select single species data
  DataSingleSpecies <- speciesData %>%
    dplyr::filter(species == !!targetSpecies)
  invisible(gc())
  
  # Remove NAs and filter out records older than 2015
  DataSingleSpecies <- DataSingleSpecies %>%
    drop_na(decimalLongitude,decimalLatitude, year)
  invisible(gc())
  
  # assign cell IDs to each occurrence based on myExpl raster
  cellValues <- terra::extract(
    myExpl_full,
    cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  cellValues$cell <- terra::cellFromXY(myExpl_full,
                                       cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  DataSingleSpecies <- cbind(DataSingleSpecies, cellValues)
  
  # keep one record per cell (to avoid biased occ points)
  DataSingleSpecies_unique <- DataSingleSpecies %>%
    group_by(cell) %>%
    slice_max(year, with_ties = FALSE) %>%  # or slice_head(n = 1) for the first
    ungroup() %>%
    dplyr::filter(complete.cases(.))  # biomod excludes all cells that do not have any data
  
  # --- Spatial thinning with spThin (3km minimum distance) ---
  set.seed(123) 
  message(paste0("  Spatial thinning for ", targetSpecies, 
                 " | records before: ", nrow(DataSingleSpecies_unique)))
  
  thinned <- spThin::thin(
    loc.data   = DataSingleSpecies_unique,
    lat.col    = "decimalLatitude",
    long.col   = "decimalLongitude",
    spec.col   = "species",
    thin.par   = 3,          # minimum distance in km
    reps       = 100,         # number of repetitions
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    verbose     = FALSE
  )
  
  # keep the repetition that retained the most records
  best_rep <- which.max(sapply(thinned, nrow))
  thinned_coords <- thinned[[best_rep]]
  
  # merge thinned coordinates back with the original data to recover all columns
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    dplyr::inner_join(thinned_coords,
                      by = c("decimalLongitude" = "Longitude",
                             "decimalLatitude"  = "Latitude"))
  
  message(paste0("  Records after thinning: ", nrow(DataSingleSpecies_unique)))
  
  # subset occurrences
  n_sample <- min(300, nrow(DataSingleSpecies_unique))
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    slice_sample(n = n_sample) %>%
    as.data.frame()
  
  # format species occurence data (presence only data)
  myResp <- as.numeric(DataSingleSpecies_unique$species == targetSpecies)
  myRespXY <- DataSingleSpecies_unique[, c("decimalLongitude", "decimalLatitude")]
  
  n.pres <- sum(myResp == 1)
  nb.PA <- c(n.pres, n.pres, n.pres, 10000, 10000, 10000) # number of pseudo-absences per set
  
  # format input data (with initial pseudo-absences set) 
  myBiomodData.PA <- BIOMOD_FormatingData(
    resp.var = myResp,
    expl.var = myExpl_full,
    resp.xy = myRespXY,
    resp.name = targetSpecies,
    PA.nb.rep = 6, 
    PA.nb.absences = nb.PA,  
    PA.strategy = 'random', 
    na.rm = TRUE, 
    filter.raster = FALSE) 
  
  # print a message
  message(paste0("Data formatting done for ", targetSpecies))
  
  # save presence points as .csv 
  presence_points <- myBiomodData.PA@coord[myBiomodData.PA@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  presence_df$type <- "Presence Points"
  presence_df$species <- as.character(targetSpecies)
  presence_df <- na.omit(presence_df)
  
  write.csv(
    presence_df,
    file = file.path(output_folder, paste0("PresencePoints_", targetSpecies, "_", extent, ".csv")),
    row.names = FALSE)
  
  ##########
  # STEP 3 # Run the models 
  ##########
  
  # selection of models and pseudo-absences set
  models.pa.list <- list(
    RF = c("PA1", "PA2", "PA3"), 
    XGBOOST = c("PA1", "PA2", "PA3"), 
    ANN = c("PA1", "PA2", "PA3"), 
    MAXNET = c("PA4", "PA5", "PA6") 
  )
  
  # run single models
  myBiomodModelOut <- BIOMOD_Modeling(
    bm.format = myBiomodData.PA,
    modeling.id = paste0("Model_", targetSpecies),
    models = c("RF", "XGBOOST", "ANN", "MAXNET"), 
    models.pa = models.pa.list,
    CV.strategy = "random",
    CV.nb.rep = 5, 
    CV.perc = 0.7, 
    OPT.strategy = 'bigboss',
    prevalence = 0.5, 
    metric.eval = c("TSS", "ROC"), 
    var.import = 3, 
    nb.cpu = ncoresToUse, 
    do.progress = TRUE)
  
  # print progress message
  message(paste0("Single models completed for ", targetSpecies))
  
  # get evaluation scores & variable importance
  eval_scores <- get_evaluations(myBiomodModelOut)
  eval_scores$species <- targetSpecies  
  var_importance <- get_variables_importance(myBiomodModelOut)
  var_importance$species <- targetSpecies  
  
  # save evaluation scores and variable importance to files
  write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", targetSpecies, "_", extent, "_", ".csv")), row.names = FALSE)
  
  invisible(gc(rm(myBiomodData.PA)))
  invisible(gc(rm(eval_scores, var_importance)))  
  
  ##########
  # STEP 4 # Project single models
  ##########
  
  # project single models
  myBiomodProj <- lapply(myExplCurrent, function(env_raster) { 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = 'Current',
      new.env = env_raster,
      models.chosen ='all',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  })    
  
  # print progress message
  message(paste0("Single models projections done for ", targetSpecies))
  
  ##########
  # STEP 5 # Do ensemble models
  ##########
  
  # Model ensemble models
  myBiomodEM <- BIOMOD_EnsembleModeling(
    bm.mod = myBiomodModelOut,
    models.chosen ='all',
    em.by ='all',
    em.algo = c('EMmean'),
    metric.select = c('TSS'),
    metric.select.thresh = c(0.7), 
    metric.eval = c('TSS','ROC'),
    nb.cpu = ncoresToUse, 
    do.progress = TRUE,
    var.import = 3,
    EMci.alpha = 0.05)
  
  # print progress message
  message(paste0("Ensemble model done for ", targetSpecies))
  
  # get evaluation scores & variable importance for ensemble models
  eval_scoresEM <- get_evaluations(myBiomodEM)
  eval_scoresEM$species <- targetSpecies  
  
  var_importanceEM <- get_variables_importance(myBiomodEM)
  var_importanceEM$species <- targetSpecies  
  
  # Save evaluation scores and variable importance to files
  write.csv(eval_scoresEM, file = file.path(output_folder, paste0("EvalScoresEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importanceEM, file = file.path(output_folder, paste0("VarImportanceEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  
  invisible(gc(rm(eval_scoresEM, var_importanceEM)))  
  
  # --- Response plots ---
  save_response_plot(myBiomodModelOut, myBiomodEM, targetSpecies, output_folder, tss_threshold = 0.7)  
  invisible(gc())  
  
  ##########
  # STEP 6 # Project ensemble models for current conditions
  ##########
  
  # Project ensemble models (from single projections) on current conditions
  myBiomodEMProj <- lapply(myBiomodProj, function(Proj) { 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = Proj,
      models.chosen ='all',
      metric.binary ='all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = TRUE
    )
  }) 
  
  # print progress message
  message(paste0("Ensemble models' projections for current conditions done for ", targetSpecies))
  
  ##########
  # STEP 7 # Project single and ensemble models to future conditions 
  ##########
  
  # Project single models onto future conditions
  myBiomodProjectionFuture <- lapply(myExplFuture, function(future_raster) { 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = "Future",
      new.env = future_raster,
      models.chosen = 'all',
      metric.binary = 'TSS',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  }) 
  names(myBiomodProjectionFuture) <- names(myExplFuture)
  
  # print progress message
  message(paste0("Single models' projection for future scenarios done for ", targetSpecies))
  
  # Project ensemble-models projections on future variables
  myBiomodEF <- lapply(myBiomodProjectionFuture, function(future_proj) { 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = future_proj, 
      models.chosen = 'all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = "xz"
    )
  })
  
  # print progress message
  message(paste0("Ensemble models' projections for future scenarios done for ", targetSpecies))
  
  ##########
  # STEP 8 # Save ensemble for current and future conditions rasters for each scenario
  ##########
  
  # print progress message
  message(paste0("Saving output rasters for ", targetSpecies))
  
  # Get evaluation results to extract threshold
  evals <- get_evaluations(myBiomodEM)
  th_TSS <- evals$cutoff[evals$metric.eval == "TSS"]
  
  ## Current Conditions Raster ##
  EMcurrent <- get_predictions(myBiomodEMProj[[1]], as.data.frame = FALSE)
  
  # save normal suitability (continuous) raster
  EMcurrent_filename <- file.path(output_folder, paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
  terra::writeRaster(EMcurrent, EMcurrent_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  # save binary (converted) raster
  bin_rasters <- bm_BinaryTransformation(data = EMcurrent, threshold = th_TSS, do.filtering = FALSE)
  names(bin_rasters) <- paste0("ssp126_2030", names(bin_rasters), "_TSSbin")
  bin_filename <- file.path(output_folder, paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_binary.tif"))
  terra::writeRaster(bin_rasters, bin_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  # clean up to save memory
  invisible(gc(rm(EMcurrent, EMcurrent_filename, bin_rasters, bin_filename)))  
  invisible(gc())
  
  ## Future Conditions Rasters ##
  for(sc in names(myBiomodEF)) {
    
    message(paste("Saving scenario:", sc))
    
    EFproj <- myBiomodEF[[sc]]
    
    # Continuous raster
    cont_rasters <- get_predictions(EFproj, as.data.frame = FALSE)
    names(cont_rasters) <- paste0(sc, "_", names(cont_rasters))
    cont_filename <- file.path(output_folder, paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
    terra::writeRaster(cont_rasters, cont_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2","BIGTIFF=YES"))
    
    # Binary raster
    bin_rasters <- bm_BinaryTransformation(data = cont_rasters, threshold = th_TSS, do.filtering = FALSE)
    names(bin_rasters) <- paste0(sc, "_", names(bin_rasters), "_TSSbin")
    bin_filename <- file.path(output_folder, paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_binary.tif"))
    terra::writeRaster(bin_rasters, bin_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2","BIGTIFF=YES"))
    
    rm(EFproj, cont_rasters, bin_rasters)
    invisible(gc())
  }
}

################################################################################
#   Function with a lower TSS threshold for O. bezoarticus and E. platensis
################################################################################
SDMensembleObezoarticus <- function(targetSpecies, # vector of target species names
                                    speciesData, # target species occurrences file from GBIF
                                    myExpl_full, # training landscape (whole world)
                                    myExplCurrent, # current environment landscape (cropped to biome)
                                    myExplFuture, # future environment landscapes (cropped to biome)
                                    extent, # extent name for files' names (e.g. tropical OR boreal)
                                    output_folder, # folder path to save outputs
                                    maxent_source, # path to maxent.jar file
                                    ncoresToUse # n cores to use in parallelization jobs
) {
  
  ##########
  # STEP 1 # Setup & Folder Prep
  ##########
  
  # create output folder
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  if (file.exists(maxent_source)) {
    file.copy(from = maxent_source,
              to = file.path(output_folder, "maxent.jar"),
              overwrite = TRUE)
  } else {
    warning("maxent.jar not found at: ", maxent_source,
            "\nDownload it or place it in this folder before running.")
  }
  
  invisible(gc())
  
  ##########
  # STEP 2 # Filter Occurrence Data & Prepare Presence/Pseudo-absence data
  ##########
  
  # print starting message
  message(paste0("Starting for ", targetSpecies))
  
  # Select single species data
  DataSingleSpecies <- speciesData %>%
    dplyr::filter(species == !!targetSpecies)
  invisible(gc())
  
  # Remove NAs and filter out records older than 2015
  DataSingleSpecies <- DataSingleSpecies %>%
    drop_na(decimalLongitude,decimalLatitude, year)
  invisible(gc())
  
  # assign cell IDs to each occurrence based on myExpl raster
  cellValues <- terra::extract(
    myExpl_full,
    cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  cellValues$cell <- terra::cellFromXY(myExpl_full,
                                       cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  DataSingleSpecies <- cbind(DataSingleSpecies, cellValues)
  
  # keep one record per cell (to avoid biased occ points)
  DataSingleSpecies_unique <- DataSingleSpecies %>%
    group_by(cell) %>%
    slice_max(year, with_ties = FALSE) %>%  # or slice_head(n = 1) for the first
    ungroup() %>%
    dplyr::filter(complete.cases(.))  
  
  # --- Spatial thinning with spThin (3km minimum distance) ---
  set.seed(123) 
  message(paste0("  Spatial thinning for ", targetSpecies, 
                 " | records before: ", nrow(DataSingleSpecies_unique)))
  
  thinned <- spThin::thin(
    loc.data   = DataSingleSpecies_unique,
    lat.col    = "decimalLatitude",
    long.col   = "decimalLongitude",
    spec.col   = "species",
    thin.par   = 3,          
    reps       = 100,         
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    verbose     = FALSE
  )
  
  best_rep <- which.max(sapply(thinned, nrow))
  thinned_coords <- thinned[[best_rep]]
  
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    dplyr::inner_join(thinned_coords,
                      by = c("decimalLongitude" = "Longitude",
                             "decimalLatitude"  = "Latitude"))
  
  message(paste0("  Records after thinning: ", nrow(DataSingleSpecies_unique)))
  
  n_sample <- min(300, nrow(DataSingleSpecies_unique))
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    slice_sample(n = n_sample) %>%
    as.data.frame()
  
  myResp <- as.numeric(DataSingleSpecies_unique$species == targetSpecies)
  myRespXY <- DataSingleSpecies_unique[, c("decimalLongitude", "decimalLatitude")]
  
  n.pres <- sum(myResp == 1)
  nb.PA <- c(n.pres, n.pres, n.pres, 10000, 10000, 10000) 
  
  myBiomodData.PA <- BIOMOD_FormatingData(
    resp.var = myResp,
    expl.var = myExpl_full,
    resp.xy = myRespXY,
    resp.name = targetSpecies,
    PA.nb.rep = 6, 
    PA.nb.absences = nb.PA,  
    PA.strategy = 'random', 
    na.rm = TRUE, 
    filter.raster = FALSE) 
  
  message(paste0("Data formatting done for ", targetSpecies))
  
  presence_points <- myBiomodData.PA@coord[myBiomodData.PA@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  presence_df$type <- "Presence Points"
  presence_df$species <- as.character(targetSpecies)
  presence_df <- na.omit(presence_df)
  
  write.csv(
    presence_df,
    file = file.path(output_folder, paste0("PresencePoints_", targetSpecies, "_", extent, ".csv")),
    row.names = FALSE)
  
  ##########
  # STEP 3 # Run the models 
  ##########
  
  models.pa.list <- list(
    RF = c("PA1", "PA2", "PA3"), 
    XGBOOST = c("PA1", "PA2", "PA3"), 
    ANN = c("PA1", "PA2", "PA3"), 
    MAXNET = c("PA4", "PA5", "PA6") 
  )
  
  myBiomodModelOut <- BIOMOD_Modeling(
    bm.format = myBiomodData.PA,
    modeling.id = paste0("Model_", targetSpecies),
    models = c("RF", "XGBOOST", "ANN", "MAXNET"), 
    models.pa = models.pa.list,
    CV.strategy = "random",
    CV.nb.rep = 5, 
    CV.perc = 0.7, 
    OPT.strategy = 'bigboss',
    prevalence = 0.5, 
    metric.eval = c("TSS", "ROC"), 
    var.import = 3, 
    nb.cpu = ncoresToUse, 
    do.progress = TRUE)
  
  message(paste0("Single models completed for ", targetSpecies))
  
  eval_scores <- get_evaluations(myBiomodModelOut)
  eval_scores$species <- targetSpecies  
  var_importance <- get_variables_importance(myBiomodModelOut)
  var_importance$species <- targetSpecies  
  
  write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", targetSpecies, "_", extent, "_", ".csv")), row.names = FALSE)
  
  invisible(gc(rm(myBiomodData.PA)))
  invisible(gc(rm(eval_scores, var_importance)))  
  
  ##########
  # STEP 4 # Project single models
  ##########
  
  myBiomodProj <- lapply(myExplCurrent, function(env_raster) { 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = 'Current',
      new.env = env_raster,
      models.chosen ='all',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  })    
  
  message(paste0("Single models projections done for ", targetSpecies))
  
  ##########
  # STEP 5 # Do ensemble models
  ##########
  
  myBiomodEM <- BIOMOD_EnsembleModeling(
    bm.mod = myBiomodModelOut,
    models.chosen ='all',
    em.by ='all',
    em.algo = c('EMmean'),
    metric.select = c('TSS'),
    metric.select.thresh = c(0.6), # 0.6 for O. bezoarticus and E. platensis
    metric.eval = c('TSS','ROC'),
    nb.cpu = ncoresToUse, 
    do.progress = TRUE,
    var.import = 3,
    EMci.alpha = 0.05)
  
  message(paste0("Ensemble model done for ", targetSpecies))
  
  eval_scoresEM <- get_evaluations(myBiomodEM)
  eval_scoresEM$species <- targetSpecies  
  
  var_importanceEM <- get_variables_importance(myBiomodEM)
  var_importanceEM$species <- targetSpecies  
  
  write.csv(eval_scoresEM, file = file.path(output_folder, paste0("EvalScoresEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importanceEM, file = file.path(output_folder, paste0("VarImportanceEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  
  invisible(gc(rm(eval_scoresEM, var_importanceEM)))  
  
  save_response_plot(myBiomodModelOut, myBiomodEM, targetSpecies, output_folder, tss_threshold = 0.6)  
  invisible(gc())  
  
  ##########
  # STEP 6 # Project ensemble models for current conditions
  ##########
  
  myBiomodEMProj <- lapply(myBiomodProj, function(Proj) { 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = Proj,
      models.chosen ='all',
      metric.binary ='all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = TRUE
    )
  }) 
  
  message(paste0("Ensemble models' projections for current conditions done for ", targetSpecies))
  
  ##########
  # STEP 7 # Project single and ensemble models to future conditions 
  ##########
  
  myBiomodProjectionFuture <- lapply(myExplFuture, function(future_raster) { 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = "Future",
      new.env = future_raster,
      models.chosen = 'all',
      metric.binary = 'TSS',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  }) 
  names(myBiomodProjectionFuture) <- names(myExplFuture)
  
  message(paste0("Single models' projection for future scenarios done for ", targetSpecies))
  
  myBiomodEF <- lapply(myBiomodProjectionFuture, function(future_proj) { 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = future_proj, 
      models.chosen = 'all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = "xz"
    )
  })
  
  message(paste0("Ensemble models' projections for future scenarios done for ", targetSpecies))
  
  ##########
  # STEP 8 # Save ensemble for current and future conditions rasters for each scenario
  ##########
  
  message(paste0("Saving output rasters for ", targetSpecies))
  
  evals <- get_evaluations(myBiomodEM)
  th_TSS <- evals$cutoff[evals$metric.eval == "TSS"]
  
  ## Current Conditions Raster ##
  EMcurrent <- get_predictions(myBiomodEMProj[[1]], as.data.frame = FALSE)
  
  EMcurrent_filename <- file.path(output_folder, paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
  terra::writeRaster(EMcurrent, EMcurrent_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  bin_rasters <- bm_BinaryTransformation(data = EMcurrent, threshold = th_TSS, do.filtering = FALSE)
  names(bin_rasters) <- paste0("ssp126_2030", names(bin_rasters), "_TSSbin")
  bin_filename <- file.path(output_folder, paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_binary.tif"))
  terra::writeRaster(bin_rasters, bin_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  invisible(gc(rm(EMcurrent, EMcurrent_filename, bin_rasters, bin_filename)))  
  invisible(gc())
  
  ## Future Conditions Rasters ##
  for(sc in names(myBiomodEF)) {
    
    message(paste("Saving scenario:", sc))
    
    EFproj <- myBiomodEF[[sc]]
    
    cont_rasters <- get_predictions(EFproj, as.data.frame = FALSE)
    names(cont_rasters) <- paste0(sc, "_", names(cont_rasters))
    cont_filename <- file.path(output_folder, paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
    terra::writeRaster(cont_rasters, cont_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2","BIGTIFF=YES"))
    
    bin_rasters <- bm_BinaryTransformation(data = cont_rasters, threshold = th_TSS, do.filtering = FALSE)
    names(bin_rasters) <- paste0(sc, "_", names(bin_rasters), "_TSSbin")
    bin_filename <- file.path(output_folder, paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_binary.tif"))
    terra::writeRaster(bin_rasters, bin_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2","BIGTIFF=YES"))
    
    rm(EFproj, cont_rasters, bin_rasters)
    invisible(gc())
  }
}

################################################################################
#   Function with a lower TSS threshold for R. americana
################################################################################
SDMensembleRamericana <- function(targetSpecies, # vector of target species names
                                  speciesData, # target species occurrences file from GBIF
                                  myExpl_full, # training landscape (whole world)
                                  myExplCurrent, # current environment landscape (cropped to biome)
                                  myExplFuture, # future environment landscapes (cropped to biome)
                                  extent, # extent name for files' names (e.g. tropical OR boreal)
                                  output_folder, # folder path to save outputs
                                  maxent_source, # path to maxent.jar file
                                  ncoresToUse # n cores to use in parallelization jobs
) {
  
  ##########
  # STEP 1 # Setup & Folder Prep
  ##########
  
  if(!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  if (file.exists(maxent_source)) {
    file.copy(from = maxent_source,
              to = file.path(output_folder, "maxent.jar"),
              overwrite = TRUE)
  } else {
    warning("maxent.jar not found at: ", maxent_source,
            "\nDownload it or place it in this folder before running.")
  }
  
  invisible(gc())
  
  ##########
  # STEP 2 # Filter Occurrence Data & Prepare Presence/Pseudo-absence data
  ##########
  
  message(paste0("Starting for ", targetSpecies))
  
  DataSingleSpecies <- speciesData %>%
    dplyr::filter(species == !!targetSpecies)
  invisible(gc())
  
  DataSingleSpecies <- DataSingleSpecies %>%
    drop_na(decimalLongitude,decimalLatitude, year)
  invisible(gc())
  
  cellValues <- terra::extract(
    myExpl_full,
    cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  cellValues$cell <- terra::cellFromXY(myExpl_full,
                                       cbind(DataSingleSpecies$decimalLongitude, DataSingleSpecies$decimalLatitude))
  
  DataSingleSpecies <- cbind(DataSingleSpecies, cellValues)
  
  DataSingleSpecies_unique <- DataSingleSpecies %>%
    group_by(cell) %>%
    slice_max(year, with_ties = FALSE) %>%  
    ungroup() %>%
    dplyr::filter(complete.cases(.))  
  
  # --- Spatial thinning with spThin (3km minimum distance) ---
  set.seed(123) 
  message(paste0("  Spatial thinning for ", targetSpecies, 
                 " | records before: ", nrow(DataSingleSpecies_unique)))
  
  thinned <- spThin::thin(
    loc.data   = DataSingleSpecies_unique,
    lat.col    = "decimalLatitude",
    long.col   = "decimalLongitude",
    spec.col   = "species",
    thin.par   = 3,          
    reps       = 100,         
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    verbose     = FALSE
  )
  
  best_rep <- which.max(sapply(thinned, nrow))
  thinned_coords <- thinned[[best_rep]]
  
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    dplyr::inner_join(thinned_coords,
                      by = c("decimalLongitude" = "Longitude",
                             "decimalLatitude"  = "Latitude"))
  
  message(paste0("  Records after thinning: ", nrow(DataSingleSpecies_unique)))
  
  n_sample <- min(300, nrow(DataSingleSpecies_unique))
  DataSingleSpecies_unique <- DataSingleSpecies_unique %>%
    slice_sample(n = n_sample) %>%
    as.data.frame()
  
  myResp <- as.numeric(DataSingleSpecies_unique$species == targetSpecies)
  myRespXY <- DataSingleSpecies_unique[, c("decimalLongitude", "decimalLatitude")]
  
  n.pres <- sum(myResp == 1)
  nb.PA <- c(n.pres, n.pres, n.pres, 10000, 10000, 10000) 
  
  myBiomodData.PA <- BIOMOD_FormatingData(
    resp.var = myResp,
    expl.var = myExpl_full,
    resp.xy = myRespXY,
    resp.name = targetSpecies,
    PA.nb.rep = 6, 
    PA.nb.absences = nb.PA,  
    PA.strategy = 'random', 
    na.rm = TRUE, 
    filter.raster = FALSE) 
  
  message(paste0("Data formatting done for ", targetSpecies))
  
  presence_points <- myBiomodData.PA@coord[myBiomodData.PA@data.species == 1, ]
  presence_df <- as.data.frame(presence_points)
  colnames(presence_df) <- c("Longitude", "Latitude")
  presence_df$type <- "Presence Points"
  presence_df$species <- as.character(targetSpecies)
  presence_df <- na.omit(presence_df)
  
  write.csv(
    presence_df,
    file = file.path(output_folder, paste0("PresencePoints_", targetSpecies, "_", extent, ".csv")),
    row.names = FALSE)
  
  ##########
  # STEP 3 # Run the models 
  ##########
  
  models.pa.list <- list(
    RF = c("PA1", "PA2", "PA3"), 
    XGBOOST = c("PA1", "PA2", "PA3"), 
    ANN = c("PA1", "PA2", "PA3"), 
    MAXNET = c("PA4", "PA5", "PA6") 
  )
  
  myBiomodModelOut <- BIOMOD_Modeling(
    bm.format = myBiomodData.PA,
    modeling.id = paste0("Model_", targetSpecies),
    models = c("RF", "XGBOOST", "ANN", "MAXNET"), 
    models.pa = models.pa.list,
    CV.strategy = "random",
    CV.nb.rep = 5, 
    CV.perc = 0.7, 
    OPT.strategy = 'bigboss',
    prevalence = 0.5, 
    metric.eval = c("TSS", "ROC"), 
    var.import = 3, 
    nb.cpu = ncoresToUse, 
    do.progress = TRUE)
  
  message(paste0("Single models completed for ", targetSpecies))
  
  eval_scores <- get_evaluations(myBiomodModelOut)
  eval_scores$species <- targetSpecies  
  var_importance <- get_variables_importance(myBiomodModelOut)
  var_importance$species <- targetSpecies  
  
  write.csv(eval_scores, file = file.path(output_folder, paste0("EvalScores_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importance, file = file.path(output_folder, paste0("VarImportance_", targetSpecies, "_", extent, "_", ".csv")), row.names = FALSE)
  
  invisible(gc(rm(myBiomodData.PA)))
  invisible(gc(rm(eval_scores, var_importance)))  
  
  ##########
  # STEP 4 # Project single models
  ##########
  
  myBiomodProj <- lapply(myExplCurrent, function(env_raster) { 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = 'Current',
      new.env = env_raster,
      models.chosen ='all',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  })    
  
  message(paste0("Single models projections done for ", targetSpecies))
  
  ##########
  # STEP 5 # Do ensemble models
  ##########
  
  myBiomodEM <- BIOMOD_EnsembleModeling(
    bm.mod = myBiomodModelOut,
    models.chosen ='all',
    em.by ='all',
    em.algo = c('EMmean'),
    metric.select = c('TSS'),
    metric.select.thresh = c(0.5), # 0.5 for R. americana
    metric.eval = c('TSS','ROC'),
    nb.cpu = ncoresToUse, 
    do.progress = TRUE,
    var.import = 3,
    EMci.alpha = 0.05)
  
  message(paste0("Ensemble model done for ", targetSpecies))
  
  eval_scoresEM <- get_evaluations(myBiomodEM)
  eval_scoresEM$species <- targetSpecies  
  
  var_importanceEM <- get_variables_importance(myBiomodEM)
  var_importanceEM$species <- targetSpecies  
  
  write.csv(eval_scoresEM, file = file.path(output_folder, paste0("EvalScoresEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  write.csv(var_importanceEM, file = file.path(output_folder, paste0("VarImportanceEM_", targetSpecies, "_", extent, ".csv")), row.names = FALSE)
  
  invisible(gc(rm(eval_scoresEM, var_importanceEM)))  
  
  save_response_plot(myBiomodModelOut, myBiomodEM, targetSpecies, output_folder, tss_threshold = 0.5)  
  invisible(gc())  
  
  ##########
  # STEP 6 # Project ensemble models for current conditions
  ##########
  
  myBiomodEMProj <- lapply(myBiomodProj, function(Proj) { 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = Proj,
      models.chosen ='all',
      metric.binary ='all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = TRUE
    )
  }) 
  
  message(paste0("Ensemble models' projections for current conditions done for ", targetSpecies))
  
  ##########
  # STEP 7 # Project single and ensemble models to future conditions 
  ##########
  
  myBiomodProjectionFuture <- lapply(myExplFuture, function(future_raster) { 
    BIOMOD_Projection(
      bm.mod = myBiomodModelOut,
      proj.name = "Future",
      new.env = future_raster,
      models.chosen = 'all',
      metric.binary = 'TSS',
      build.clamping.mask = TRUE,
      nb.cpu = ncoresToUse
    )
  }) 
  names(myBiomodProjectionFuture) <- names(myExplFuture)
  
  message(paste0("Single models' projection for future scenarios done for ", targetSpecies))
  
  myBiomodEF <- lapply(myBiomodProjectionFuture, function(future_proj) { 
    BIOMOD_EnsembleForecasting(
      bm.em = myBiomodEM,
      bm.proj = future_proj, 
      models.chosen = 'all',
      nb.cpu = ncoresToUse,
      binary.meth = c("TSS"),
      compress = "xz"
    )
  })
  
  message(paste0("Ensemble models' projections for future scenarios done for ", targetSpecies))
  
  ##########
  # STEP 8 # Save ensemble for current and future conditions rasters for each scenario
  ##########
  
  message(paste0("Saving output rasters for ", targetSpecies))
  
  evals <- get_evaluations(myBiomodEM)
  th_TSS <- evals$cutoff[evals$metric.eval == "TSS"]
  
  ## Current Conditions Raster ##
  EMcurrent <- get_predictions(myBiomodEMProj[[1]], as.data.frame = FALSE)
  
  EMcurrent_filename <- file.path(output_folder, paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
  terra::writeRaster(EMcurrent, EMcurrent_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  bin_rasters <- bm_BinaryTransformation(data = EMcurrent, threshold = th_TSS, do.filtering = FALSE)
  names(bin_rasters) <- paste0("ssp126_2030", names(bin_rasters), "_TSSbin")
  bin_filename <- file.path(output_folder, paste0("proj_Current_EM_", gsub(" ", ".", targetSpecies), "_binary.tif"))
  terra::writeRaster(bin_rasters, bin_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))
  
  invisible(gc(rm(EMcurrent, EMcurrent_filename, bin_rasters, bin_filename)))  
  invisible(gc())
  
  ## Future Conditions Rasters ##
  for(sc in names(myBiomodEF)) {
    
    message(paste("Saving scenario:", sc))
    
    EFproj <- myBiomodEF[[sc]]
    
    cont_rasters <- get_predictions(EFproj, as.data.frame = FALSE)
    names(cont_rasters) <- paste0(sc, "_", names(cont_rasters))
    cont_filename <- file.path(output_folder, paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_continuous.tif"))
    terra::writeRaster(cont_rasters, cont_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2","BIGTIFF=YES"))
    
    bin_rasters <- bm_BinaryTransformation(data = cont_rasters, threshold = th_TSS, do.filtering = FALSE)
    names(bin_rasters) <- paste0(sc, "_", names(bin_rasters), "_TSSbin")
    bin_filename <- file.path(output_folder, paste0("proj_", sc, "_", gsub(" ", ".", targetSpecies), "_binary.tif"))
    terra::writeRaster(bin_rasters, bin_filename, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2","BIGTIFF=YES"))
    
    rm(EFproj, cont_rasters, bin_rasters)
    invisible(gc())
  }
}
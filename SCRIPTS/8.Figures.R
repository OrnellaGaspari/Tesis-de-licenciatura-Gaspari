## Name: 8.Figures.R
## Author: Ornella Gaspari
## Goal: Create all plots, stacked barplots, summary metrics tables (TSS/ROC) 
##       and landscape cover figures for the final thesis.

source(here("SCRIPTS", "0.libraries.R"))

################################################################################
#                                Plot SDM outputs
################################################################################
# Directories
input_dir  <- here("OUTPUT", "SDMOutputs no Forest")
output_dir <- here("OUTPUT", "SDMOutputs no Forest", "plots")

# If needed create output_dir
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Get all the .tif files starting with "proj_"
tif_files <- list.files(
  path       = input_dir,
  pattern    = "^proj_.*\\.tif$",
  full.names = TRUE
)

message("Archivos encontrados: ", length(tif_files))

# Loop see plot and save PNG
for (f in tif_files) {
  
  # Name
  fname <- tools::file_path_sans_ext(basename(f))
  
  # load raster
  r <- rast(f)
  
  # See plot
  plot(r, main = fname, range = c(0, 1000))
  
  # Save PNG
  out_path <- file.path(output_dir, paste0(fname, ".png"))
  
  png(out_path, width = 1200, height = 900, res = 150)
  plot(r, main = fname, range = c(0, 1000))
  dev.off()
  
  message("Guardado: ", basename(out_path))
}

################################################################################
#                                 Plot landscapes
################################################################################
# myExpl_Current - current conditions
myExplCurrent <- here("DATA", "Landscapes", "ProcessedLandscapes", "CurrentLandscapes_2015_1km_pampa_cropped_bioclim_NF.tif")

# myExpl_Future - future conditions
ssp2_2050 <- here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp45_ssp2_2050_no_policy_bioclim_1km_NF.tif")
ssp2_2100 <- here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp45_ssp2_2100_no_policy_bioclim_1km_NF.tif")

ssp1_2050 <- here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp26_ssp1_2050_no_policy_bioclim_1km_NF.tif")
ssp1_2100 <- here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp26_ssp1_2100_no_policy_bioclim_1km_NF.tif")

ssp5_2050 <- here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp85_ssp5_2050_no_policy_bioclim_1km_NF.tif")
ssp5_2100 <- here("DATA", "Landscapes", "ProcessedLandscapes", "futureLandscape_lulc_seals7_gtap1_rcp85_ssp5_2100_no_policy_bioclim_1km_NF.tif")


plot_landscape_layers <- function(filepath, output_folder_landscapes) {
  
  # If needed create output folder
  if (!dir.exists(output_folder_landscapes)) dir.create(output_folder_landscapes, recursive = TRUE)
  
  # Read raster
  r <- rast(filepath)
  filename <- tools::file_path_sans_ext(basename(filepath))
  
  # Get metadata from the file name
  scenario <- regmatches(filename, regexpr("ssp\\d+", filename))
  year     <- regmatches(filename, regexpr("\\d{4}", filename))
  rcp      <- regmatches(filename, regexpr("rcp\\d+", filename))
  
  # For the files that don't have scenario and date (current)
  if (length(scenario) == 0) scenario <- "Current"
  if (length(year)     == 0) year     <- "Current"
  if (length(rcp)      == 0) rcp      <- ""
  
  # Loop layers
  for (i in 1:nlyr(r)) {
    layer_name <- names(r)[i]
    
    titulo <- if (rcp == "") {
      paste0(layer_name, " | ", scenario, " | ", year)
    } else {
      paste0(layer_name, " | ", scenario, " | ", year)
    }
    
    out_file <- file.path(output_folder_landscapes, 
                          paste0(scenario, "_", year, "_", layer_name, ".png"))
    
    png(out_file, width = 1200, height = 900, res = 150)
    plot(r[[i]], main = titulo)
    dev.off()
    
    message("Saved: ", basename(out_file))
  }
  
  message("Ready! ", nlyr(r), " layers saved in: ", output_folder_landscapes)
}

output_landscapes_plots <- here("OUTPUT", "Landscapes_plots_1km")

plot_landscape_layers(ssp2_2050, output_landscapes_plots)
plot_landscape_layers(ssp2_2100, output_landscapes_plots)
plot_landscape_layers(ssp1_2050, output_landscapes_plots)
plot_landscape_layers(ssp1_2100, output_landscapes_plots)
plot_landscape_layers(ssp5_2050, output_landscapes_plots)
plot_landscape_layers(ssp5_2100, output_landscapes_plots)
plot_landscape_layers(myExplCurrent, output_landscapes_plots)

################################################################################
#                        Changes in total suitable area
################################################################################

#################################### TABLE #####################################

# Species list
species_list <- c(
  "Boana pulchella",
  "Ceratophrys ornata",
  "Dasypus hybridus",
  "Embernagra platensis",
  "Odontophrynus asper",
  "Ozotoceros bezoarticus",
  "Pseudoleistes virescens",
  "Rhea americana",
  "Rhinella dorbignyi",
  "Xanthopsar flavus"
)

# Get the total suitable area from the binary projections
get_suitable_area <- function(species_name, sdm_tif_dir) {
  
  sp_file <- gsub(" ", ".", species_name)
  
  files <- list(
    Current   = file.path(sdm_tif_dir, paste0("proj_Current_EM_",  sp_file, "_binary.tif")),
    SSP1_2050 = file.path(sdm_tif_dir, paste0("proj_ssp1_2050_",   sp_file, "_binary.tif")),
    SSP2_2050 = file.path(sdm_tif_dir, paste0("proj_ssp2_2050_",   sp_file, "_binary.tif")),
    SSP5_2050 = file.path(sdm_tif_dir, paste0("proj_ssp5_2050_",   sp_file, "_binary.tif")),
    SSP1_2100 = file.path(sdm_tif_dir, paste0("proj_ssp1_2100_",   sp_file, "_binary.tif")),
    SSP2_2100 = file.path(sdm_tif_dir, paste0("proj_ssp2_2100_",   sp_file, "_binary.tif")),
    SSP5_2100 = file.path(sdm_tif_dir, paste0("proj_ssp5_2100_",   sp_file, "_binary.tif"))
  )
  
  missing <- names(files)[!sapply(files, file.exists)]
  if (length(missing) > 0) {
    warning("Files not found for ", species_name, ": ", paste(missing, collapse = ", "))
  }
  
  calc_area_ha <- function(path) {
    if (!file.exists(path)) return(NA)
    r          <- rast(path)
    cell_areas <- cellSize(r, unit = "km")
    product    <- cell_areas * r  
    area_km2   <- global(product, fun = "sum", na.rm = TRUE)[[1]]
    area_km2 * 100
  }
  
  data.frame(
    species  = species_name,
    scenario = c("Current", "SSP1", "SSP2", "SSP5", "SSP1",  "SSP2", "SSP5"),
    period   = c("2015", "2050", "2050", "2050", "2100", "2100",  "2100"),
    area_ha  = sapply(files, calc_area_ha),
    row.names = NULL
  )
}

# Apply to all the species and combine in one dataframe
tif_dir <- here("OUTPUT", "SDMOutputs no Forest")

suitable_areas <- bind_rows(lapply(species_list, get_suitable_area, sdm_tif_dir = tif_dir))

print(suitable_areas)

# Calculate difference from current for each future scenario
suitable_areas_diff <- suitable_areas %>%
  group_by(species) %>%
  mutate(
    area_current = area_ha[period == "2015"],
    diff_ha      = area_ha - area_current,
    diff_pct     = ((area_ha - area_current) / area_current) * 100
  ) %>%
  filter(period != "2015") %>%
  dplyr::select(species, scenario, period, area_ha, diff_ha, diff_pct) %>%
  ungroup()

print(suitable_areas_diff)

# Export to Word
suitable_areas_diff |>
  gt(groupname_col = "species") |>
  cols_label(
    scenario = "Scenario",
    period   = "Period",
    area_ha  = "Area (ha)",
    diff_ha  = "Difference (ha)",
    diff_pct = "Change (%)"
  ) |>
  fmt_number(columns = c(area_ha, diff_ha), decimals = 0) |>
  fmt_number(columns = diff_pct, decimals = 1) |>
  tab_options(table.font.name = "Times New Roman", table.font.size = 10) |>
  gtsave(file.path(output_dir, "suitable_areas_diff.docx"))

################################## BAR CHART ###################################

colores <- c("Current" = "gray54", "SSP1" = "aquamarine3", "SSP2" = "midnightblue", "SSP5" = "plum")

# separar current de los escenarios futuros
current_plot <- suitable_areas %>%
  filter(period == "2015") %>%
  mutate(
    group    = "Current",
    scenario = "Current",
    period   = "Current"
  )

future_plot <- suitable_areas %>%
  filter(period != "2015") %>%
  mutate(group = period)  # group = "2050" o "2100"

all_plot <- bind_rows(current_plot, future_plot) %>%
  mutate(
    area_Mha = area_ha / 1e6,
    scenario = factor(scenario, levels = c("Current", "SSP1", "SSP2", "SSP5")),
    group    = factor(group,    levels = c("Current", "2050", "2100")),
    # position on x axis
    x_pos = case_when(
      group == "Current" ~ 1,
      group == "2050" & scenario == "SSP1" ~ 3,
      group == "2050" & scenario == "SSP2" ~ 4,
      group == "2050" & scenario == "SSP5" ~ 5,
      group == "2100" & scenario == "SSP1" ~ 7,
      group == "2100" & scenario == "SSP2" ~ 8,
      group == "2100" & scenario == "SSP5" ~ 9
    )
  )

# Generate a png for each species
dir.create(file.path(output_dir, "plots"), showWarnings = FALSE)

for (sp in species_list) {
  
  sp_data <- all_plot %>% filter(species == sp)
  
  p <- ggplot(sp_data, aes(x = x_pos, y = area_Mha, fill = scenario)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = colores, name = "Escenario",
                      breaks = c("DS", "TH", "DFD")) +
    scale_x_continuous(
      breaks = c(1, 3, 4, 5, 7, 8, 9),
      labels = c("Actual", "DS", "TH", "DFD", "DS", "TH", "DFD"),
      expand = expansion(add = 1)
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.05)),
      n.breaks = 10 
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = bquote(italic(.(sp))),
      x     = NULL,
      y     = "Área idónea total (millones de hectáreas)"
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title      = element_text(size = 11, hjust = 0.5),
      axis.text.x      = element_text(size = 9),
      axis.text.y      = element_text(size = 9),
      axis.title.y     = element_text(size = 10),
      axis.line.x      = element_line(color = "black"),
      axis.line.y      = element_line(color = "black"),
      legend.position = "right",
      legend.title    = element_text(size = 9),
      legend.text      = element_text(size = 9),
      plot.margin      = ggplot2::margin(t = 10, r = 10, b = 40, l = 10)
    )  
  sp_file <- gsub(" ", "_", sp)
  ggsave(
    file.path(output_dir, paste0(sp_file, "_barrplot_suitable_area.png")),
    plot   = p,
    width  = 5,
    height = 4,
    dpi    = 300
  )
  
  message("Saved: ", sp)
}

################################################################################
#       Calculate the area cover for each land-use category on each scenario
################################################################################

# -- Directories
dir_landscapes    <- here("DATA", "Landscapes", "ProcessedLandscapes")
output_dir_escrito <- here("Escrito")

if (!dir.exists(output_dir_escrito)) dir.create(output_dir_escrito, recursive = TRUE)

# Land-use categories
land_use_vars <- c("Urban", "Cropland", "Pasture_Grassland",
                   "Forest", "Nonforest_vegetation", "Water", "Barren_other")

# Calculate effective area (ha) per category for a multilayer raster
calc_area_efectiva <- function(r, scenario_name) {
  
  # cell area in km²
  cell_areas <- terra::cellSize(r, unit = "km")
  
  result <- lapply(land_use_vars, function(var) {
    
    lyr <- r[[var]]
    
    # effective area = sum(proportion × cell_area), ignoring NAs
    area_km2 <- terra::global(lyr * cell_areas, fun = "sum", na.rm = TRUE)$sum
    area_ha  <- area_km2 * 100  # 1 km² = 100 ha
    
    data.frame(
      scenario = scenario_name,
      land_use = var,
      area_ha  = round(area_ha, 2)
    )
  })
  
  do.call(rbind, result)
}

# ---- Current landscape 
r_current <- terra::rast(file.path(dir_landscapes, "CurrentLandscapes_2015_1km_pampa_cropped_bioclim.tif"))

# subset only the land-use layers (exclude bio03 and bio15)
r_current_lu <- r_current[[land_use_vars]]

area_current <- calc_area_efectiva(r_current_lu, scenario_name = "Current_2015")

terra::minmax(r_current[["Urban"]])

# ---- Future landscapes 
future_files <- list.files(dir_landscapes,
                           pattern = "^futureLandscape_.*_bioclim_1km\\.tif$",
                           full.names = TRUE)

area_future_list <- lapply(future_files, function(f) {
  
  r <- terra::rast(f)
  r_lu <- r[[land_use_vars]]
  
  # extract scenario name from file name
  scenario_name <- tools::file_path_sans_ext(basename(f))
  
  calc_area_efectiva(r_lu, scenario_name)
})

# ---- Combine and save 
area_all <- do.call(rbind, c(list(area_current), area_future_list))

write.csv(area_all,
          file = file.path(output_dir_escrito, "landuse_area_ha_all_scenarios.csv"),
          row.names = FALSE)

message("Done. File saved at: ", normalizePath(file.path(output_dir_escrito, "landuse_area_ha_all_scenarios.csv")))

############ Create a stacked barplot to show the changes in the land-use cover
area_by_category <- read.csv(file.path(output_dir_escrito, "landuse_area_ha_all_scenarios.csv"))

# --- Clean scenario names and set order
scenario_labels <- c(
  "Current_2015"                                                           = "Actual\n2015",
  "futureLandscape_lulc_seals7_gtap1_rcp26_ssp1_2050_no_policy_bioclim_1km" = "DS\n2050",
  "futureLandscape_lulc_seals7_gtap1_rcp26_ssp1_2100_no_policy_bioclim_1km" = "DS\n2100",
  "futureLandscape_lulc_seals7_gtap1_rcp45_ssp2_2050_no_policy_bioclim_1km" = "TH\n2050",
  "futureLandscape_lulc_seals7_gtap1_rcp45_ssp2_2100_no_policy_bioclim_1km" = "TH\n2100",
  "futureLandscape_lulc_seals7_gtap1_rcp85_ssp5_2050_no_policy_bioclim_1km" = "DFD\n2050",
  "futureLandscape_lulc_seals7_gtap1_rcp85_ssp5_2100_no_policy_bioclim_1km" = "DFD\n2100"
)

# --- Calculate percentages and prepare plot data
plot_data <- area_by_category %>%
  group_by(scenario) %>%
  mutate(pct = area_ha / sum(area_ha, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(
    scenario_label = factor(scenario_labels[scenario], 
                            levels = scenario_labels),
    land_use = factor(land_use, levels = rev(land_use_vars))
  )

# --- Colors per land-use category
land_use_colors <- c(
  "Urban"                = "#E05C5C",
  "Cropland"             = "#F5C542",
  "Pasture_Grassland"    = "#A8D97F",
  "Forest"               = "#2E8B57",
  "Nonforest_vegetation" = "#90C3A0",
  "Water"                = "#5B9BD5",
  "Barren_other"         = "#C4A882"
)

# --- Stacked barplot
p <- ggplot(plot_data, aes(x = scenario_label, y = pct, fill = land_use)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = land_use_colors,
    name   = "Land-use category",
    guide  = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Land-use cover (%)"
  ) +
  theme_classic() +
  theme(
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title.y     = element_text(size = 10),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 9),
  )

# --- Save
ggsave(
  file.path(output_dir, "landuse_cover_pct_barplot.png"),
  plot = p, width = 9, height = 5, dpi = 300
)

message("Saved: landuse_cover_pct_barplot.png")


################################################################################
#                         Summary table TSS and ROC
################################################################################
eval_files <- list.files(
  here("OUTPUT", "SDMOutputs no Forest"),
  pattern = "^EvalScores_.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

all_evals <- bind_rows(lapply(eval_files, read.csv))

# Summary table TSS and ROC
eval_summary <- all_evals %>%
  filter(metric.eval %in% c("TSS", "ROC"), !is.na(validation)) %>%
  group_by(species, metric.eval) %>%
  summarise(
    n_models = n(),
    min      = round(min(validation, na.rm = TRUE), 3),
    mean     = round(mean(validation, na.rm = TRUE), 3),
    median   = round(median(validation, na.rm = TRUE), 3),
    max      = round(max(validation, na.rm = TRUE), 3),
    .groups  = "drop"
  )

print(eval_summary)







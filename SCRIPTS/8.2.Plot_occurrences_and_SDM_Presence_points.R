## Name: 8.2.Plot_occurrences_and_SDM_Presence_points.R
## Author: Ornella Gaspari

source(here("SCRIPTS", "0.libraries.R"))

################################################################################
#                    Plot occurrences and SDM presences 
################################################################################

# Base layers
# South america polygon 
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

south_america_sf <- world_sf |>
  filter(continent == "South America")

# Argentine provinces
prov <- geodata::gadm(
  country = "ARG",
  level = 1,
  path = tempdir()
)

provincias_sf <- st_as_sf(prov) %>%
  st_transform(4326)

# South America bounding box 
bbox <- st_bbox(south_america_sf)

# Plot function
plot_species_map <- function(species_name, all_species_data, presences_dir = ".", fixed_bbox, output_dir = ".") {
  
  # Occurrences
  occurrences_df <- all_species_data |>
    filter(species == species_name) |>
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude))
  
  # Count occurrences 
  n_occ <- nrow(occurrences_df)
  
  occ_sp <- st_as_sf(occurrences_df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  # Presences
  fname <- list.files(path = presences_dir, pattern = paste0("^PresencePoints_", species_name), full.names = TRUE)
  
  if (length(fname) == 0) {
    message("No SDM files found for: ", species_name)
    return(NULL)
  }
  
  presences_sp_df <- read.csv(fname[1]) |> filter(!is.na(Longitude), !is.na(Latitude))
  
  # Count SDM presences
  n_sdm <- nrow(presences_sp_df)
  
  presences_sp <- st_as_sf(presences_sp_df, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Draw the map
  p <- ggplot() +
    # Base layers
    geom_sf(data = world_sf, fill = "gray95", color = "gray70", linewidth = 0.3) +
    geom_sf(data = south_america_sf, fill = "gray92", color = "gray60", linewidth = 0.4) +
    geom_sf(data = provincias_sf, fill = NA, color = "gray80", linewidth = 0.2) +
    
    # Points legend
    geom_sf(data = occ_sp, aes(color = "Occurrences"), size = 1.2, alpha = 0.5) +
    geom_sf(data = presences_sp, aes(color = "SDM presences"), size = 1.8, alpha = 0.8) +
    
    # Force bbox
    coord_sf(
      xlim = c(fixed_bbox["xmin"] - 1, fixed_bbox["xmax"] + 1),
      ylim = c(fixed_bbox["ymin"] - 1, fixed_bbox["ymax"] + 1),
      expand = FALSE
    ) +
    
    # NOTES: Point Count
    # Use paste0 with \n to create a line break
    annotate(
      "text", 
      x = fixed_bbox["xmax"], 
      y = fixed_bbox["ymin"], 
      label = paste0("Total occurrence points = ", n_occ, "\nUsed SDM presences = ", n_sdm),
      hjust = 1.1, vjust = -0.5,
      size = 3.2, 
      fontface = "bold.italic", 
      color = "black",
      family = "sans"
    ) +
    
    # Color
    scale_color_manual(
      values = c("Occurrences" = "#4EA8DE", "SDM presences" = "#E8A020"),
      guide = guide_legend(override.aes = list(size = 3, alpha = 1))
    ) +
    
    labs(
      title = bquote(italic(.(gsub("_", " ", species_name))) ~ "occurrences"),
      x = "Longitude", y = "Latitude"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "italic", size = 14),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2)
    )
  
  return(p)
}

# --- Apply to all the species
# Load species data
all_species_data <- read.csv(here("DATA", "Processed data", "All_species_data.csv"))

my_manual_bbox <- st_bbox(c(xmin = -71, xmax = -32, ymin = -42, ymax = -5), 
                          crs = st_crs(4326))

species_list <- unique(all_species_data$species)

output_dir <- here("OUTPUT", "SDMOutputs no Forest", "plots")

lapply(species_list, function(sp) {
  p <- plot_species_map(sp, all_species_data, here("OUTPUT", "SDMOutputs no Forest"), 
                        fixed_bbox = my_manual_bbox)
  if (!is.null(p)) {
    ggsave(
      file.path(output_dir, paste0(gsub(" ", "_", sp), "_map.png")), 
      p, width = 8, height = 7, dpi = 300
    )
  }
})
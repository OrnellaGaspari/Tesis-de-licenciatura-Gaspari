## Name: 8.1.VarImportanceEM_table.R
## Author: Ornella Gaspari
## Goal: Read, combine, and summarize Ensemble Model variable importance 
##       scores across all modeled species, exporting a publication-ready table to Word.

# VarImportance table 
source(here("SCRIPTS", "0.libraries.R"))

##########
# STEP 1 # Read and combine all the csv files
##########

# Directory with all the .csv
csv_dir <- here("OUTPUT", "SDMOutputs no Forest")  

# List all the csv with the pattern VarImportanceEM_*.csv
csv_files <- list.files(
  path       = csv_dir,
  pattern    = "^VarImportanceEM_.*\\.csv$",
  full.names = TRUE
)

# Read and combine in one dataframe
all_data <- bind_rows(lapply(csv_files, read.csv))

##########
# STEP 2 # Calculate Mean and SD by species and variable
##########

summary_table <- all_data |>
  group_by(species, expl.var) |>
  summarise(
    Mean = mean(var.imp, na.rm = TRUE),
    SD   = sd(var.imp,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Interleave Mean row and SD row by species
  pivot_longer(
    cols      = c(Mean, SD),
    names_to  = "metric",
    values_to = "value"
  ) |>
  mutate(value = round(value, 6)) |>
  # Pivoting variables as columns
  pivot_wider(
    names_from  = expl.var,
    values_from = value
  ) |>
  # Sort: species in italics, metric = Mean first
  arrange(species, desc(metric == "Mean")) |>
  # Rename columns for final table
  rename(
    Species = species,
    Metric  = metric
  )

##########
# STEP 3 # Build table and export to word
##########

ft <- summary_table |>
  gt(groupname_col = "Species", rowname_col = "Metric") |>
  
  # Species names in italic
  tab_style(
    style     = cell_text(style = "italic"),
    locations = cells_row_groups()
  ) |>
  
  # Black heading
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  
  # Font and general size
  tab_options(
    table.font.name = "Times New Roman",
    table.font.size = 10
  ) |>
  
  # Number format: 6 decimal places
  fmt_number(
    columns  = everything(),
    decimals = 6
  )

# --- Export to Word ---
gt::gtsave(ft, filename = file.path(csv_dir, "VarImportance_Table.docx"))
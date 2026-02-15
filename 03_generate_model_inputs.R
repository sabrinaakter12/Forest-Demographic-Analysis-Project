###############################################################################
# Script: 02_generate_inital_states_PPA_model_inputs.R
# Author: Sabrina Akter
# Date:   February 2026
#
# Description: 
#   Generates initial state files (.txt) for the PPA simulation model.
#   Filters trees by species and DBH class from the cleaned inventory data.
###############################################################################

library(dplyr)

# Source preprocessing and demographic scripts (project-relative, anonymized)
source(file.path("R", "demographic_rates_script_new.R"))

# Define output directory relative to project root
output_dir <- file.path("outputs", "initial_states")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

create_ppa_states <- function(data, filename, plot_area_ha) {
  dbh_classes <- seq(5, 152, by = 1)
  states      <- data.frame()
  
  for (sp in species_order) {
    sp_data <- data %>%
      filter(Species == sp, !is.na(DBH_2020_num), DBH_2020_num > 0)
    if (nrow(sp_data) == 0) next
    
    for (dbh_class in dbh_classes) {
      in_class <- sp_data %>%
        filter(DBH_2020_num >= dbh_class,
               DBH_2020_num < (dbh_class + 1))
      
      n_trees <- nrow(in_class)
      
      if (n_trees > 0) {
        canopy_layer <- ifelse(
          !is.na(in_class$Canopy_clean[1]),
          in_class$Canopy_clean[1],
          1
        )
        
        n_per_ha <- n_trees / plot_area_ha
        
        states <- rbind(
          states,
          data.frame(
            dbh     = dbh_class,
            n       = n_per_ha,
            canopy  = canopy_layer,
            species = paste0('"', sp, '"')
          )
        )
      }
    }
  }
  
  write.table(
    states,
    file.path(output_dir, filename),
    row.names = FALSE,
    col.names = FALSE,
    sep       = "\t",
    quote     = FALSE
  )
}

create_ppa_states(
  data_Group_A,
  "initial_states_treatment_wet.txt",
  plot_area_ha = 0.75
)

create_ppa_states(
  data_Group_B,
  "initial_states_treatment_dry.txt",
  plot_area_ha = 0.50
)

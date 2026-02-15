############################################################
# Script: 02_rates_validation_plots.R
# Author: Sabrina Akter
# Demographic rate comparison – Treatment_Wet / Treatment_Dry / Reference
# Files use columns: Species, G1, G2, mu1, mu2, rec_ha_yr
############################################################

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#-------------------------------------------------
# Paths
#-------------------------------------------------
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

rates_dir <- file.path(output_dir, "demographic_rates")
if (!dir.exists(rates_dir)) dir.create(rates_dir, recursive = TRUE)

ref_dir <- file.path("data")
out_dir <- file.path(output_dir, "reference_graphs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

path_Treatment_Wet  <- file.path(rates_dir, "demographic_rates_treatment_wet.csv")
path_Treatment_Dry  <- file.path(rates_dir, "demographic_rates_treatment_dry.csv")
refpaper_path       <- file.path(ref_dir, "reference_rates_literature.xlsx")

#-------------------------------------------------
# Species colours and order (anonymized)
#-------------------------------------------------
species_cols <- c(
  "Species_A" = "#228B22",
  "Species_B" = "#00FF00",
  "Species_C" = "#FFA500",
  "Species_D" = "blue",
  "Species_E" = "red",
  "Species_F" = "#17becf",
  "Species_G" = "grey"
)
species_order <- names(species_cols)

#-------------------------------------------------
# Read data (Species, G1, G2, mu1, mu2, rec_ha_yr)
#   G1, G2     : mean annual growth (cm year^-1)
#   mu1, mu2   : annual mortality probability (year^-1)
#   rec_ha_yr  : recruitment (trees ha^-1 year^-1)
#-------------------------------------------------
wet_raw  <- read_csv(path_Treatment_Wet, show_col_types = FALSE)
dry_raw  <- read_csv(path_Treatment_Dry, show_col_types = FALSE)
ref_raw  <- read_excel(refpaper_path, sheet = 1)

# Ensure consistent names in reference file
# (assumes same structure: Species, G1, G2, mu1, mu2, rec_ha_yr)
ref_raw <- ref_raw %>%
  rename(
    Species   = Species,
    G1        = G1,
    G2        = G2,
    mu1       = mu1,
    mu2       = mu2,
    rec_ha_yr = rec_ha_yr
  )

#-------------------------------------------------
# Add scenario labels and combine
#-------------------------------------------------
rates_wet <- wet_raw %>%
  mutate(
    Species = as.character(Species),
    scenario = "Treatment_Wet"
  )

rates_dry <- dry_raw %>%
  mutate(
    Species = as.character(Species),
    scenario = "Treatment_Dry"
  )

rates_ref <- ref_raw %>%
  mutate(
    Species = as.character(Species),
    scenario = "Reference_Literature"
  )

all_rates <- bind_rows(rates_wet, rates_dry, rates_ref) %>%
  mutate(
    Species = factor(Species, levels = species_order),
    scenario = factor(
      scenario,
      levels = c("Treatment_Wet", "Reference_Literature", "Treatment_Dry")
    )
  )

#-------------------------------------------------
# Long format with rate groups and canopy labels
#-------------------------------------------------
rates_long <- all_rates %>%
  pivot_longer(
    cols      = c(G1, G2, mu1, mu2, rec_ha_yr),
    names_to  = "rate_code",
    values_to = "value"
  ) %>%
  mutate(
    rate_group = case_when(
      rate_code %in% c("G1", "G2")     ~ "Growth",
      rate_code %in% c("mu1", "mu2")   ~ "Mortality",
      rate_code == "rec_ha_yr"         ~ "Recruitment",
      TRUE ~ NA_character_
    ),
    canopy = case_when(
      rate_code %in% c("G1", "mu1")    ~ "Canopy 1",
      rate_code %in% c("G2", "mu2")    ~ "Canopy 2",
      rate_code == "rec_ha_yr"         ~ "All canopies",
      TRUE ~ NA_character_
    ),
    canopy = factor(
      canopy,
      levels = c("Canopy 1", "Canopy 2", "All canopies")
    ),
    category = factor(
      scenario,
      levels = c("Treatment_Wet", "Reference_Literature", "Treatment_Dry")
    )
  )

#-------------------------------------------------
# Common theme
#-------------------------------------------------
theme_academic <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title  = element_text(face = "bold", size = 14, hjust = 0),
      axis.title  = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

#-------------------------------------------------
# 1) Growth (G1,G2) – cm year^-1, categories on x-axis
#-------------------------------------------------
growth_dat <- rates_long %>%
  filter(rate_group == "Growth")

p_growth_cat <- ggplot(
  growth_dat,
  aes(x = category, y = value,
      colour = Species, group = Species)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~ canopy, nrow = 1) +
  scale_colour_manual(values = species_cols, name = "Species") +
  labs(
    title = "Growth rates – Treatment_Wet, Reference, Treatment_Dry",
    x     = "Category",
    y     = "Mean annual growth (cm year\u207b\u00b9)"
  ) +
  theme_academic()

ggsave(
  file.path(out_dir, "growth_categories_wet_ref_dry.png"),
  p_growth_cat, width = 8, height = 4.5, dpi = 400
)

#-------------------------------------------------
# 2) Mortality (mu1,mu2) – probability year^-1
#-------------------------------------------------
mort_dat <- rates_long %>%
  filter(rate_group == "Mortality")

p_mort_cat <- ggplot(
  mort_dat,
  aes(x = category, y = value,
      colour = Species, group = Species)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~ canopy, nrow = 1) +
  scale_colour_manual(values = species_cols, name = "Species") +
  labs(
    title = "Mortality rates – Treatment_Wet, Reference, Treatment_Dry",
    x     = "Category",
    y     = "Annual mortality probability (year\u207b\u00b9)"
  ) +
  theme_academic()

ggsave(
  file.path(out_dir, "mortality_categories_wet_ref_dry.png"),
  p_mort_cat, width = 8, height = 4.5, dpi = 400
)

#-------------------------------------------------
# 3) Recruitment (rec_ha_yr) – trees ha^-1 year^-1
#-------------------------------------------------
rec_dat <- rates_long %>%
  filter(rate_group == "Recruitment")

p_rec_cat <- ggplot(
  rec_dat,
  aes(x = category, y = value,
      colour = Species, group = Species)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = species_cols, name = "Species") +
  labs(
    title = "Recruitment – Treatment_Wet, Reference, Treatment_Dry",
    x     = "Category",
    y     = "Recruitment (trees ha\u207b\u00b9 year\u207b\u00b9)"
  ) +
  theme_academic()

ggsave(
  file.path(out_dir, "recruitment_categories_wet_ref_dry.png"),
  p_rec_cat, width = 8, height = 4.5, dpi = 400
)


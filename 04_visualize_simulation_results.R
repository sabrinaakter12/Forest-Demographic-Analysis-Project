## ======================================================================
# Script: 04_visualize_simulation_results.R
# Author: Sabrina Akter
#
# Description: 
#   Visualizes PPA simulation outputs (forest structural dynamics).
#   Compares species composition, basal area, and size structure 
#   between Treatment_Wet vs. Treatment_Dry scenarios over time.
#
# Input: Simulation result files (all_cohorts_out.csv, stats_out.csv).
# Output: Time-series plots and size-class distribution bar charts.

## ======================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(tibble)

## ---- output directory -------------------------------------------------
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

out_dir <- file.path(output_dir, "sp_comp_size_structure")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ---- input file paths -------------------------------------------------
# Cohort outputs from simulation runs
cohort_wet_path  <- file.path("simulation_outputs", "Treatment_Wet", "all_cohorts_out.csv")
cohort_dry_path  <- file.path("simulation_outputs", "Treatment_Dry", "all_cohorts_out.csv")

# Stand-level summaries from simulation runs
stats_wet_path   <- file.path("simulation_outputs", "Treatment_Wet", "stats_out.csv")
stats_dry_path   <- file.path("simulation_outputs", "Treatment_Dry", "stats_out.csv")

## ---- read data --------------------------------------------------------
cohort_wet <- read_csv(cohort_wet_path, show_col_types = FALSE) |>
  mutate(treatment = "Treatment_Wet")

cohort_dry <- read_csv(cohort_dry_path, show_col_types = FALSE) |>
  mutate(treatment = "Treatment_Dry")

cohorts <- bind_rows(cohort_wet, cohort_dry)

stats_wet <- read_csv(stats_wet_path, show_col_types = FALSE) |>
  mutate(treatment = "Treatment_Wet")

stats_dry <- read_csv(stats_dry_path, show_col_types = FALSE) |>
  mutate(treatment = "Treatment_Dry")

stats_all <- bind_rows(stats_wet, stats_dry)

## ---- map species codes to anonymized names ----------------------------
sp_lut <- tibble(
  sp_code = c(1, 2, 3, 4, 5, 6, 7),
  sp_name = c(
    "Species_A",
    "Species_B",
    "Species_C",
    "Species_D",
    "Species_E",
    "Species_F",
    "Species_G"
  )
)

cohorts <- cohorts |>
  mutate(sp = as.integer(sp)) |>
  left_join(sp_lut, by = c("sp" = "sp_code"))

## ---- compute trees/ha and BA/ha ---------------------------------------
## Treatment_Wet: 1.50 ha (plots 101, 102, 103)
## Treatment_Dry: 1.00 ha (plots 201, 202)
cohorts <- cohorts |>
  mutate(
    plot_area_ha = if_else(treatment == "Treatment_Wet", 1.50, 1.00),
    trees_ha     = n / plot_area_ha,
    ba_ind       = pi * (dbh / 200)^2,   # dbh (cm) -> radius (m), area per tree
    ba_ha        = ba_ind * trees_ha
  )

## ======================================================================
## 1. Species composition over time
## ======================================================================

sp_time <- cohorts |>
  group_by(treatment, time, sp_name) |>
  summarise(
    ba_ha    = sum(ba_ha, na.rm = TRUE),
    trees_ha = sum(trees_ha, na.rm = TRUE),
    .groups  = "drop"
  )

# Identify top 5 species by maximum basal area across treatments and time
top_sp <- sp_time |>
  group_by(sp_name) |>
  summarise(max_ba = max(ba_ha, na.rm = TRUE), .groups = "drop") |>
  slice_max(max_ba, n = 5) |>
  pull(sp_name)

sp_time_top <- sp_time |>
  filter(sp_name %in% top_sp)

p_species_BA <- ggplot(
  sp_time_top,
  aes(x = time, y = ba_ha, colour = sp_name)
) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ treatment, nrow = 1) +
  labs(
    x      = "Year",
    y      = "Basal area (m²/ha)",
    colour = "Species"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(size = 12),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    legend.position    = "bottom"
  )

ggsave(
  file.path(out_dir, "species_BA_top5.png"),
  p_species_BA, width = 10, height = 5, dpi = 300
)

## ======================================================================
## 2. Size structure – DBH classes
## ======================================================================

dbh_breaks <- c(0, 10, 20, 30, 40, 60, 80, Inf)
dbh_labels <- c("0-10", "10-20", "20-30", "30-40", "40-60", "60-80", ">80")

cohorts_sizes <- cohorts |>
  mutate(
    dbh_class = cut(
      dbh,
      breaks = dbh_breaks,
      labels = dbh_labels,
      right  = FALSE
    )
  ) |>
  group_by(treatment, time, dbh_class) |>
  summarise(
    ba_ha    = sum(ba_ha, na.rm = TRUE),
    trees_ha = sum(trees_ha, na.rm = TRUE),
    .groups  = "drop"
  )

years_available <- sort(unique(cohorts_sizes$time))
target_years    <- c(min(years_available), max(years_available))

size_plot_data <- cohorts_sizes |>
  filter(time %in% target_years) |>
  mutate(time = factor(time))

year_cols <- c("#2b8cbe", "#fdae6b")

# Basal area per DBH class
p_size_BA_bar <- ggplot(
  size_plot_data,
  aes(x = dbh_class, y = ba_ha, fill = time)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    colour   = "black",
    width    = 0.75
  ) +
  facet_wrap(~ treatment, nrow = 1) +
  scale_fill_manual(values = year_cols, name = "Year") +
  labs(
    x = "DBH class (cm)",
    y = "Basal area (m²/ha)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(size = 12),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    legend.position    = "top"
  )

ggsave(
  file.path(out_dir, "size_structure_BA_bar.png"),
  p_size_BA_bar, width = 10, height = 5, dpi = 300
)

# Trees per ha per DBH class
p_size_trees_bar <- ggplot(
  size_plot_data,
  aes(x = dbh_class, y = trees_ha, fill = time)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    colour   = "black",
    width    = 0.65
  ) +
  facet_wrap(~ treatment, nrow = 1) +
  scale_fill_manual(values = year_cols, name = "Year") +
  labs(
    x = "DBH class (cm)",
    y = "Trees per ha"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(size = 12),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    legend.position    = "top"
  )

ggsave(
  file.path(out_dir, "size_structure_trees_bar.png"),
  p_size_trees_bar, width = 10, height = 5, dpi = 300
)

## ======================================================================
## 3. Stand-level metrics from stats_out
## ======================================================================

treat_cols <- c("Treatment_Wet" = "#2b8cbe", "Treatment_Dry" = "#31a354")

p_stand_BA <- ggplot(
  stats_all,
  aes(x = time, y = ba, colour = treatment)
) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = treat_cols, name = "Treatment") +
  labs(
    x = "Year",
    y = "Basal area (m²/ha)"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    legend.position    = "top"
  )

ggsave(
  file.path(out_dir, "stand_BA.png"),
  p_stand_BA, width = 7, height = 5, dpi = 300
)

p_stand_ntrees <- ggplot(
  stats_all,
  aes(x = time, y = numtrees, colour = treatment)
) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = treat_cols, name = "Treatment") +
  labs(
    x     = "Year",
    y     = "Number of trees",
    title = "Stand density by treatment"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    legend.position    = "top"
  )

ggsave(
  file.path(out_dir, "stand_numtrees.png"),
  p_stand_ntrees, width = 6, height = 5, dpi = 300
)

## ======================================================================
## 4. Summary table (first vs last year)
## ======================================================================

summary_years <- c(min(stats_all$time), max(stats_all$time))

summary_first_last <- stats_all |>
  filter(time %in% summary_years) |>
  select(treatment, time, ba, numtrees, timb_vol, maxdbh, Dstar)

write_csv(
  summary_first_last,
  file.path(out_dir, "stand_summary_first_last_year.csv")
)

## ======================================================================
## End of script
## ======================================================================

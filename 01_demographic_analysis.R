
# Script: 01_data_cleaning_and_demographic_rates.R
#Forest demographic analysis
# Author: Sabrina Akter
# Description:
#   - Species recoding and plot-type classification
#   - Plot types:
#       * Treatment_Wet plots: 101, 102, 103
#       * Treatment-dry plots: 201, 202
#   - Identification of dead trees and recruits
#   - Estimation of species-level growth, mortality, and recruitment rates
#       * Species-level, canopy-specific (Canopy 1 = overstory, Canopy 2 = understory)
#       * Plot-specific, converted to per-hectare values
#   - Preparation of PPA initial state files (dbh ≥ 5 cm)
#   - Graphical summaries for flooded and non-flooded plots by species and canopy layer

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

###############################################################################
# 1. Setup
###############################################################################

output_dir <-"outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

graph_dir <- file.path(output_dir, "demographic_graphs")
if (!dir.exists(graph_dir)) dir.create(graph_dir, recursive = TRUE)

input_path <- file.path("data", "forest_inventory_data.xlsx")

species_order <- c("Species_A", "Species_B", "Species_C", "Species_D", "Species_E", "Species_F", "Species_G")

###############################################################################
# 2. Data preparation
###############################################################################

data_all <- suppressWarnings({
  data %>%
    mutate(
      Species = case_when(
        Species_raw == "Raw_Sp_A" ~ "Species_A",
        Species_raw == "Raw_Sp_B" ~ "Species_B",
        Species_raw == "Raw_Sp_C" ~ "Species_C",
        Species_raw == "Raw_Sp_D" ~ "Species_D",
        Species_raw == "Raw_Sp_E" ~ "Species_E",
        Species_raw %in% c("Raw_Sp_F1", "Raw_Sp_F2") ~ "Species_F",
        Species_raw %in% c("Raw_Sp_G1", "Raw_Sp_G2") ~ "Species_G",
        TRUE ~ NA_character_
      ),
      PlOTID_num = as.numeric(as.character(PlOTID)),
      PlotType = case_when(
        PlOTID_num %in% c(101, 102, 103) ~ "Treatment_Wet",
        PlOTID_num %in% c(201, 202)     ~ "Treatment_Dry",
        TRUE                          ~ NA_character_
      ),
      Canopy_clean = case_when(
        as.character(Canopy) %in% c("1", "2") ~ as.numeric(Canopy),
        TRUE                                 ~ NA_real_
      ),
      DBH_2020_num    = as.numeric(DBH_2020),
      DBH_t2_num      = as.numeric(DBH_t2),
      Years_num       = as.numeric(Years),
      Status_Code_clean = as.character(Status_Code)
    ) %>%
    filter(!is.na(Species), !is.na(PlotType))
})

###############################################################################
# 3. Dead indicator
###############################################################################

data_all <- data_all %>%
  mutate(
    Dead_code = ifelse(
      Status_Code %in% c("Dead_Standing", "Dead_Fallen"),
      1, 0
    ),
    Dead_dbh = ifelse(
      !is.na(DBH_2020_num) & DBH_2020_num > 0 &
        (is.na(DBH_t2_num) | DBH_t2_num <= 0),
      1, 0
    ),
    Dead = ifelse(Dead_code == 1 | Dead_dbh == 1, 1, 0)
  )

###############################################################################
# 4. Recruitment definition and calculation
###############################################################################

is_recruit <- function(row) {
  (
    (is.na(row$DBH_2020_num) | row$DBH_2020_num <= 0) &
      !is.na(row$DBH_t2_num) & row$DBH_t2_num > 0
  ) |
    grepl("New_Recruit", row$Status_Code)
}

calc_recruitment <- function(data, plot_ids, area_ha) {
  recruits <- data %>%
    filter(PlOTID_num %in% plot_ids, is_recruit(.))
  
  mean_years <- mean(recruits$Years_num, na.rm = TRUE)
  
  rec_df <- recruits %>%
    group_by(Species) %>%
    summarise(n_recruits = n(), .groups = "drop") %>%
    right_join(data.frame(Species = species_order), by = "Species") %>%
    mutate(
      n_recruits = ifelse(is.na(n_recruits), 0, n_recruits),
      rec_ha_yr  = round(n_recruits / (area_ha * mean_years), 4)
    )
  
  rec_df
}

recruit_101_102 <- calc_recruitment(data_all, c(101, 102), 1)
recruit_103    <- calc_recruitment(data_all, 103,        0.50)
recruit_201_202 <- calc_recruitment(data_all, c(201, 202), 1)

###############################################################################
# 5. Subset for growth and mortality (canopy 1–2, by plot type)
###############################################################################

data_growth <- data_all %>%
  filter(!is.na(Canopy_clean), Canopy_clean %in% c(1, 2))

data_Group_A    <- data_growth %>% filter(PlotType == "Treatment_Wet")
data_Group_B<- data_growth %>% filter(PlotType == "Treatment_Dry")

###############################################################################
# 6. Standardized growth and mortality rates
###############################################################################

calc_standardized_rates <- function(data) {
  
  # Growth: only living trees with valid DBH at both censuses
  growth_data <- data %>%
    filter(
      !is.na(DBH_2020_num), DBH_2020_num > 0,
      !is.na(DBH_t2_num),   DBH_t2_num   > 0,
      Dead == 0,
      !is.na(Years_num), Years_num > 0
    ) %>%
    mutate(
      AnnualGrowth    = (DBH_t2_num - DBH_2020_num) / Years_num,
      growth_in_range = AnnualGrowth >= -1 & AnnualGrowth <= 1
    )
  
  growth_filtered <- growth_data %>%
    filter(growth_in_range)
  
  g1 <- growth_filtered %>%
    filter(Canopy_clean == 1) %>%
    group_by(Species) %>%
    summarise(G1 = mean(AnnualGrowth, na.rm = TRUE), .groups = "drop")
  
  g2 <- growth_filtered %>%
    filter(Canopy_clean == 2) %>%
    group_by(Species) %>%
    summarise(G2 = mean(AnnualGrowth, na.rm = TRUE), .groups = "drop")
  
  # Mortality: trees at risk (DBH_2020 > 0) with valid interval
  mort_data <- data %>%
    filter(!is.na(DBH_2020_num), DBH_2020_num > 0,
           !is.na(Years_num), Years_num > 0)
  
  mu1 <- mort_data %>%
    filter(Canopy_clean == 1) %>%
    group_by(Species, Years_num) %>%
    summarise(
      n_dead  = sum(Dead),
      n_total = n(),
      .groups = "drop"
    ) %>%
    mutate(
      mu_raw    = n_dead / n_total,
      mu_annual = ifelse(mu_raw > 0, 1 - (1 - mu_raw)^(1 / Years_num), 0)
    ) %>%
    group_by(Species) %>%
    summarise(mu1 = mean(mu_annual, na.rm = TRUE), .groups = "drop")
  
  mu2 <- mort_data %>%
    filter(Canopy_clean == 2) %>%
    group_by(Species, Years_num) %>%
    summarise(
      n_dead  = sum(Dead),
      n_total = n(),
      .groups = "drop"
    ) %>%
    mutate(
      mu_raw    = n_dead / n_total,
      mu_annual = ifelse(mu_raw > 0, 1 - (1 - mu_raw)^(1 / Years_num), 0)
    ) %>%
    group_by(Species) %>%
    summarise(mu2 = mean(mu_annual, na.rm = TRUE), .groups = "drop")
  
  rates_df <- data.frame(Species = species_order) %>%
    left_join(g1,  by = "Species") %>%
    left_join(g2,  by = "Species") %>%
    left_join(mu1, by = "Species") %>%
    left_join(mu2, by = "Species") %>%
    mutate(
      G1  = round(ifelse(is.na(G1),  0, G1),  4),
      G2  = round(ifelse(is.na(G2),  0, G2),  4),
      mu1 = round(ifelse(is.na(mu1), 0, mu1), 4),
      mu2 = round(ifelse(is.na(mu2), 0, mu2), 4)
    )
  
  list(
    rates      = rates_df,
    growth_all = growth_data,
    growth_use = growth_filtered
  )
}

results_Treatment_A   <- calc_standardized_rates(data_Group_A)
results_Treatment_B <- calc_standardized_rates(data_Group_B)

rates_Group_A   <- results_Group_A$rates
rates_Group_B<- results_Group_B$rates

###############################################################################
# 7. Demographic rate tables (Treatmet-wet vs Treatment-Dry)
###############################################################################

rates_table_Wet <- rates_Group_A%>%
  left_join(recruit_101_102 %>% select(Species, rec_101102 = rec_ha_yr), by = "Species") %>%
  left_join(recruit_103     %>% select(Species, rec_103   = rec_ha_yr), by = "Species") %>%
  mutate(
    rec_ha_yr = round(rec_101102 * (1 /1.50) + rec_103 * (0.50 /1.50), 4),
    rec_ha_yr = round(ifelse(is.na(rec_ha_yr), 0, rec_ha_yr), 4)
  ) %>%
  select(Species, G1, G2, mu1, mu2, rec_ha_yr)

rates_table_Dry <- rates_Group_B%>%
  left_join(recruit_201_202 %>% select(Species, rec_ha_yr), by = "Species") %>%
  mutate(rec_ha_yr = round(ifelse(is.na(rec_ha_yr), 0, rec_ha_yr), 4)) %>%
  select(Species, G1, G2, mu1, mu2, rec_ha_yr)

write.csv(
  rates_table_Wet,
  file.path(output_dir, "demographic_rates_treatment_wet.csv"),
  row.names = FALSE
)

write.csv(
  rates_table_Dry,
  file.path(output_dir, "demographic_rates_treatment_dry.csv"),
  row.names = FALSE
)
###############################################################################
# 8. Growth outlier plots (per species, canopy, plot type)
###############################################################################

canopy_levels <- c(1, 2)

for (sp in species_order) {
  for (can in canopy_levels) {
    
    ## ---------------------- Treatment_Wet ---------------------------------------
    plot_data_f <- results_Treatment_A$growth_alll %>%
      filter(Species == sp, Canopy_clean == can)
    
    # Produce a plot whenever at least 1 valid growth record exists
    if (nrow(plot_data_f) >= 1) {
      p_f <- ggplot(plot_data_f,
                    aes(x = DBH_2020_num,
                        y = AnnualGrowth,
                        colour = growth_in_range)) +
        geom_point(size = 2.5, alpha = 0.8) +
        geom_hline(yintercept = c(-1, 1),
                   linetype = "dashed",
                   colour = "red",
                   linewidth = 0.7) +
        scale_colour_manual(
          values = c(`TRUE` = "black", `FALSE` = "grey70"),
          labels = c(
            `TRUE`  = "Within [-1, 1] cm/year",
            `FALSE` = "Outside [-1, 1] cm/year"
          )
        ) +
        labs(
          title  = paste0(sp, " – Canopy ", can, " – Treatment_Wet"),
          x      = "Initial DBH 2020 (cm)",
          y      = "Annual growth (cm year\u207b\u00b9)",
          colour = "Growth class"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          plot.title      = element_text(hjust = 0.5, face = "bold")
        )
      
      ggsave(
        filename = file.path(
          graph_dir,
          paste0("growth_outliers_", sp, "_C", can, "_Treatment_Wet.png")
        ),
        plot   = p_f,
        width  = 8,
        height = 6,
        dpi    = 300
      )
    }
    
    ## -------------------- Treatment-dry -------------------------------------
    plot_data_nf <- results_Treatment_B$growth_all %>%
      filter(Species == sp, Canopy_clean == can)
    
    if (nrow(plot_data_nf) >= 1) {
      p_nf <- ggplot(plot_data_nf,
                     aes(x = DBH_2020_num,
                         y = AnnualGrowth,
                         colour = growth_in_range)) +
        geom_point(size = 2.5, alpha = 0.8) +
        geom_hline(yintercept = c(-1, 1),
                   linetype = "dashed",
                   colour = "red",
                   linewidth = 0.7) +
        scale_colour_manual(
          values = c(`TRUE` = "black", `FALSE` = "grey70"),
          labels = c(
            `TRUE`  = "Within [-1, 1] cm/year",
            `FALSE` = "Outside [-1, 1] cm/year"
          )
        ) +
        labs(
          title  = paste0(sp, " – Canopy ", can, " – Treatment-dry"),
          x      = "Initial DBH 2020 (cm)",
          y      = "Annual growth (cm year\u207b\u00b9)",
          colour = "Growth class"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          plot.title      = element_text(hjust = 0.5, face = "bold")
        )
      
      ggsave(
        filename = file.path(
          graph_dir,
          paste0("growth_outliers_", sp, "_C", can, "_Treatment_Dry.png")
        ),
        plot   = p_nf,
        width  = 8,
        height = 6,
        dpi    = 300
      )
    }
  }
}

###############################################################################
# 9. Species-level rate graphs
#    Grouped bars for G1/G2 and mu1/mu2, separate for Treatment_Wet / Treatment-dry
###############################################################################

# 9a. GROWTH  -------------------------------------------------------

# Prepare Data
prep_growth_data <- function(df) {
  df %>%
    select(Species, G1, G2) %>%
    pivot_longer(cols = c(G1, G2), names_to = "Canopy", values_to = "Value") %>%
    mutate(
      Canopy = factor(Canopy, levels = c("G1", "G2"), 
                      labels = c("Canopy 1 (overstory)", "Canopy 2 (understory)")),
      Label = ifelse(is.na(Value) | Value == 0, "NA", ""),
      PlotValue = ifelse(is.na(Value), 0, Value)
    )
}

growth_combined <- bind_rows(
  prep_growth_data(rates_table_Wet) %>% mutate(Condition = "Treatment_Wet"),
  prep_growth_data(rates_table_Dry) %>% mutate(Condition = "Treatment-dry")
) %>%
  mutate(Condition = factor(Condition, levels = c("Treatment_Wet", "Treatment-dry")))

col_growth_canopy <- c("Canopy 1 (overstory)" = "#006400", "Canopy 2 (understory)" = "#8FB393")

p_growth_final <- ggplot(growth_combined, aes(x = Species, y = PlotValue, fill = Canopy)) +
  geom_col(position = position_dodge(width = 0.8), colour = "grey30", width = 0.7) +
  geom_text(aes(label = Label, group = Canopy), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, colour = "red", fontface = "bold") +
  scale_fill_manual(values = col_growth_canopy) +
  facet_wrap(~Condition, nrow = 1) + 
  labs(title = NULL, x = "Species", y = expression(paste("Mean annual growth (cm year"^-1, ")"))) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(face = "bold", size = 14)
  )

ggsave(file.path(graph_dir, "growth_rates_final.png"), plot = p_growth_final, width = 12, height = 6)


# 9b. MORTALITY 

prep_mort_data <- function(df) {
  df %>%
    select(Species, mu1, mu2) %>%
    pivot_longer(cols = c(mu1, mu2), names_to = "Canopy", values_to = "Value") %>%
    mutate(
      Canopy = factor(Canopy, levels = c("mu1","mu2"),
                      labels = c("Canopy 1 (overstory)","Canopy 2 (understory)")),
      Label    = ifelse(is.na(Value), "NA", ""),      # ONLY NA
      PlotValue = ifelse(is.na(Value), 0, Value)      # 0 only to place the NA label
    )
}

mort_combined <- bind_rows(
  prep_mort_data(rates_table_Wet) %>% mutate(Condition = "Treatment_Wet"),
  prep_mort_data(rates_table_Dry) %>% mutate(Condition = "Treatment-dry")
) %>% mutate(Condition = factor(Condition, levels = c("Treatment_Wet", "Treatment-dry")))

p_mort <- ggplot(mort_combined, aes(x = Species, y = PlotValue, fill = Canopy)) +
  geom_col(position = position_dodge(width = 0.8), colour = "grey30", width = 0.7) +
  geom_text(aes(label = Label, group = Canopy), 
            position = position_dodge(width = 0.8), 
            vjust = 0, nudge_y = 0.001, size = 3, colour = "red", fontface = "bold") +
  scale_fill_manual(values = c("Canopy 1 (overstory)" = "#b30000", "Canopy 2 (understory)" = "#fb8072")) +
  facet_wrap(~Condition, nrow = 1) + 
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(title = NULL, x = "Species",y = expression(paste("Annual mortality rate (year⁻¹)"))) +
  theme_minimal()

ggsave(file.path(graph_dir, "mortality_rates_final.png"), plot = p_mort, width = 12, height = 6)


# 9c. RECRUITMENT 
rec_combined <- bind_rows(
  rates_table_Wet %>% select(Species, rec_ha_yr) %>% mutate(Condition = "Treatment_Wet"),
  rates_table_Dry %>% select(Species, rec_ha_yr) %>% mutate(Condition = "Treatment-dry")
) %>%
  mutate(
    Condition = factor(Condition, levels = c("Treatment_Wet", "Treatment-dry")),
    PlotValue = rec_ha_yr,
    Label = ifelse(PlotValue == 0, "NA", "")
  )
p_rec_bars <- ggplot(rec_combined, aes(x = Species, y = PlotValue, fill = Condition)) +
  geom_col(position = position_dodge(width = 0.8), colour = "grey30", width = 0.7) +
  
  # Add NA labels
  geom_text(aes(label = Label), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, colour = "red", fontface = "bold") +
  
  scale_fill_manual(values = c("Treatment_Wet" = "#1f78b4", "Treatment-dry" = "#ff7f00")) +
  
  # Free Y-axis
  facet_wrap(~Condition, nrow = 1, scales = "free_y") + 
  
  labs(title = NULL, x = "Species", y = "Recruitment (trees ha⁻¹ year⁻¹)") +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    panel.spacing = unit(3, "lines"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(face = "bold", size = 14)
  )

ggsave(file.path(graph_dir, "recruitment_rates_bars_free_y.png"), plot = p_rec_bars, width = 14, height = 7)


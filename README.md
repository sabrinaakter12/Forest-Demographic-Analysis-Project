## Forest Demography Analysis

R workflow for floodplain forest demography: estimates plot-type and species-specific demographic rates (growth, mortality, recruitment), validates parameters against reference literature, generates initial states for PPA simulation, and visualizes model outputs.

**Author:** Sabrina Akter  
**Tools:** R( tidyr, dplyr, ggplot2 ) 
**Context:** Floodplain Forest Research Project (University of Leipzig)

> **Note on data privacy and placeholders**  
> All species, plot IDs, and treatment labels used in the scripts are **anonymized** (for example, `Species_A`–`Species_G`, `Treatment_Wet`, `Treatment_Dry`, generic plot IDs). The raw forest inventory dataset is **confidential** and not included.However, the code reproduces the complete analysis workflow .

## Project Overview

This repository contains an end-to-end R workflow for analyzing forest inventory data to parameterize and validate the **Perfect Plasticity Approximation (PPA)** simulation model. The project focuses on estimating demographic rates (growth, mortality, and recruitment) for floodplain forest species under different hydrological conditions (**Treatment_Wet** vs. **Treatment_Dry**).

The workflow follows a scientific pipeline: **Estimate Rates** → **Validate against Literature** → **Generate Model Inputs** → **Visualize Simulation Results**.

## Repository Structure

The workflow is organized into four modular scripts :

### 1. `01_demographic_analysis.R`

* **Data Cleaning:** Standardizes species codes (e.g. `Species_A`–`Species_G`) and classifies plots into `Treatment_Wet` and `Treatment_Dry`.
* **Rate Estimation:** Calculates annual growth (cm/yr), mortality probabilities (year⁻¹), and recruitment rates (trees/ha/yr) per species and canopy layer using `dplyr` for each plot type.
* **Outlier Detection:** Visualizes growth outliers using `ggplot2` to ensure biological plausibility of estimated rates.
* **Output:** Saves processed, anonymized demographic rates for validation (for example, `demographic_rates_treatment_wet.csv`, `demographic_rates_treatment_dry.csv`).

### 2. `02_validation_plots.R`

* **Benchmarking:** Compares the locally estimated demographic rates (from Step 1) against reference values from scientific literature provided in an anonymized `Reference_Literature` table.
* **Quality Control:** Visualizes discrepancies in growth, mortality, and recruitment between `Treatment_Wet`, `Treatment_Dry`, and the reference to validate model parameters *before* simulation runs.

### 3. `03_generate_model_inputs.R`

* **Input Generation:** Formats the validated, anonymized data into the specific `.txt` structure required for PPA model parameterization(species, DBH classes, canopy layer, trees per hectare).
* **Filtering:** Filters trees by DBH thresholds (≥ 5 cm) and assigns canopy positions for model start states for `Treatment_Wet` and `Treatment_Dry`.

### 4. `04_visualize_simulation_results.R`

* **Post-Processing:** Analyzes output files from the PPA simulation runs for `Treatment_Wet` and `Treatment_Dry`.
* **Dynamics Visualization:** Creates figures comparing long-term trends in basal area (m²/ha), species composition, and size-class distributions between the two treatments, using the same placeholder species and treatment labels as in the analysis scripts.

# Analysis of CO₂ and Drought Effects on Mediterranean Post-Fire Seeder Shrubs

This repository contains all code, data, and outputs needed to reproduce the analyses in:

**Maya A. Zomer, Bruno Moreira, Jordi Martínez-Vilalta, Juli G. Pausas (2026). Drought response of fire-adapted Mediterranean shrubs under elevated CO₂. *Tree Physiology*, tpag006. https://doi.org/10.1093/treephys/tpag006.**

We investigated how elevated atmospheric CO₂ and drought affect five Mediterranean shrub species characterized by the post-fire seeding life-history strategy. Among the studied species, three are obligate post-fire seeders (non-resprouting: *Cistus albidus*, *Lavandula stoechas*, *Salvia rosmarinus*), while two are facultative post-fire seeders (resprouting: *Anthyllis cytisoides*, *Dorycnium pentaphyllum*). Plants were grown from seed under ambient or elevated CO₂ in experimental greenhouses for eight months, then subjected to a three-phase progressive drought (50%, 30%, 10% soil water content). We measured leaf functional traits (specific leaf area, stomatal density), biomass allocation (above- and belowground biomass, root-to-shoot ratios), water status (midday water potential, osmotic potential), water-fluxes((transpiration) and hydraulic behaviour (whole-plant and leaf-specific hydraulic conductance and conductivity) across all drought phases. 

We addressed three key questions: 
1. For each species, does eCO₂ alter plant traits and resource allocation? 
2. For each species, during drought, does eCO₂ alter plant water status, water fluxes, and hydraulic behavior? 
3. Does the ability to resprout modify the above eCO₂ effects and drought responses (resprouting vs non-resprouting seeders)?

We used linear mixed-effects models (lmer) to analyze species-specific and resprouter trait effects on response variables. Model selection followed a stepwise approach comparing nested models with likelihood ratio tests. Predicted marginal means and pairwise contrasts (with Bonferroni adjustment) were used for post-hoc comparisons.

---

## Workflow Notes

All analyses were run with R version 4.5. This project uses **renv** to manage package dependencies and ensure reproducibility. 

1. Open `CO2-medshrub.Rproj` in RStudio
2. When you first open the project, renv should automatically prompt you to restore packages
   - If not, run: `renv::restore()`
3. Source `scripts/setup.R`, then run numbered scripts in `scripts/` sequentially

The `renv.lock` file ensures all analyses use the same package versions as the original study.


## Folder Overview

**data** 📁
- `raw/` – original CSV data files from greenhouse experiment
- `processed/` – cleaned and processed RDS and CSV data files

**outputs** 📁
- `models/` – saved model objects (RDS): model fits, predictions, emmeans
- `figures/` – publication-ready PNG figures (300 dpi)

**scripts** 📁
- `setup.R` 📄 – data import and variable calculation
- `01_data_processing.R` 📄 – data import and variable calculation
- `02_data_cleaning.R` 📄 – data cleaning
- `03a_models_traits.R` 📄 – models for SLA, stomatal density, carbohydrates
- `03b_models_biomass.R` 📄 – models for biomass allocation (above/belowground, R:S ratio)
- `03c_models_water_status.R` 📄 – models for water potentials (midday, osmotic)
- `03d_models_water_use.R` 📄 – models for transpiration and hydraulic behaviour
- `03e_models_resprout.R` 📄 – models for resprouter trait effects across all variables
- `04a_plots_traits.R` 📄 – plots for SLA and stomatal density
- `04b_plots_biomass.R` 📄 – plots for biomass allocation and leaf area
- `04c_plots_water_status.R` 📄 – plots for water potentials
- `04d_plots_water_use.R` 📄 – plots for transpiration and hydraulic variables
- `05_combined_plots.R` 📄 – combined manuscript figures (Figure 2, Figure 3, Supplementary Figures)
- `06_additional_plots.R` 📄 – additional supplementary material figures

- `CO2-medshrub.Rproj` – RStudio Project file (opens the project root)

- `renv.lock` – records the exact versions of all R packages used in the project; these are restored during the Docker build via renv::restore().

## README Files ##

- `README.md` – (this file) project overview and folder descriptions
- `README_variables.md` – column definitions and units for data files
---



# ─────────────────────────────────────────────────────────────────────
# 01_data_processing.R — Calculate variables from raw data
# ─────────────────────────────────────────────────────────────────────
# Variable calculation for CO2 & drought greenhouse experiment
# Creates: data_CO2_processed (main dataset with calculated variables)
# ─────────────────────────────────────────────────────────────────────

# Load setup ----
source(here::here("scripts", "setup.R"))

# Load all raw data ----
pot_weight <- read.csv(here::here("data", "raw", "pot_weights.csv"),
                       header = TRUE, sep = ",", na.strings = c("", "NA"))

drought_phase_dates <- read.csv(here::here("data", "raw", "pot_drought_phase.csv"),
                                header = TRUE, sep = ",", na.strings = c("", "NA"))

swc_biomass <- read.csv(here::here("data", "raw", "swc_biomass.csv"),
                         header = TRUE, sep = ",", na.strings = c("", "NA"))

leaf_counts <- read.csv(here::here("data", "raw", "leafcounts_height.csv"),
                        header = TRUE, sep = ",", na.strings = c("", "NA"))

leaf_dryweights <- read.csv(here::here("data", "raw", "leaves_dryweights.csv"),
                        header = TRUE, sep = ",", na.strings = c("", "NA"))

leaf_area_scans <- read.csv(here::here("data", "raw", "leafareas_phase1.csv"),
                            header = TRUE, sep = ",", na.strings = c("", "NA"))

water_potential_midday <- read.csv(here::here("data", "raw", "drought_stress_allphases.csv"),
                                   header = TRUE, sep = ",", na.strings = c("", "NA"))


# Convert drought_phase to factor in all datasets ----
if ("drought_phase" %in% names(drought_phase_dates))      drought_phase_dates$drought_phase <- factor(drought_phase_dates$drought_phase)
if ("drought_phase" %in% names(swc_biomass))              swc_biomass$drought_phase <- factor(swc_biomass$drought_phase)
if ("drought_phase" %in% names(leaf_counts))              leaf_counts$drought_phase <- factor(leaf_counts$drought_phase)
if ("drought_phase" %in% names(leaf_dryweights))          leaf_dryweights$drought_phase <- factor(leaf_dryweights$drought_phase)
if ("drought_phase" %in% names(leaf_area_scans))          leaf_area_scans$drought_phase <- factor(leaf_area_scans$drought_phase)
if ("drought_phase" %in% names(water_potential_midday))   water_potential_midday$drought_phase <- factor(water_potential_midday$drought_phase)



# Helper: date parsing ----
parse_any_date <- function(x) {
  as.Date(lubridate::parse_date_time(
    as.character(x),
    orders = c("d/m/Y", "d/m/y", "Y-m-d")
  ))
}

# Standardize dates ----
if ("date" %in% names(pot_weight))           pot_weight$date <- parse_any_date(pot_weight$date)
if ("date" %in% names(drought_phase_dates))  drought_phase_dates$date <- parse_any_date(drought_phase_dates$date)
if ("date" %in% names(swc_biomass))         swc_biomass$date <- parse_any_date(swc_biomass$date)

# ─────────────────────────────────────────────────────────────────────
# 1. Pot weights → water loss → transpiration ----
# ─────────────────────────────────────────────────────────────────────

drought_phase_dates <- drought_phase_dates %>%
  select(species, drought_phase, phase_day, total_day, date)

weight_by_phase <- left_join(pot_weight, drought_phase_dates, by = c("species", "date"))

# Remove accidental index columns
if ("X" %in% names(weight_by_phase))      weight_by_phase$X <- NULL
if ("...1" %in% names(weight_by_phase))   weight_by_phase$...1 <- NULL

# Numeric conversion
weight_by_phase <- weight_by_phase %>%
  mutate(
    start_pot_weight_kg = as.numeric(start_pot_weight_kg),
    result_pot_weight_kg = as.numeric(result_pot_weight_kg)
  ) %>%
  filter(!is.na(start_pot_weight_kg))

# Calculate water lost
water_loss <- weight_by_phase %>%
  arrange(species, greenhouse, plant_no, total_day) %>%
  group_by(species, greenhouse, plant_no) %>%
  mutate(
    water_lost_kg = start_pot_weight_kg - lag(result_pot_weight_kg),
    water_lost_kg = ifelse(water_lost_kg > 0, 0, water_lost_kg),  # Fix weighing errors
    days_since_water = total_day - lag(total_day),
    water_lost_per_day_kg = water_lost_kg / days_since_water,
    water_lost_per_day_kg = round(water_lost_per_day_kg, 2),
    # Estimate SWC from pot weight
    start_swc_estimate = ((start_pot_weight_kg - 6.1) / 4.72) * 100,
    result_swc_estimate = ((result_pot_weight_kg - 6.1) / 4.72) * 100
  ) %>%
  ungroup() %>%
  filter(drought_phase %in% c(1, 2, 3))

# ─────────────────────────────────────────────────────────────────────
# 1b. Standardize SWC estimates using gravimetric measurements ----
# ─────────────────────────────────────────────────────────────────────

swc_measured <- swc_biomass %>%
  select(greenhouse, species, plant_no, plant, date,
         drought_phase, phase_day, total_day,
         resprouter, co2, grav_swc_perc)

water_loss_subset <- water_loss %>%
  select(greenhouse, species, plant_no, date,
         drought_phase, phase_day, total_day,
         start_pot_weight_kg, water.added.l, result_pot_weight_kg,
         water_lost_kg, days_since_water, water_lost_per_day_kg,
         start_swc_estimate, result_swc_estimate)

# Standardize SWC by species using calibration day
standardize_swc_by_species <- function(species_code, calibration_day) {
  calibration_samples <- water_loss_subset %>%
    filter(species == species_code, total_day == calibration_day) %>%
    na.omit()
  
  merged_data <- merge(
    swc_measured, calibration_samples,
    by = c("greenhouse", "species", "plant_no", "date", "drought_phase", "phase_day", "total_day")
  )
  
  merged_data$swc_diff <- merged_data$result_swc_estimate - merged_data$grav_swc_perc
  
  calibration_factors <- merged_data[, c("greenhouse", "species", "plant_no", "plant", 
                                         "resprouter", "co2", "swc_diff")]
  
  standardized_data <- merge(water_loss_subset, calibration_factors, 
                             by = c("greenhouse", "species", "plant_no"))
  
  standardized_data$start_swc_standardized <- standardized_data$start_swc_estimate - standardized_data$swc_diff
  standardized_data$result_swc_standardized <- standardized_data$result_swc_estimate - standardized_data$swc_diff
  
  standardized_data
}

# Apply calibration by species
swc_standardized_acy <- standardize_swc_by_species("ACY", 79)
swc_standardized_dpe <- standardize_swc_by_species("DPE", 81)
swc_standardized_cal <- standardize_swc_by_species("CAL", 80)
swc_standardized_rof <- standardize_swc_by_species("ROF", 85)
swc_standardized_lst <- standardize_swc_by_species("LST", 82)

water_loss_transpiration <- bind_rows(
  swc_standardized_acy, swc_standardized_dpe, swc_standardized_cal, 
  swc_standardized_rof, swc_standardized_lst
) %>%
  mutate(transpiration_g_day = -water_lost_per_day_kg * 1000)  # kg → g

# Write final output ----
write.csv(water_loss_transpiration, here::here("data", "raw", "swc_estimates.csv"), row.names = FALSE, na = "")

# ─────────────────────────────────────────────────────────────────────
# 2. Leaf biomass estimation ----
# ─────────────────────────────────────────────────────────────────────

biomass_phase3 <- swc_biomass %>%
  mutate(
    co2 = factor(co2, levels = c("ambient", "elevated")),
    leaves_biomass_phase3 = leaves_biomass
  ) %>%
  select(plant, drought_phase, leaves_biomass_phase3)

leaf_data_all_phases <- merge(leaf_counts, biomass_phase3, 
                              by = c("plant", "drought_phase"), all = TRUE) %>%
  mutate(
    total_leaves_count_marked = as.numeric(total_leaves_count_marked)
  )

# Estimate leaf biomass for phases 1 and 2 from phase 3
leaf_biomass_estimated <- leaf_data_all_phases %>%
  group_by(species, greenhouse, plant_no, plant, co2) %>%
  mutate(
    leaves_biomass = case_when(
      drought_phase == "1" ~ leaves_biomass_phase3[drought_phase == "3"] *
        (total_leaves_count_marked[drought_phase == "1"] / total_leaves_count_marked[drought_phase == "3"]),
      drought_phase == "2" ~ leaves_biomass_phase3[drought_phase == "3"] *
        (total_leaves_count_marked[drought_phase == "2"] / total_leaves_count_marked[drought_phase == "3"]),
      drought_phase == "3" ~ leaves_biomass_phase3,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  group_by(plant) %>%
  mutate(
    # Percentage change from phase 1 to phase 2
    percentage_biomass_change_1_2 =
      ((leaves_biomass[drought_phase == "2"] - leaves_biomass[drought_phase == "1"]) /
         leaves_biomass[drought_phase == "1"]) * 100,
    # Percentage change from phase 2 to phase 3
    percentage_biomass_change_2_3 =
      ((leaves_biomass[drought_phase == "3"] - leaves_biomass[drought_phase == "2"]) /
         leaves_biomass[drought_phase == "2"]) * 100
  ) %>%
  ungroup()

leaf_biomass_estimated$percentage_biomass_change_1_2[!is.finite(leaf_biomass_estimated$percentage_biomass_change_1_2)] <- NA
leaf_biomass_estimated$percentage_biomass_change_2_3[!is.finite(leaf_biomass_estimated$percentage_biomass_change_2_3)] <- NA

leaf_biomass_estimated <- leaf_biomass_estimated[, c("greenhouse", "species", "plant_no", "plant", "co2",
                                                     "drought_phase", "percentage_biomass_change_1_2", 
                                                     "percentage_biomass_change_2_3", "leaves_biomass")]

#replace Inf and Nan with Na's
leaf_biomass_estimated[sapply(leaf_biomass_estimated, is.infinite)] <- NA
leaf_biomass_estimated[sapply(leaf_biomass_estimated, is.nan)] <- NA


# ─────────────────────────────────────────────────────────────────────
# 3. SLA and leaf area estimation ----
# ─────────────────────────────────────────────────────────────────────

# Leaf dry weights (scan data only in phase 1)
leaf_dry_weights <- leaf_dryweights %>%
  select(-marked_stem_dry_weight, -marked_leaves_dry_weight)  # Remove columns not needed for SLA

# Leaf area from scans (phase 1 only)
leaf_area_measured <- leaf_area_scans %>%
  mutate(
    leaf_area_mm2_mean = as.numeric(Average.Size)
  ) %>%
  select(co2, greenhouse, species, plant_no, drought_phase, leaf_area_mm2_mean)

# Calculate SLA (for phase 1 only)
leaf_traits_phase1 <- merge(leaf_area_measured, leaf_dry_weights, 
                            by = c("greenhouse", "species", "plant_no", "co2", "drought_phase")) %>%
  mutate(
    leaf_dry_weight_mg = scanned_leaves_dry_weight * 1000,
    leaf_dry_weight_mg_mean = leaf_dry_weight_mg / number_scanned_leaves_DW,
    SLA = leaf_area_mm2_mean / leaf_dry_weight_mg_mean
  ) %>%
  select(plant, leaf_area_mm2_mean, leaf_dry_weight_mg_mean, SLA)

# Merge phase 1 scan data with ALL phases of leaf biomass
# This applies phase 1 ratios to calculate leaf area for all phases
leaf_area_biomass <- leaf_biomass_estimated %>%
  left_join(leaf_traits_phase1, by = "plant") %>%
  mutate(
    whole_plant_leaf_area_mm2 = leaves_biomass * 1000 * SLA,
    whole_plant_leaf_area_cm2 = (leaves_biomass * 1000 * SLA) / 100
  )

# ─────────────────────────────────────────────────────────────────────
# 4. Soil water potential (Ysoil) ----
# ─────────────────────────────────────────────────────────────────────

# Phase 3 measured ysoil
ysoil_phase3 <- swc_biomass[, c("greenhouse", "species", "plant_no", "plant", "co2", "resprouter", "drought_phase",
                                      "vol_swc.cm3.cm3.", "ysoil_bc.kpa.")] 


# Plant height
plant_height <- leaf_counts %>%
  select(plant, plant_height) %>%
  mutate(plant_height = as.numeric(plant_height)) %>%
  na.omit()

# Midday water potentials
water_potential_data <- water_potential_midday[, c("greenhouse", "species", "plant_no", "plant", "co2", "resprouter",
                                                   "drought_phase", "phase_day", "total_day",
                                                   "water_potential_mpa", "osmotic_potential_mpa")]


# Mean transpiration per plant per phase
transpiration_mean <- water_loss_transpiration %>%
  group_by(plant, drought_phase) %>%
  summarize(transpiration_mean_g_day = mean(transpiration_g_day, na.rm = TRUE), .groups = "drop") %>%
  filter(drought_phase %in% c(1, 2, 3))

# Estimate ysoil for phases 1 and 2
water_loss_transpiration$result_swc_g_per_g <- water_loss_transpiration$result_swc_standardized / 100
bulk_density <- swc_biomass[, c("greenhouse", "species", "plant_no", "plant", "pot_bulk_density_g.cm3")]

# Extract samples on calibration days by species
ysoil_phase1_samples <- bind_rows(
  filter(water_loss_transpiration, species == "ACY", total_day == 23),
  filter(water_loss_transpiration, species == "DPE", total_day == 19),
  filter(water_loss_transpiration, species == "CAL", total_day == 22),
  filter(water_loss_transpiration, species == "LST", total_day == 20),
  filter(water_loss_transpiration, species == "ROF", total_day == 21)
)

ysoil_phase2_samples <- bind_rows(
  filter(water_loss_transpiration, species == "ACY", total_day == 47),
  filter(water_loss_transpiration, species == "DPE", total_day == 49),
  filter(water_loss_transpiration, species == "CAL", total_day == 50),
  filter(water_loss_transpiration, species == "LST", total_day == 51),
  filter(water_loss_transpiration, species == "ROF", total_day == 48)
)

# Calculate ysoil for phases 1 and 2
calculate_ysoil <- function(samples) {
  samples %>%
    left_join(bulk_density, by = c("species", "greenhouse", "plant_no", "plant")) %>%
    select(species, greenhouse, plant_no, plant, co2, drought_phase,
           pot_bulk_density_g.cm3, result_swc_g_per_g) %>%
    mutate(
      vol_swc_cm3_per_cm3 = result_swc_g_per_g * pot_bulk_density_g.cm3,
      ysoil_kpa = -0.464 * (vol_swc_cm3_per_cm3 / 0.7213)^(-3.315)
    )
}

ysoil_phase1 <- calculate_ysoil(ysoil_phase1_samples)
ysoil_phase2 <- calculate_ysoil(ysoil_phase2_samples)

# Merge water relations data
water_relations_temp <- merge(water_potential_data, transpiration_mean, by = c("plant", "drought_phase")) %>%
  merge(plant_height, by = "plant") %>%
  select(-phase_day, -total_day)

# Merge by phase
water_relations_phase1 <- merge(water_relations_temp, ysoil_phase1,
                                by = c("greenhouse", "species", "plant_no", "plant", "co2", "drought_phase")) %>%
  select(-pot_bulk_density_g.cm3, -result_swc_g_per_g)

water_relations_phase2 <- merge(water_relations_temp, ysoil_phase2,
                                by = c("greenhouse", "species", "plant_no", "plant", "co2", "drought_phase")) %>%
  select(-pot_bulk_density_g.cm3, -result_swc_g_per_g)

water_relations_phase3 <- merge(water_relations_temp, ysoil_phase3,
                                by = c("greenhouse", "species", "plant_no", "plant", "co2", "resprouter", "drought_phase")) %>%
  rename(
    ysoil_kpa = ysoil_bc.kpa.,
    vol_swc_cm3_per_cm3 = vol_swc.cm3.cm3.
  )

# Combine all phases
water_relations_all <- bind_rows(water_relations_phase1, water_relations_phase2, water_relations_phase3) %>%
  filter(plant != "ECAL2W") %>%  # Remove plant with no soil data
  mutate(water_potential_midday_kpa = water_potential_mpa * 1000)

# ─────────────────────────────────────────────────────────────────────
# 5. Hydraulics calculations ----
# ─────────────────────────────────────────────────────────────────────

leaf_area_subset <- leaf_area_biomass[, c("plant", "drought_phase", "whole_plant_leaf_area_mm2","whole_plant_leaf_area_cm2", "leaves_biomass", "SLA", "percentage_biomass_change_1_2", 
                                          "percentage_biomass_change_2_3")]

hydraulics_data <- merge(water_relations_all, leaf_area_subset, by = c("plant", "drought_phase")) %>%
  mutate(
    # Whole plant hydraulic conductance and conductivity
    whole_plant_conductance = transpiration_mean_g_day / (ysoil_kpa - water_potential_midday_kpa),
    whole_plant_conductivity = (transpiration_mean_g_day * plant_height) / (ysoil_kpa - water_potential_midday_kpa),
    # Fix negative values
    whole_plant_conductance = ifelse(whole_plant_conductance < 0, 0.001, whole_plant_conductance),
    whole_plant_conductivity = ifelse(whole_plant_conductivity < 0, 0.05, whole_plant_conductivity),
    # Leaf-specific variables
    leaf_specific_conductance = whole_plant_conductance / whole_plant_leaf_area_cm2,
    leaf_specific_conductivity = whole_plant_conductivity / whole_plant_leaf_area_cm2,
    leaf_specific_transpiration = transpiration_mean_g_day / whole_plant_leaf_area_cm2
  )

# Add biomass data
biomass_data <- swc_biomass[, c("greenhouse", "species", "plant_no", "plant", "co2", "resprouter", "root_shoot_ratio", "stems_biomass",
                                "total_aboveground_biomass", "total_belowground_biomass")]

data_final <- merge(hydraulics_data, biomass_data,
                    by = c("greenhouse", "species", "plant_no", "plant", "co2", "resprouter")) %>%
  mutate(
    drought_phase = factor(drought_phase),
    species = factor(species, levels = c("ACY", "DPE", "LST", "CAL", "ROF")),
    resprouter = factor(resprouter),
    co2 = factor(co2)
  ) 

# Final data cleaning ----
# Set SLA to NA for phases 2 and 3 (only measured in phase 1)
data_final$SLA[data_final$drought_phase %in% c(2, 3)] <- NA_real_

# Set plant_height to NA for phases 2 and 3 (only measured in phase 1)
data_final$plant_height[data_final$drought_phase %in% c(2, 3)] <- NA_real_

# Set biomass variables to NA for phases 1 and 2 (only measured in phase 3)
data_final$root_shoot_ratio[data_final$drought_phase %in% c(1, 2)] <- NA_real_
data_final$stems_biomass[data_final$drought_phase %in% c(1, 2)] <- NA_real_
data_final$total_aboveground_biomass[data_final$drought_phase %in% c(1, 2)] <- NA_real_
data_final$total_belowground_biomass[data_final$drought_phase %in% c(1, 2)] <- NA_real_

# Set percentage biomass change to appropriate phases
data_final$percentage_biomass_change_1_2[data_final$drought_phase %in% c(1, 3)] <- NA_real_  # Only in phase 2
data_final$percentage_biomass_change_2_3[data_final$drought_phase %in% c(1, 2)] <- NA_real_  # Only in phase 3


# Update species code ROF → SRO 
data_final$species <- as.character(data_final$species)
data_final$species[data_final$species == "ROF"] <- "SRO"
data_final$species <- factor(data_final$species, levels = c("ACY", "DPE", "LST", "CAL", "SRO"))


# Write final output ----
write.csv(data_final, here::here("data", "processed", "data_processed_combined.csv"), row.names = FALSE, na = "")

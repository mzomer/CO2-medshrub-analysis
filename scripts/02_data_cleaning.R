# ─────────────────────────────────────────────────────────────────────
# 02_data_cleaning.R — Data wrangling for CO2 greenhouse experiment
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load Data ----
data_main <- read.csv(here::here("data", "processed", "data_processed_combined.csv"), 
                      header = TRUE, sep = ",", 
                      na.strings = c("", "NA"))

data_stom <- read.csv(here::here("data", "raw", "stomatal_density.csv"), 
                      header = TRUE, sep = ",", 
                      na.strings = c("", "NA"))

data_carbs <- read.csv(here::here("data", "raw","carbohydrates.csv"), 
                       header = TRUE, sep = ",", 
                       na.strings = c("", "NA"))

# Data cleanup ----
# Species codes already updated (SRO) in processing script
# Update stomatal density data
data_stom[data_stom == "ROF"] <- "SRO"
data_carbs[data_carbs == "ROF"] <- "SRO"  # Add this line

# Convert to factors
data_main <- data_main %>%
  mutate(
    across(c(co2, greenhouse, resprouter, drought_phase), as.factor),
    species = factor(species, levels = c("ACY", "DPE", "LST", "CAL", "SRO")),
    drought_phase = ordered(drought_phase),
    # Recreate plant ID with updated species codes
    plant = paste0(greenhouse, species, plant_no)  # replace rof with sro in plant id 
  )

# Remove outliers
# CSRO1W (formerly CROF1W) - soil data collection issues
# BDPE1W - missing leaf biomass  
# ESRO4W (formerly EROF4W) - problematic osmotic potential value
data_main <- data_main %>%
  mutate(
    whole_plant_conductance = ifelse(plant == "CSRO1W", NA, whole_plant_conductance),
    whole_plant_conductivity = ifelse(plant == "CSRO1W", NA, whole_plant_conductivity),
    leaf_specific_conductance = ifelse(plant == "CSRO1W", NA, leaf_specific_conductance),
    leaf_specific_conductivity = ifelse(plant == "CSRO1W", NA, leaf_specific_conductivity),
    total_aboveground_biomass = ifelse(plant == "BDPE1W", NA, total_aboveground_biomass),
    root_shoot_ratio = ifelse(plant == "BDPE1W", NA, root_shoot_ratio),
    osmotic_potential_mpa = ifelse(plant == "ESRO4W", NA, osmotic_potential_mpa)
  )

# Log transformations ----
data_main <- data_main %>%
  mutate(
    # Biomass
    total_aboveground_biomass_log = log(total_aboveground_biomass),
    total_belowground_biomass_log = log(total_belowground_biomass),
    root_shoot_ratio_log = log(root_shoot_ratio),
    leaves_biomass_log = log(leaves_biomass),
    stems_biomass_log = log(stems_biomass),
    whole_plant_leaf_area_log = log(whole_plant_leaf_area_cm2),
    # Water status
    water_potential_midday_log = log(-water_potential_mpa),
    osmotic_potential_log = log(-osmotic_potential_mpa),
    # Water use
    transpiration_mean_log = log(transpiration_mean_g_day),
    whole_plant_conductance_log = log(whole_plant_conductance),
    whole_plant_conductivity_log = log(whole_plant_conductivity),
    leaf_specific_transpiration_log = log(leaf_specific_transpiration),
    leaf_specific_conductance_log = log(leaf_specific_conductance),
    leaf_specific_conductivity_log = log(leaf_specific_conductivity)
  )

# Stomatal density data ----
# mean and sd density for each plant (3-5 leaves per plant)
data_stomatal_density <- data_stom %>%
  mutate(
    co2 = as.factor(co2),
    species = factor(species, levels = c("ACY", "DPE", "LST", "CAL", "SRO")),
    # Create plant ID to match main dataset
    plant = paste0(greenhouse, species, plant_no)
  ) %>%
  group_by(plant, species, plant_no, greenhouse,  co2) %>%
  summarise(
    sd_stomatal_density = sd(StoD_stomata_mm2, na.rm = TRUE),
    mean_stomatal_density = mean(StoD_stomata_mm2, na.rm = TRUE),
    .groups = "drop"
  )

# Carbohydrates data ----
data_carbohydrates <- data_carbs %>%
  mutate(
    species = factor(species, levels = c("ACY", "DPE", "LST", "CAL", "SRO")),
    # Recreate plant ID with updated species codes
    plant = paste0(greenhouse, species, plant_no),
    # Calculate NSC from new column names
    NSC = starch + solublesugars_total
  ) %>%
  relocate(plant, .after = register_number) %>%  # Move plant to 2nd column
  na.omit()


# Create datasets ----

## Traits & biomass (one-time measurements)
data_traits_biomass <- data_main %>%
  select(plant, species, greenhouse, plant_no, co2, resprouter, drought_phase,
         SLA,
         total_aboveground_biomass, total_belowground_biomass, root_shoot_ratio,
         leaves_biomass, stems_biomass,
         total_aboveground_biomass_log, total_belowground_biomass_log, 
         root_shoot_ratio_log, leaves_biomass_log, stems_biomass_log,
         percentage_biomass_change_1_2, percentage_biomass_change_2_3) %>%
  group_by(plant) %>%
  summarise(
    species = first(species),
    co2 = first(co2),
    greenhouse = first(greenhouse),
    plant_no = first(plant_no), 
    resprouter = first(resprouter),
    SLA = first(na.omit(SLA)),  # Phase 1 only
    # Phase 3 measurements (actually measured at harvest)
    total_aboveground_biomass = total_aboveground_biomass[drought_phase == "3"],
    total_belowground_biomass = total_belowground_biomass[drought_phase == "3"],
    root_shoot_ratio = root_shoot_ratio[drought_phase == "3"],
    leaves_biomass = leaves_biomass[drought_phase == "3"],  
    stems_biomass = stems_biomass[drought_phase == "3"],
    total_aboveground_biomass_log = total_aboveground_biomass_log[drought_phase == "3"],
    total_belowground_biomass_log = total_belowground_biomass_log[drought_phase == "3"],
    root_shoot_ratio_log = root_shoot_ratio_log[drought_phase == "3"],
    leaves_biomass_log = leaves_biomass_log[drought_phase == "3"],  # PHASE 3
    stems_biomass_log = stems_biomass_log[drought_phase == "3"],
    percentage_biomass_change_1_2 = percentage_biomass_change_1_2[drought_phase == "2"],
    percentage_biomass_change_2_3 = percentage_biomass_change_2_3[drought_phase == "3"],
    .groups = "drop"
  )

## Water relations (repeated measurements)
data_water_relations <- data_main %>%
  select(plant, species, plant_no, greenhouse,co2, resprouter, drought_phase,
         water_potential_mpa, water_potential_midday_log,
         osmotic_potential_mpa, osmotic_potential_log,
         transpiration_mean_g_day, transpiration_mean_log,
         whole_plant_conductance, whole_plant_conductance_log,
         whole_plant_conductivity, whole_plant_conductivity_log,
         leaf_specific_transpiration, leaf_specific_transpiration_log,
         leaf_specific_conductance, leaf_specific_conductance_log,
         leaf_specific_conductivity, leaf_specific_conductivity_log,
         whole_plant_leaf_area_cm2, whole_plant_leaf_area_log, leaves_biomass, leaves_biomass_log)
# Check structure
str(data_carbohydrates)
str(data_stomatal_density)
str(data_traits_biomass)
str(data_water_relations)

# Sample sizes
data_main %>% count(co2, drought_phase)

# Save data objects ----
saveRDS(data_carbohydrates, here::here("data", "processed", "data_carbohydrates.rds"))
saveRDS(data_stomatal_density, here::here("data", "processed", "data_stomatal_density.rds"))
saveRDS(data_traits_biomass, here::here("data", "processed", "data_traits_biomass.rds"))
saveRDS(data_water_relations, here::here("data", "processed", "data_water_relations.rds"))

# Save as CSV ----
write.csv(data_carbohydrates, here::here("data", "processed", "data_carbohydrates.csv"), row.names = FALSE, na = "")
write.csv(data_stomatal_density, here::here("data", "processed", "data_stomatal_density.csv"), row.names = FALSE, na = "")
write.csv(data_traits_biomass, here::here("data", "processed", "data_traits_biomass.csv"), row.names = FALSE, na = "")
write.csv(data_water_relations, here::here("data", "processed", "data_water_relations.csv"), row.names = FALSE, na = "")

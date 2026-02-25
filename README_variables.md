# Study design and variable definitions

# This document describes the study design and column variables and units for the processed data files in the `data/processed/` directory.

## Study Design

**Species**: 5 Mediterranean shrubs
- **ACY** (*Anthyllis cytisoides*) – Resprouter (R+)
- **DPE** (*Doryncnium penthaphyllum*) – Resprouter (R+)
- **LST** (*Lavandula stoechas*) – Non-resprouter (R-)
- **CAL** (*Cistus albidus*) – Non-resprouter (R-)
- **SRO** (*Salvia rosmarinus*) – Non-resprouter (R-)

**Treatments**:
- **CO2**: Ambient (~400 ppm) vs. Elevated (~850 ppm)
- **Drought**: Three progressive phases (Phase 1: 50%, Phase 2: 30%, Phase 3: 10% soil water content)

**Key trait**: Resprouting ability (R+ vs. R-)
---

## data_traits_biomass.csv

### Identifiers
- **plant**: Unique plant identifier (text code).
- **species**: Species code (ACY, DPE, LST, CAL, SRO).
- **co2**: CO₂ treatment ("ambient" or "elevated").
- **greenhouse**: Greenhouse number (A,B,C,D,E,F).
- **plant_no**: Plant number within species and greenhouse (integer).
- **resprouter**: Resprouting ability ("R+" = resprouting seeder; "R-" = non-resprouting seeder).

### Leaf traits (measured in drought phase 1)
- **SLA**: Specific leaf area (mm² mg⁻¹).
- **stomatal_density**: Stomatal density (number mm⁻²).

### Biomass allocation (measured in drought phase 3)
- **leaves_biomass**: Total leaf dry biomass (grams, g).
- **stems_biomass**: Total stem dry biomass (grams, g).
- **total_aboveground_biomass**: Total aboveground dry biomass (grams, g).
- **total_belowground_biomass**: Total belowground (root) dry biomass (grams, g).
- **root_shoot_ratio**: Root-to-shoot ratio

### Changes in Leaf Biomass (phase 1 and 2 are estimated values)
- **percentage_biomass_change_1_2**: Percentage change in leaf biomass from phase 1 to phase 2 (%).
- **percentage_biomass_change_2_3**: Percentage change in leaf biomass from phase 2 to phase 3 (%).


### Log-transformed variables
- **leaves_biomass_log**: Natural log of leaves biomass
- **total_aboveground_biomass_log**: Natural log of total aboveground biomass.
- **total_belowground_biomass_log**: Natural log of total belowground biomass.
- **root_shoot_ratio_log**: Natural log of root-to-shoot ratio.

---

## data_water_relations.csv

### Identifiers
- **plant**: Unique plant identifier (text code).
- **species**: Species code (ACY, DPE, LST, CAL, SRO).
- **plant_no**: Plant number within species and greenhouse (integer).
- **greenhouse**: Greenhouse number (A,B,C,D,E,F).
- **co2**: CO₂ treatment ("ambient" or "elevated").
- **resprouter**: Resprouting ability ("R+" or "R-").
- **drought_phase**: Drought phase (factor: "1" = 50% SWC; "2" = 30% SWC; "3" = 10% SWC).


### Water status
- **water_potential_mpa**: Midday leaf water potential (megapascals, MPa).
- **osmotic_potential_mpa**: Osmotic potential (megapascals, MPa).

### Water fluxes
- **transpiration_mean_g_day**: Mean whole-plant transpiration per drought phase (grams per day, g d⁻¹).

### Leaf biomass and area
- **leaves_biomass**: Whole-plant leaf biomass (g).
- **whole_plant_leaf_area_cm2**: Whole-plant leaf area (cm²).

### Hydraulic function
- **whole_plant_conductance**: Whole-plant hydraulic conductance (g d⁻¹ kPa⁻¹).
- **whole_plant_conductivity**: Whole-plant hydraulic conductivity (g d⁻¹ cm kPa⁻¹).
- **leaf_specific_transpiration**: Leaf-specific transpiration (g d⁻¹ cm⁻²).
- **leaf_specific_conductance**: Leaf-specific hydraulic conductance (g d⁻¹ kPa⁻¹ cm⁻²).
- **leaf_specific_conductivity**: Leaf-specific hydraulic conductivity (g d⁻¹ cm⁻¹ kPa⁻¹).

### Log-transformed variables
- **water_potential_midday_log**: Natural log of (-midday water potential).
- **osmotic_potential_log**: Natural log of (-osmotic potential).
- **transpiration_mean_log**: Natural log of mean transpiration.
- **whole_plant_leaf_area_log**: Natural log of whole-plant leaf area.
- **whole_plant_conductance_log**: Natural log of whole-plant hydraulic conductance.
- **whole_plant_conductivity_log**: Natural log of whole-plant hydraulic conductivity.
- **leaf_specific_conductance_log**: Natural log of leaf-specific hydraulic conductance.
- **leaf_specific_conductivity_log**: Natural log of leaf-specific hydraulic conductivity.
- **leaf_specific_transpiration_log**: Natural log of leaf-specific transpiration.

---

## data_stomatal_density.rds / data_stomatal_density.csv

### Identifiers
- **plant**: Unique plant identifier (text code).
- **species**: Species code (ACY, DPE, LST, CAL, SRO).
- **plant_no**: Plant number within species and greenhouse (integer).
- **greenhouse**: Greenhouse number (1 or 2).
- **co2**: CO₂ treatment ("ambient" or "elevated").

### Stomatal density measurements
- **sd_stomatal_density**: Standard deviation of stomatal density across replicate leaf impressions (number mm⁻²).
- **mean_stomatal_density**: Mean stomatal density (number mm⁻²).

---

## data_carbohydrates.rds / data_carbohydrates.csv

### Identifiers
- **register_number**: Laboratory sample registration number.
- **plant**: Unique plant identifier (text code).
- **co2**: CO₂ treatment ("ambient" or "elevated").
- **greenhouse**: Greenhouse number (A,B,C,D,E).
- **species**: Species code (ACY, DPE, LST, CAL, SRO).
- **plant_no**: Plant number within species and greenhouse (integer).

### Carbohydrate measurements
- **drymaterial**: Dry matter content of fresh root sample (%).
- **starch**: Root starch concentration (g 100 g⁻¹ dry material).
- **solublesugars_total**: Total soluble sugars concentration (g 100 g⁻¹ dry material).
- **solublesugars_reducing**: Reducing sugars concentration (g 100 g⁻¹ dry material). Values "<0.10" indicate below detection limit.
- **NSC**: Total non-structural carbohydrates (starch + total soluble sugars) (g 100 g⁻¹ dry material).

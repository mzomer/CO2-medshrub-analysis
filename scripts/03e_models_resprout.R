# ─────────────────────────────────────────────────────────────────────
# 03e_models_resprout.R — Statistical models for resprouter effects
# ─────────────────────────────────────────────────────────────────────
# Models for: All variables tested for resprouter trait effects
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data ----
data_traits_biomass <- readRDS(here::here("data", "processed", "data_traits_biomass.rds"))
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))

# ═════════════════════════════════════════════════════════════════════
# ROOT:SHOOT RATIO (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

rs_ro_null <- lmer(root_shoot_ratio_log ~ 1 + (1|species) + (1|greenhouse), 
                   data = data_traits_biomass)
rs_ro_m1 <- lmer(root_shoot_ratio_log ~ co2 + (1|species) + (1|greenhouse), 
                 data = data_traits_biomass)
rs_ro_m2 <- lmer(root_shoot_ratio_log ~ resprouter + (1|species) + (1|greenhouse), 
                 data = data_traits_biomass)
rs_ro_m3 <- lmer(root_shoot_ratio_log ~ co2 + resprouter + (1|species) + (1|greenhouse), 
                 data = data_traits_biomass)
rs_ro_m4 <- lmer(root_shoot_ratio_log ~ co2 * resprouter + (1|species) + (1|greenhouse), 
                 data = data_traits_biomass)

anova(rs_ro_null, rs_ro_m2, rs_ro_m3, rs_ro_m4)

# Final model
rs_ro_model <- rs_ro_m4
anova(rs_ro_model)
summary(rs_ro_model)
plot(simulateResiduals(rs_ro_model))

# Post-hoc comparisons
rs_ro_emmeans <- emmeans(rs_ro_model, pairwise ~ co2 | resprouter)
rs_ro_emmeans

# Generate predictions
rs_ro_predictions <- ggpredict(rs_ro_model, terms = c("resprouter", "co2"))

# Save outputs
saveRDS(rs_ro_model, here::here("outputs", "models", "rs_ro_model.rds"))
saveRDS(rs_ro_emmeans, here::here("outputs", "models", "rs_ro_emmeans.rds"))
saveRDS(rs_ro_predictions, here::here("outputs", "models", "rs_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# MIDDAY WATER POTENTIAL (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

ymd_ro_null <- lmer(water_potential_midday_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                    data = data_water_relations)
ymd_ro_m1 <- lmer(water_potential_midday_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
ymd_ro_m2 <- lmer(water_potential_midday_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
ymd_ro_m3 <- lmer(water_potential_midday_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
ymd_ro_m4 <- lmer(water_potential_midday_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)

anova(ymd_ro_null, ymd_ro_m1, ymd_ro_m2, ymd_ro_m3, ymd_ro_m4)

# Final model
ymd_ro_model <- ymd_ro_m4
anova(ymd_ro_model)
summary(ymd_ro_model)
plot(simulateResiduals(ymd_ro_model))

# Post-hoc comparisons
ymd_ro_emmeans <- emmeans(ymd_ro_model, pairwise ~ resprouter|drought_phase)
ymd_ro_emmeans

# Generate predictions
ymd_ro_predictions <- ggpredict(ymd_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(ymd_ro_model, here::here("outputs", "models", "ymd_ro_model.rds"))
saveRDS(ymd_ro_emmeans, here::here("outputs", "models", "ymd_ro_emmeans.rds"))
saveRDS(ymd_ro_predictions, here::here("outputs", "models", "ymd_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# OSMOTIC POTENTIAL (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

osm_ro_null <- lmer(osmotic_potential_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                    data = data_water_relations)
osm_ro_m1 <- lmer(osmotic_potential_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
osm_ro_m2 <- lmer(osmotic_potential_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
osm_ro_m3 <- lmer(osmotic_potential_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
osm_ro_m4 <- lmer(osmotic_potential_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)

anova(osm_ro_null, osm_ro_m1, osm_ro_m2, osm_ro_m3, osm_ro_m4)

# Final model
osm_ro_model <- osm_ro_m3
anova(osm_ro_model)
summary(osm_ro_model)
plot(simulateResiduals(osm_ro_model))

# Post-hoc comparisons
osm_ro_emmeans <- emmeans(osm_ro_model, pairwise ~ resprouter | drought_phase)
osm_ro_emmeans

# Generate predictions
osm_ro_predictions <- ggpredict(osm_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(osm_ro_model, here::here("outputs", "models", "osm_ro_model.rds"))
saveRDS(osm_ro_emmeans, here::here("outputs", "models", "osm_ro_emmeans.rds"))
saveRDS(osm_ro_predictions, here::here("outputs", "models", "osm_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT TRANSPIRATION (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

wt_ro_null <- lmer(transpiration_mean_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                   data = data_water_relations)
wt_ro_m1 <- lmer(transpiration_mean_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)
wt_ro_m2 <- lmer(transpiration_mean_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)
wt_ro_m3 <- lmer(transpiration_mean_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)
wt_ro_m4 <- lmer(transpiration_mean_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)

anova(wt_ro_null, wt_ro_m1, wt_ro_m2, wt_ro_m3, wt_ro_m4)

# Final model
wt_ro_model <- wt_ro_m1
anova(wt_ro_model)
summary(wt_ro_model)
plot(simulateResiduals(wt_ro_model))

# Post-hoc comparisons
wt_ro_emmeans <- emmeans(wt_ro_model, pairwise ~ drought_phase)
wt_ro_emmeans

# Generate predictions
wt_ro_predictions <- ggpredict(wt_ro_model, terms = c("drought_phase"))

# Save outputs
saveRDS(wt_ro_model, here::here("outputs", "models", "wt_ro_model.rds"))
saveRDS(wt_ro_emmeans, here::here("outputs", "models", "wt_ro_emmeans.rds"))
saveRDS(wt_ro_predictions, here::here("outputs", "models", "wt_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT HYDRAULIC CONDUCTANCE (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

wpc_ro_null <- lmer(whole_plant_conductance_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                    data = data_water_relations)
wpc_ro_m1 <- lmer(whole_plant_conductance_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
wpc_ro_m2 <- lmer(whole_plant_conductance_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
wpc_ro_m3 <- lmer(whole_plant_conductance_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)
wpc_ro_m4 <- lmer(whole_plant_conductance_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                  data = data_water_relations)

anova(wpc_ro_null, wpc_ro_m1, wpc_ro_m2, wpc_ro_m3, wpc_ro_m4)

# Final model
wpc_ro_model <- wpc_ro_m3
anova(wpc_ro_model)
summary(wpc_ro_model)
plot(simulateResiduals(wpc_ro_model))

# Post-hoc comparisons
wpc_ro_emmeans <- emmeans(wpc_ro_model, pairwise ~ resprouter | drought_phase)
wpc_ro_emmeans
# Generate predictions
wpc_ro_predictions <- ggpredict(wpc_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(wpc_ro_model, here::here("outputs", "models", "wpc_ro_model.rds"))
saveRDS(wpc_ro_emmeans, here::here("outputs", "models", "wpc_ro_emmeans.rds"))
saveRDS(wpc_ro_predictions, here::here("outputs", "models", "wpc_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT HYDRAULIC CONDUCTIVITY (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

wpconductivity_ro_null <- lmer(whole_plant_conductivity_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                               data = data_water_relations)
wpconductivity_ro_m1 <- lmer(whole_plant_conductivity_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                             data = data_water_relations)
wpconductivity_ro_m2 <- lmer(whole_plant_conductivity_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                             data = data_water_relations)
wpconductivity_ro_m3 <- lmer(whole_plant_conductivity_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                             data = data_water_relations)
wpconductivity_ro_m4 <- lmer(whole_plant_conductivity_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                             data = data_water_relations)

anova(wpconductivity_ro_null, wpconductivity_ro_m1, wpconductivity_ro_m2, wpconductivity_ro_m3, wpconductivity_ro_m4)

# Final model
wpconductivity_ro_model <- wpconductivity_ro_m3
anova(wpconductivity_ro_model)
summary(wpconductivity_ro_model)
plot(simulateResiduals(wpconductivity_ro_model))

# Post-hoc comparisons
wpconductivity_ro_emmeans <- emmeans(wpconductivity_ro_model, pairwise ~ resprouter | drought_phase)
wpconductivity_ro_emmeans

# Generate predictions
wpconductivity_ro_predictions <- ggpredict(wpconductivity_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(wpconductivity_ro_model, here::here("outputs", "models", "wpconductivity_ro_model.rds"))
saveRDS(wpconductivity_ro_emmeans, here::here("outputs", "models", "wpconductivity_ro_emmeans.rds"))
saveRDS(wpconductivity_ro_predictions, here::here("outputs", "models", "wpconductivity_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC TRANSPIRATION (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

leaf_trans_ro_null <- lmer(leaf_specific_transpiration_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                           data = data_water_relations)
leaf_trans_ro_m1 <- lmer(leaf_specific_transpiration_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                         data = data_water_relations)
leaf_trans_ro_m2 <- lmer(leaf_specific_transpiration_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                         data = data_water_relations)
leaf_trans_ro_m3 <- lmer(leaf_specific_transpiration_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                         data = data_water_relations)
leaf_trans_ro_m4 <- lmer(leaf_specific_transpiration_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                         data = data_water_relations)

anova(leaf_trans_ro_null, leaf_trans_ro_m1, leaf_trans_ro_m2, leaf_trans_ro_m3, leaf_trans_ro_m4)

# Final model
leaf_trans_ro_model <- leaf_trans_ro_m3
anova(leaf_trans_ro_model)
summary(leaf_trans_ro_model)
plot(simulateResiduals(leaf_trans_ro_model))

# Post-hoc comparisons
leaf_trans_ro_emmeans <- emmeans(leaf_trans_ro_model, pairwise ~ resprouter | drought_phase)
leaf_trans_ro_emmeans

# Generate predictions
leaf_trans_ro_predictions <- ggpredict(leaf_trans_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(leaf_trans_ro_model, here::here("outputs", "models", "leaf_trans_ro_model.rds"))
saveRDS(leaf_trans_ro_emmeans, here::here("outputs", "models", "leaf_trans_ro_emmeans.rds"))
saveRDS(leaf_trans_ro_predictions, here::here("outputs", "models", "leaf_trans_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC CONDUCTANCE (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

lc_ro_null <- lmer(leaf_specific_conductance_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                   data = data_water_relations)
lc_ro_m1 <- lmer(leaf_specific_conductance_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)
lc_ro_m2 <- lmer(leaf_specific_conductance_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)
lc_ro_m3 <- lmer(leaf_specific_conductance_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)
lc_ro_m4 <- lmer(leaf_specific_conductance_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                 data = data_water_relations)

anova(lc_ro_null, lc_ro_m1, lc_ro_m2, lc_ro_m3, lc_ro_m4)

# Final model
lc_ro_model <- lc_ro_m3
anova(lc_ro_model)
summary(lc_ro_model)
plot(simulateResiduals(lc_ro_model))

# Post-hoc comparisons
lc_ro_emmeans <- emmeans(lc_ro_model, pairwise ~ resprouter | drought_phase)
lc_ro_emmeans

# Generate predictions
lc_ro_predictions <- ggpredict(lc_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(lc_ro_model, here::here("outputs", "models", "lc_ro_model.rds"))
saveRDS(lc_ro_emmeans, here::here("outputs", "models", "lc_ro_emmeans.rds"))
saveRDS(lc_ro_predictions, here::here("outputs", "models", "lc_ro_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC CONDUCTIVITY (resprouter effect) ----
# ═════════════════════════════════════════════════════════════════════

lconductivity_ro_null <- lmer(leaf_specific_conductivity_log ~ 1 + (1|species/plant) + (1|greenhouse), 
                              data = data_water_relations)
lconductivity_ro_m1 <- lmer(leaf_specific_conductivity_log ~ drought_phase + (1|species/plant) + (1|greenhouse), 
                            data = data_water_relations)
lconductivity_ro_m2 <- lmer(leaf_specific_conductivity_log ~ drought_phase + resprouter + (1|species/plant) + (1|greenhouse), 
                            data = data_water_relations)
lconductivity_ro_m3 <- lmer(leaf_specific_conductivity_log ~ drought_phase * resprouter + (1|species/plant) + (1|greenhouse), 
                            data = data_water_relations)
lconductivity_ro_m4 <- lmer(leaf_specific_conductivity_log ~ drought_phase * resprouter + co2 + (1|species/plant) + (1|greenhouse), 
                            data = data_water_relations)

anova(lconductivity_ro_null, lconductivity_ro_m1, lconductivity_ro_m2, lconductivity_ro_m3, lconductivity_ro_m4)

# Final model
lconductivity_ro_model <- lconductivity_ro_m3
anova(lconductivity_ro_model)
summary(lconductivity_ro_model)
plot(simulateResiduals(lconductivity_ro_model))

# Post-hoc comparisons
lconductivity_ro_emmeans <- emmeans(lconductivity_ro_model, pairwise ~ resprouter | drought_phase)
lconductivity_ro_emmeans

# Generate predictions
lconductivity_ro_predictions <- ggpredict(lconductivity_ro_model, terms = c("drought_phase", "resprouter"))

# Save outputs
saveRDS(lconductivity_ro_model, here::here("outputs", "models", "lconductivity_ro_model.rds"))
saveRDS(lconductivity_ro_emmeans, here::here("outputs", "models", "lconductivity_ro_emmeans.rds"))
saveRDS(lconductivity_ro_predictions, here::here("outputs", "models", "lconductivity_ro_predictions.rds"))


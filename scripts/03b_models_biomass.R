# ─────────────────────────────────────────────────────────────────────
# 03b_models_biomass.R — Statistical models for biomass allocation
# ─────────────────────────────────────────────────────────────────────
# Models for: Aboveground, belowground, R:S, leaf/stem biomass, leaf area
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data ----
data_traits_biomass <- readRDS(here::here("data", "processed", "data_traits_biomass.rds"))
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))


# ═════════════════════════════════════════════════════════════════════
# ABOVEGROUND BIOMASS ----
# ═════════════════════════════════════════════════════════════════════

## Model selection ----
tab_null <- lmer(total_aboveground_biomass_log ~ 1 + (1|greenhouse), 
                 data = data_traits_biomass)
tab_m1 <- update(tab_null, .~ . + species)
tab_m2 <- update(tab_m1, .~ . + co2)
tab_m3 <- update(tab_m2, .~ . + species:co2)

anova(tab_null, tab_m1, tab_m2, tab_m3) #***, ***, ***

## Final model ----
tab_final_model <- lmer(total_aboveground_biomass_log ~ species * co2 + (1|greenhouse), 
                        data = data_traits_biomass)

anova(tab_final_model)
summary(tab_final_model)
plot(simulateResiduals(tab_final_model))

# Post-hoc comparisons
tab_emmeans <- emmeans(tab_final_model, pairwise ~ co2 | species, 
                       adjust = "bonferroni")
tab_emmeans

# Generate predictions
tab_predictions <- ggpredict(tab_final_model, terms = c("species", "co2"))

# Save outputs
saveRDS(tab_final_model, here::here("outputs", "models", "tab_model.rds"))
saveRDS(tab_emmeans, here::here("outputs", "models", "tab_emmeans.rds"))
saveRDS(tab_predictions, here::here("outputs", "models", "tab_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# BELOWGROUND BIOMASS ----
# ═════════════════════════════════════════════════════════════════════

## Model selection ----
tbb_null <- lmer(total_belowground_biomass_log ~ 1 + (1|greenhouse), 
                 data = data_traits_biomass)
tbb_m1 <- update(tbb_null, .~ . + species)
tbb_m2 <- update(tbb_m1, .~ . + co2)
tbb_m3 <- update(tbb_m2, .~ . + species: co2)

anova(tbb_null, tbb_m1, tbb_m2, tbb_m3) #***, ** ,***

## Final model ----
tbb_final_model <- lmer(total_belowground_biomass_log ~ species * co2 + (1|greenhouse), 
                        data = data_traits_biomass)

anova(tbb_final_model)
summary(tbb_final_model)
plot(simulateResiduals(tbb_final_model))

# Post-hoc comparisons
tbb_emmeans <- emmeans(tbb_final_model, pairwise ~ co2 | species, 
                       adjust = "bonferroni")

tbb_emmeans

# Generate predictions
tbb_predictions <- ggpredict(tbb_final_model, terms = c("species", "co2"))

# Save outputs
saveRDS(tbb_final_model, here::here("outputs", "models", "tbb_model.rds"))
saveRDS(tbb_emmeans, here::here("outputs", "models", "tbb_emmeans.rds"))
saveRDS(tbb_predictions, here::here("outputs", "models", "tbb_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# ROOT:SHOOT RATIO ----
# ═════════════════════════════════════════════════════════════════════

## Model selection ----
rs_null <- lmer(root_shoot_ratio_log ~ 1 + (1|greenhouse), 
                data = data_traits_biomass)
rs_m1 <- update(rs_null, .~ . + species)
rs_m2 <- update(rs_m1, .~ . + co2)
rs_m3 <-update(rs_m2, .~ . + species:co2)

anova(rs_null, rs_m1, rs_m2,rs_m3 ) # ***, ns, ns


## Final model ----
rs_final_model <- lmer(root_shoot_ratio_log ~ species + co2 + (1|greenhouse), 
                       data = data_traits_biomass)

anova(rs_final_model)
summary(rs_final_model)
plot(simulateResiduals(rs_final_model))

# Post-hoc comparisons
rs_emmeans <- emmeans(rs_final_model, pairwise ~ co2 | species, 
                      adjust = "bonferroni")

# Generate predictions
rs_predictions <- ggpredict(rs_final_model, terms = c("species", "co2"))

# Save outputs
saveRDS(rs_final_model, here::here("outputs", "models", "rs_model.rds"))
saveRDS(rs_emmeans, here::here("outputs", "models", "rs_emmeans.rds"))
saveRDS(rs_predictions, here::here("outputs", "models", "rs_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF BIOMASS ----
# ═════════════════════════════════════════════════════════════════════

lfbio_null <- lmer(leaves_biomass_log ~ 1 + (1|greenhouse), 
                   data = data_traits_biomass)

lfbio_m1 <- update(lfbio_null, .~ . + species)
lfbio_m2 <- update(lfbio_m1, .~ . + co2)
lfbio_m3 <-update(lfbio_m2, .~ . + species:co2)


anova(lfbio_null, lfbio_m1, lfbio_m2, lfbio_m3)

lfbio_final_model <- lmer(leaves_biomass_log ~ species * co2 + (1|greenhouse), 
                          data = data_traits_biomass)
anova(lfbio_final_model)
summary(lfbio_final_model)
plot(simulateResiduals(lfbio_final_model))


# Post-hoc comparisons
lfbio_emmeans <- emmeans(lfbio_final_model, pairwise ~ co2 | species, 
                         adjust = "bonferroni")
lfbio_emmeans


# Generate predictions
lfbio_predictions <- ggpredict(lfbio_final_model, terms = c("species", "co2"))

# Save outputs
saveRDS(lfbio_final_model, here::here("outputs", "models", "lfbio_model.rds"))
saveRDS(lfbio_emmeans, here::here("outputs", "models", "lfbio_emmeans.rds"))
saveRDS(lfbio_predictions, here::here("outputs", "models", "lfbio_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# STEM BIOMASS ----
# ═════════════════════════════════════════════════════════════════════

stembio_null <- lmer(stems_biomass_log ~ 1 + (1|greenhouse), 
                     data = data_traits_biomass)
stembio_m1 <- update(stembio_null, .~ . + species)
stembio_m2 <- update(stembio_m1, .~ . + co2)
stembio_m3 <-update(stembio_m2, .~ . + species:co2)


anova(stembio_null, stembio_m1, stembio_m2, stembio_m3)

stembio_final_model <- lmer(stems_biomass_log ~ species * co2 + (1|greenhouse), 
                     data = data_traits_biomass)

anova(stembio_final_model)
summary(stembio_final_model)
plot(simulateResiduals(stembio_final_model))

# Post-hoc comparisons
stembio_emmeans <- emmeans(stembio_final_model, pairwise ~ co2 | species, 
                           adjust = "bonferroni")
stembio_emmeans

# Generate predictions
stembio_predictions <- ggpredict(stembio_final_model, terms = c("species", "co2"))

# Save outputs
saveRDS(stembio_final_model, here::here("outputs", "models", "stembio_model.rds"))
saveRDS(stembio_emmeans, here::here("outputs", "models", "stembio_emmeans.rds"))
saveRDS(stembio_predictions, here::here("outputs", "models", "stembio_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF AREA (by phase) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
lfarea_acy_null <- lmer(whole_plant_leaf_area_log ~ 1 + (1|greenhouse/plant), 
                        data = data_water_relations[data_water_relations$species == "ACY",])
lfarea_acy_m1 <- update(lfarea_acy_null, .~ . + drought_phase)
lfarea_acy_m2 <- update(lfarea_acy_m1, .~ . + co2)
lfarea_acy_m3 <- update(lfarea_acy_m2, .~ . + drought_phase:co2)

anova(lfarea_acy_null, lfarea_acy_m1, lfarea_acy_m2, lfarea_acy_m3)

lfarea_acy <- lmer(whole_plant_leaf_area_log ~ drought_phase + co2 + 
                     (1|greenhouse/plant), 
                   data = data_water_relations[data_water_relations$species == "ACY",])
anova(lfarea_acy)
summary(lfarea_acy)
plot(simulateResiduals(lfarea_acy))

# DPE ----
lfarea_dpe_null <- lmer(whole_plant_leaf_area_log ~ 1 + (1|greenhouse/plant), 
                        data = data_water_relations[data_water_relations$species == "DPE",])
lfarea_dpe_m1 <- update(lfarea_dpe_null, .~ . + drought_phase)
lfarea_dpe_m2 <- update(lfarea_dpe_m1, .~ . + co2)
lfarea_dpe_m3 <- update(lfarea_dpe_m2, .~ . + drought_phase:co2)

anova(lfarea_dpe_null, lfarea_dpe_m1, lfarea_dpe_m2, lfarea_dpe_m3) #*

lfarea_dpe <- lmer(whole_plant_leaf_area_log ~ drought_phase + co2 + 
                     (1|greenhouse/plant), 
                   data = data_water_relations[data_water_relations$species == "DPE",])
anova(lfarea_dpe)
summary(lfarea_dpe)
plot(simulateResiduals(lfarea_dpe))

# LST ----
lfarea_lst_null <- lmer(whole_plant_leaf_area_log ~ 1 + (1|greenhouse/plant), 
                        data = data_water_relations[data_water_relations$species == "LST",])
lfarea_lst_m1 <- update(lfarea_lst_null, .~ . + drought_phase)
lfarea_lst_m2 <- update(lfarea_lst_m1, .~ . + co2)
lfarea_lst_m3 <- update(lfarea_lst_m2, .~ . + drought_phase:co2)

anova(lfarea_lst_null, lfarea_lst_m1, lfarea_lst_m2, lfarea_lst_m3) #*

lfarea_lst <- lmer(whole_plant_leaf_area_log ~ drought_phase + co2 + 
                     (1|greenhouse/plant), 
                   data = data_water_relations[data_water_relations$species == "LST",])
anova(lfarea_lst)
summary(lfarea_lst)
plot(simulateResiduals(lfarea_lst))

# CAL ----
lfarea_cal_null <- lmer(whole_plant_leaf_area_log ~ 1 + (1|greenhouse/plant), 
                        data = data_water_relations[data_water_relations$species == "CAL",])
lfarea_cal_m1 <- update(lfarea_cal_null, .~ . + drought_phase)
lfarea_cal_m2 <- update(lfarea_cal_m1, .~ . + co2)
lfarea_cal_m3 <- update(lfarea_cal_m2, .~ . + drought_phase:co2)

anova(lfarea_cal_null, lfarea_cal_m1, lfarea_cal_m2, lfarea_cal_m3) #***

lfarea_cal <- lmer(whole_plant_leaf_area_log ~ drought_phase + co2 + 
                     (1|greenhouse/plant), 
                   data = data_water_relations[data_water_relations$species == "CAL",])
anova(lfarea_cal)
summary(lfarea_cal)
plot(simulateResiduals(lfarea_cal))

# SRO ----
lfarea_sro_null <- lmer(whole_plant_leaf_area_log ~ 1 + (1|greenhouse/plant), 
                        data = data_water_relations[data_water_relations$species == "SRO",])
lfarea_sro_m1 <- update(lfarea_sro_null, .~ . + drought_phase)
lfarea_sro_m2 <- update(lfarea_sro_m1, .~ . + co2)
lfarea_sro_m3 <- update(lfarea_sro_m2, .~ . + drought_phase:co2)

anova(lfarea_sro_null, lfarea_sro_m1, lfarea_sro_m2, lfarea_sro_m3) #***, ns, #***

lfarea_sro <- lmer(whole_plant_leaf_area_log ~ drought_phase * co2 + 
                     (1|greenhouse/plant), 
                   data = data_water_relations[data_water_relations$species == "SRO",])
anova(lfarea_sro)
summary(lfarea_sro)
plot(simulateResiduals(lfarea_sro))

# Post-hoc comparisons ----
emmeans(lfarea_acy, pairwise ~ co2 | drought_phase)
emmeans(lfarea_dpe, pairwise ~ co2 | drought_phase)
emmeans(lfarea_lst, pairwise ~ co2 | drought_phase)
emmeans(lfarea_cal, pairwise ~ co2 | drought_phase)
emmeans(lfarea_sro, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_area <- ggpredict(lfarea_acy, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_area <- ggpredict(lfarea_dpe, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_area <- ggpredict(lfarea_lst, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_area <- ggpredict(lfarea_cal, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_area <- ggpredict(lfarea_sro, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")


# Combine predictions
lfarea_predictions <- bind_rows(pred_acy_area, pred_dpe_area, pred_lst_area, 
                                pred_cal_area, pred_sro_area) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

# Factor species separately, not in the pipe
lfarea_predictions$species <- factor(lfarea_predictions$species, 
                                     levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# drop NAs if necessary
lfarea_predictions <- lfarea_predictions %>% drop_na()


# Save outputs ----
saveRDS(list(acy = lfarea_acy, dpe = lfarea_dpe, lst = lfarea_lst, 
             cal = lfarea_cal, sro = lfarea_sro), 
        here::here("outputs", "models", "lfarea_models.rds"))
saveRDS(lfarea_predictions, 
        here::here("outputs", "models", "lfarea_predictions.rds"))


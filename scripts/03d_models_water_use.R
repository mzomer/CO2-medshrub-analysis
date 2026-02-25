# ─────────────────────────────────────────────────────────────────────
# 03d_models_water_use.R — Statistical models for water use and hydraulics
# ─────────────────────────────────────────────────────────────────────
# Models for: Transpiration, hydraulic conductance/conductivity (whole-plant & leaf-specific)
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data ----
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT TRANSPIRATION (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
wt_acy_null <- lmer(transpiration_mean_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "ACY",])
wt_acy_m1 <- update(wt_acy_null, .~ . + drought_phase)
wt_acy_m2 <- update(wt_acy_m1, .~ . + co2)
wt_acy_m3 <- update(wt_acy_m2, .~ . + drought_phase:co2)

anova(wt_acy_null, wt_acy_m1, wt_acy_m2, wt_acy_m3)

wt_acy_final <- lmer(transpiration_mean_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "ACY",])
anova(wt_acy_final)
summary(wt_acy_final)
plot(simulateResiduals(wt_acy_final))

# DPE ----
wt_dpe_null <- lmer(transpiration_mean_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "DPE",])
wt_dpe_m1 <- update(wt_dpe_null, .~ . + drought_phase)
wt_dpe_m2 <- update(wt_dpe_m1, .~ . + co2)
wt_dpe_m3 <- update(wt_dpe_m2, .~ . + drought_phase:co2)

anova(wt_dpe_null, wt_dpe_m1, wt_dpe_m2, wt_dpe_m3)

wt_dpe_final <- lmer(transpiration_mean_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "DPE",])
anova(wt_dpe_final)
summary(wt_dpe_final)
plot(simulateResiduals(wt_dpe_final))

# LST ----
wt_lst_null <- lmer(transpiration_mean_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "LST",])
wt_lst_m1 <- update(wt_lst_null, .~ . + drought_phase)
wt_lst_m2 <- update(wt_lst_m1, .~ . + co2)
wt_lst_m3 <- update(wt_lst_m2, .~ . + drought_phase:co2)

anova(wt_lst_null, wt_lst_m1, wt_lst_m2, wt_lst_m3)

wt_lst_final <- lmer(transpiration_mean_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "LST",])
anova(wt_lst_final)
summary(wt_lst_final)
plot(simulateResiduals(wt_lst_final))

# CAL ----
wt_cal_null <- lmer(transpiration_mean_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "CAL",])
wt_cal_m1 <- update(wt_cal_null, .~ . + drought_phase)
wt_cal_m2 <- update(wt_cal_m1, .~ . + co2)
wt_cal_m3 <- update(wt_cal_m2, .~ . + drought_phase:co2)

anova(wt_cal_null, wt_cal_m1, wt_cal_m2, wt_cal_m3)

wt_cal_final <- lmer(transpiration_mean_log ~ drought_phase * co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "CAL",])
anova(wt_cal_final)
summary(wt_cal_final)
plot(simulateResiduals(wt_cal_final))

# SRO ----
wt_sro_null <- lmer(transpiration_mean_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "SRO",])
wt_sro_m1 <- update(wt_sro_null, .~ . + drought_phase)
wt_sro_m2 <- update(wt_sro_m1, .~ . + co2)
wt_sro_m3 <- update(wt_sro_m2, .~ . + drought_phase:co2)

anova(wt_sro_null, wt_sro_m1, wt_sro_m2, wt_sro_m3)

wt_sro_final <- lmer(transpiration_mean_log ~ drought_phase * co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "SRO",])
anova(wt_sro_final)
summary(wt_sro_final)
plot(simulateResiduals(wt_sro_final))

# Post-hoc comparisons ----
wt_acy_emmeans <- emmeans(wt_acy_final, pairwise ~ co2 | drought_phase)
wt_dpe_emmeans <- emmeans(wt_dpe_final, pairwise ~ co2 | drought_phase)
wt_lst_emmeans <- emmeans(wt_lst_final, pairwise ~ co2 | drought_phase)
wt_cal_emmeans <- emmeans(wt_cal_final, pairwise ~ co2 | drought_phase)
wt_sro_emmeans <- emmeans(wt_sro_final, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_wt <- ggpredict(wt_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_wt <- ggpredict(wt_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_wt <- ggpredict(wt_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_wt <- ggpredict(wt_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_wt <- ggpredict(wt_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
wt_predictions <- bind_rows(pred_acy_wt, pred_dpe_wt, pred_lst_wt, 
                            pred_cal_wt, pred_sro_wt) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

wt_predictions$species <- factor(wt_predictions$species, 
                                 levels = c("ACY", "DPE", "LST", "CAL", "SRO"))
# Save outputs ----
saveRDS(list(acy = wt_acy_final, dpe = wt_dpe_final, lst = wt_lst_final, 
             cal = wt_cal_final, sro = wt_sro_final), 
        here::here("outputs", "models", "wt_models.rds"))
saveRDS(list(acy = wt_acy_emmeans, dpe = wt_dpe_emmeans, lst = wt_lst_emmeans,
             cal = wt_cal_emmeans, sro = wt_sro_emmeans),
        here::here("outputs", "models", "wt_emmeans.rds"))
saveRDS(wt_predictions, 
        here::here("outputs", "models", "wt_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT HYDRAULIC CONDUCTANCE (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
wpc_acy_null <- lmer(whole_plant_conductance_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "ACY",])
wpc_acy_m1 <- update(wpc_acy_null, .~ . + drought_phase)
wpc_acy_m2 <- update(wpc_acy_m1, .~ . + co2)
wpc_acy_m3 <- update(wpc_acy_m2, .~ . + drought_phase:co2)

anova(wpc_acy_null, wpc_acy_m1, wpc_acy_m2, wpc_acy_m3)

wpc_acy_final <- lmer(whole_plant_conductance_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "ACY",])
anova(wpc_acy_final)
summary(wpc_acy_final)
plot(simulateResiduals(wpc_acy_final))

# DPE ----
wpc_dpe_null <- lmer(whole_plant_conductance_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "DPE",])
wpc_dpe_m1 <- update(wpc_dpe_null, .~ . + drought_phase)
wpc_dpe_m2 <- update(wpc_dpe_m1, .~ . + co2)
wpc_dpe_m3 <- update(wpc_dpe_m2, .~ . + drought_phase:co2)

anova(wpc_dpe_null, wpc_dpe_m1, wpc_dpe_m2, wpc_dpe_m3)

wpc_dpe_final <- lmer(whole_plant_conductance_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "DPE",])
anova(wpc_dpe_final)
summary(wpc_dpe_final)
plot(simulateResiduals(wpc_dpe_final))

# LST ----
wpc_lst_null <- lmer(whole_plant_conductance_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "LST",])
wpc_lst_m1 <- update(wpc_lst_null, .~ . + drought_phase)
wpc_lst_m2 <- update(wpc_lst_m1, .~ . + co2)
wpc_lst_m3 <- update(wpc_lst_m2, .~ . + drought_phase:co2)

anova(wpc_lst_null, wpc_lst_m1, wpc_lst_m2, wpc_lst_m3)

wpc_lst_final <- lmer(whole_plant_conductance_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "LST",])
anova(wpc_lst_final)
summary(wpc_lst_final)
plot(simulateResiduals(wpc_lst_final))

# CAL ----
wpc_cal_null <- lmer(whole_plant_conductance_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "CAL",])
wpc_cal_m1 <- update(wpc_cal_null, .~ . + drought_phase)
wpc_cal_m2 <- update(wpc_cal_m1, .~ . + co2)
wpc_cal_m3 <- update(wpc_cal_m2, .~ . + drought_phase:co2)

anova(wpc_cal_null, wpc_cal_m1, wpc_cal_m2, wpc_cal_m3)

wpc_cal_final <- lmer(whole_plant_conductance_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "CAL",])
anova(wpc_cal_final)
summary(wpc_cal_final)
plot(simulateResiduals(wpc_cal_final))

# SRO ----
wpc_sro_null <- lmer(whole_plant_conductance_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "SRO",])
wpc_sro_m1 <- update(wpc_sro_null, .~ . + drought_phase)
wpc_sro_m2 <- update(wpc_sro_m1, .~ . + co2)
wpc_sro_m3 <- update(wpc_sro_m2, .~ . + drought_phase:co2)

anova(wpc_sro_null, wpc_sro_m1, wpc_sro_m2, wpc_sro_m3)

wpc_sro_final <- lmer(whole_plant_conductance_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "SRO",])
anova(wpc_sro_final)
summary(wpc_sro_final)
plot(simulateResiduals(wpc_sro_final))

# Post-hoc comparisons ----
wpc_acy_emmeans <- emmeans(wpc_acy_final, pairwise ~ co2 | drought_phase)
wpc_dpe_emmeans <- emmeans(wpc_dpe_final, pairwise ~ co2 | drought_phase)
wpc_lst_emmeans <- emmeans(wpc_lst_final, pairwise ~ co2 | drought_phase)
wpc_cal_emmeans <- emmeans(wpc_cal_final, pairwise ~ co2 | drought_phase)
wpc_sro_emmeans <- emmeans(wpc_sro_final, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_wpc <- ggpredict(wpc_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_wpc <- ggpredict(wpc_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_wpc <- ggpredict(wpc_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_wpc <- ggpredict(wpc_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_wpc <- ggpredict(wpc_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
wpc_predictions <- bind_rows(pred_acy_wpc, pred_dpe_wpc, pred_lst_wpc, 
                             pred_cal_wpc, pred_sro_wpc) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

wpc_predictions$species <- factor(wpc_predictions$species, 
                                  levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Save outputs ----
saveRDS(list(acy = wpc_acy_final, dpe = wpc_dpe_final, lst = wpc_lst_final, 
             cal = wpc_cal_final, sro = wpc_sro_final), 
        here::here("outputs", "models", "wpc_models.rds"))
saveRDS(list(acy = wpc_acy_emmeans, dpe = wpc_dpe_emmeans, lst = wpc_lst_emmeans,
             cal = wpc_cal_emmeans, sro = wpc_sro_emmeans),
        here::here("outputs", "models", "wpc_emmeans.rds"))
saveRDS(wpc_predictions, 
        here::here("outputs", "models", "wpc_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT HYDRAULIC CONDUCTIVITY (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
wpconductivity_acy_null <- lmer(whole_plant_conductivity_log ~ 1 + (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "ACY",])
wpconductivity_acy_m1 <- update(wpconductivity_acy_null, .~ . + drought_phase)
wpconductivity_acy_m2 <- update(wpconductivity_acy_m1, .~ . + co2)
wpconductivity_acy_m3 <- update(wpconductivity_acy_m2, .~ . + drought_phase:co2)

anova(wpconductivity_acy_null, wpconductivity_acy_m1, wpconductivity_acy_m2, wpconductivity_acy_m3)

wpconductivity_acy_final <- lmer(whole_plant_conductivity_log ~ drought_phase + co2 + 
                                   (1|greenhouse/plant), 
                                 data = data_water_relations[data_water_relations$species == "ACY",])
anova(wpconductivity_acy_final)
summary(wpconductivity_acy_final)
plot(simulateResiduals(wpconductivity_acy_final))

# DPE ----
wpconductivity_dpe_null <- lmer(whole_plant_conductivity_log ~ 1 + (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "DPE",])
wpconductivity_dpe_m1 <- update(wpconductivity_dpe_null, .~ . + drought_phase)
wpconductivity_dpe_m2 <- update(wpconductivity_dpe_m1, .~ . + co2)
wpconductivity_dpe_m3 <- update(wpconductivity_dpe_m2, .~ . + drought_phase:co2)

anova(wpconductivity_dpe_null, wpconductivity_dpe_m1, wpconductivity_dpe_m2, wpconductivity_dpe_m3)

wpconductivity_dpe_final <- lmer(whole_plant_conductivity_log ~ drought_phase + co2 + 
                                   (1|greenhouse/plant), 
                                 data = data_water_relations[data_water_relations$species == "DPE",])
anova(wpconductivity_dpe_final)
summary(wpconductivity_dpe_final)
plot(simulateResiduals(wpconductivity_dpe_final))

# LST ----
wpconductivity_lst_null <- lmer(whole_plant_conductivity_log ~ 1 + (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "LST",])
wpconductivity_lst_m1 <- update(wpconductivity_lst_null, .~ . + drought_phase)
wpconductivity_lst_m2 <- update(wpconductivity_lst_m1, .~ . + co2)
wpconductivity_lst_m3 <- update(wpconductivity_lst_m2, .~ . + drought_phase:co2)

anova(wpconductivity_lst_null, wpconductivity_lst_m1, wpconductivity_lst_m2, wpconductivity_lst_m3)

wpconductivity_lst_final <- lmer(whole_plant_conductivity_log ~ drought_phase + co2 + 
                                   (1|greenhouse/plant), 
                                 data = data_water_relations[data_water_relations$species == "LST",])
anova(wpconductivity_lst_final)
summary(wpconductivity_lst_final)
plot(simulateResiduals(wpconductivity_lst_final))

# CAL ----
wpconductivity_cal_null <- lmer(whole_plant_conductivity_log ~ 1 + (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "CAL",])
wpconductivity_cal_m1 <- update(wpconductivity_cal_null, .~ . + drought_phase)
wpconductivity_cal_m2 <- update(wpconductivity_cal_m1, .~ . + co2)
wpconductivity_cal_m3 <- update(wpconductivity_cal_m2, .~ . + drought_phase:co2)

anova(wpconductivity_cal_null, wpconductivity_cal_m1, wpconductivity_cal_m2, wpconductivity_cal_m3)

wpconductivity_cal_final <- lmer(whole_plant_conductivity_log ~ drought_phase + co2 + 
                                   (1|greenhouse/plant), 
                                 data = data_water_relations[data_water_relations$species == "CAL",])
anova(wpconductivity_cal_final)
summary(wpconductivity_cal_final)
plot(simulateResiduals(wpconductivity_cal_final))

# SRO ----
wpconductivity_sro_null <- lmer(whole_plant_conductivity_log ~ 1 + (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "SRO",])
wpconductivity_sro_m1 <- update(wpconductivity_sro_null, .~ . + drought_phase)
wpconductivity_sro_m2 <- update(wpconductivity_sro_m1, .~ . + co2)
wpconductivity_sro_m3 <- update(wpconductivity_sro_m2, .~ . + drought_phase:co2)

anova(wpconductivity_sro_null, wpconductivity_sro_m1, wpconductivity_sro_m2, wpconductivity_sro_m3)

wpconductivity_sro_final <- lmer(whole_plant_conductivity_log ~ drought_phase * co2 + 
                                   (1|greenhouse/plant), 
                                 data = data_water_relations[data_water_relations$species == "SRO",])
anova(wpconductivity_sro_final)
summary(wpconductivity_sro_final)
plot(simulateResiduals(wpconductivity_sro_final))

# Post-hoc comparisons ----
wpconductivity_acy_emmeans <- emmeans(wpconductivity_acy_final, pairwise ~ co2 | drought_phase)
wpconductivity_dpe_emmeans <- emmeans(wpconductivity_dpe_final, pairwise ~ co2 | drought_phase)
wpconductivity_lst_emmeans <- emmeans(wpconductivity_lst_final, pairwise ~ co2 | drought_phase)
wpconductivity_cal_emmeans <- emmeans(wpconductivity_cal_final, pairwise ~ co2 | drought_phase)
wpconductivity_sro_emmeans <- emmeans(wpconductivity_sro_final, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_wpconductivity <- ggpredict(wpconductivity_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_wpconductivity <- ggpredict(wpconductivity_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_wpconductivity <- ggpredict(wpconductivity_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_wpconductivity <- ggpredict(wpconductivity_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_wpconductivity <- ggpredict(wpconductivity_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
wpconductivity_predictions <- bind_rows(pred_acy_wpconductivity, pred_dpe_wpconductivity, 
                                        pred_lst_wpconductivity, pred_cal_wpconductivity, 
                                        pred_sro_wpconductivity) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

wpconductivity_predictions$species <- factor(wpconductivity_predictions$species, 
                                             levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Save outputs ----
saveRDS(list(acy = wpconductivity_acy_final, dpe = wpconductivity_dpe_final, 
             lst = wpconductivity_lst_final, cal = wpconductivity_cal_final, 
             sro = wpconductivity_sro_final), 
        here::here("outputs", "models", "wpconductivity_models.rds"))
saveRDS(list(acy = wpconductivity_acy_emmeans, dpe = wpconductivity_dpe_emmeans, 
             lst = wpconductivity_lst_emmeans, cal = wpconductivity_cal_emmeans, 
             sro = wpconductivity_sro_emmeans),
        here::here("outputs", "models", "wpconductivity_emmeans.rds"))
saveRDS(wpconductivity_predictions, 
        here::here("outputs", "models", "wpconductivity_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC TRANSPIRATION (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
leaf_trans_acy_null <- lmer(leaf_specific_transpiration_log ~ 1 + (1|greenhouse/plant), 
                            data = data_water_relations[data_water_relations$species == "ACY",])
leaf_trans_acy_m1 <- update(leaf_trans_acy_null, .~ . + drought_phase)
leaf_trans_acy_m2 <- update(leaf_trans_acy_m1, .~ . + co2)
leaf_trans_acy_m3 <- update(leaf_trans_acy_m2, .~ . + drought_phase:co2)

anova(leaf_trans_acy_null, leaf_trans_acy_m1, leaf_trans_acy_m2, leaf_trans_acy_m3)

leaf_trans_acy_final <- lmer(leaf_specific_transpiration_log ~ drought_phase + co2 + 
                               (1|greenhouse/plant), 
                             data = data_water_relations[data_water_relations$species == "ACY",])
anova(leaf_trans_acy_final)
summary(leaf_trans_acy_final)
plot(simulateResiduals(leaf_trans_acy_final))

# DPE ----
leaf_trans_dpe_null <- lmer(leaf_specific_transpiration_log ~ 1 + (1|greenhouse/plant), 
                            data = data_water_relations[data_water_relations$species == "DPE",])
leaf_trans_dpe_m1 <- update(leaf_trans_dpe_null, .~ . + drought_phase)
leaf_trans_dpe_m2 <- update(leaf_trans_dpe_m1, .~ . + co2)
leaf_trans_dpe_m3 <- update(leaf_trans_dpe_m2, .~ . + drought_phase:co2)

anova(leaf_trans_dpe_null, leaf_trans_dpe_m1, leaf_trans_dpe_m2, leaf_trans_dpe_m3)

leaf_trans_dpe_final <- lmer(leaf_specific_transpiration_log ~ drought_phase + co2 + 
                               (1|greenhouse/plant), 
                             data = data_water_relations[data_water_relations$species == "DPE",])
anova(leaf_trans_dpe_final)
summary(leaf_trans_dpe_final)
plot(simulateResiduals(leaf_trans_dpe_final))

# LST ----
leaf_trans_lst_null <- lmer(leaf_specific_transpiration_log ~ 1 + (1|greenhouse/plant), 
                            data = data_water_relations[data_water_relations$species == "LST",])
leaf_trans_lst_m1 <- update(leaf_trans_lst_null, .~ . + drought_phase)
leaf_trans_lst_m2 <- update(leaf_trans_lst_m1, .~ . + co2)
leaf_trans_lst_m3 <- update(leaf_trans_lst_m2, .~ . + drought_phase:co2)

anova(leaf_trans_lst_null, leaf_trans_lst_m1, leaf_trans_lst_m2, leaf_trans_lst_m3)

leaf_trans_lst_final <- lmer(leaf_specific_transpiration_log ~ drought_phase + co2 + 
                               (1|greenhouse/plant), 
                             data = data_water_relations[data_water_relations$species == "LST",])
anova(leaf_trans_lst_final)
summary(leaf_trans_lst_final)
plot(simulateResiduals(leaf_trans_lst_final))

# CAL ----
leaf_trans_cal_null <- lmer(leaf_specific_transpiration_log ~ 1 + (1|greenhouse/plant), 
                            data = data_water_relations[data_water_relations$species == "CAL",])
leaf_trans_cal_m1 <- update(leaf_trans_cal_null, .~ . + drought_phase)
leaf_trans_cal_m2 <- update(leaf_trans_cal_m1, .~ . + co2)
leaf_trans_cal_m3 <- update(leaf_trans_cal_m2, .~ . + drought_phase:co2)

anova(leaf_trans_cal_null, leaf_trans_cal_m1, leaf_trans_cal_m2, leaf_trans_cal_m3)

leaf_trans_cal_final <- lmer(leaf_specific_transpiration_log ~ drought_phase * co2 + 
                               (1|greenhouse/plant), 
                             data = data_water_relations[data_water_relations$species == "CAL",])
anova(leaf_trans_cal_final)
summary(leaf_trans_cal_final)
plot(simulateResiduals(leaf_trans_cal_final))

# SRO ----
leaf_trans_sro_null <- lmer(leaf_specific_transpiration_log ~ 1 + (1|greenhouse/plant), 
                            data = data_water_relations[data_water_relations$species == "SRO",])
leaf_trans_sro_m1 <- update(leaf_trans_sro_null, .~ . + drought_phase)
leaf_trans_sro_m2 <- update(leaf_trans_sro_m1, .~ . + co2)
leaf_trans_sro_m3 <- update(leaf_trans_sro_m2, .~ . + drought_phase:co2)

anova(leaf_trans_sro_null, leaf_trans_sro_m1, leaf_trans_sro_m2, leaf_trans_sro_m3)

leaf_trans_sro_final <- lmer(leaf_specific_transpiration_log ~ drought_phase * co2 + 
                               (1|greenhouse/plant), 
                             data = data_water_relations[data_water_relations$species == "SRO",])
anova(leaf_trans_sro_final)
summary(leaf_trans_sro_final)
plot(simulateResiduals(leaf_trans_sro_final))

# Post-hoc comparisons ----
leaf_trans_acy_emmeans <- emmeans(leaf_trans_acy_final, pairwise ~ co2 | drought_phase)
leaf_trans_dpe_emmeans <- emmeans(leaf_trans_dpe_final, pairwise ~ co2 | drought_phase)
leaf_trans_lst_emmeans <- emmeans(leaf_trans_lst_final, pairwise ~ co2 | drought_phase)
leaf_trans_cal_emmeans <- emmeans(leaf_trans_cal_final, pairwise ~ co2 | drought_phase)
leaf_trans_sro_emmeans <- emmeans(leaf_trans_sro_final, pairwise ~ co2 | drought_phase)


# Generate predictions ----
pred_acy_leaf_trans <- ggpredict(leaf_trans_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_leaf_trans <- ggpredict(leaf_trans_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_leaf_trans <- ggpredict(leaf_trans_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_leaf_trans <- ggpredict(leaf_trans_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_leaf_trans <- ggpredict(leaf_trans_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
leaf_trans_predictions <- bind_rows(pred_acy_leaf_trans, pred_dpe_leaf_trans, 
                                    pred_lst_leaf_trans, pred_cal_leaf_trans, 
                                    pred_sro_leaf_trans) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

leaf_trans_predictions$species <- factor(leaf_trans_predictions$species, 
                                         levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Save outputs ----
saveRDS(list(acy = leaf_trans_acy_final, dpe = leaf_trans_dpe_final, 
             lst = leaf_trans_lst_final, cal = leaf_trans_cal_final, 
             sro = leaf_trans_sro_final), 
        here::here("outputs", "models", "leaf_trans_models.rds"))
saveRDS(list(acy = leaf_trans_acy_emmeans, dpe = leaf_trans_dpe_emmeans, 
             lst = leaf_trans_lst_emmeans, cal = leaf_trans_cal_emmeans, 
             sro = leaf_trans_sro_emmeans),
        here::here("outputs", "models", "leaf_trans_emmeans.rds"))
saveRDS(leaf_trans_predictions, 
        here::here("outputs", "models", "leaf_trans_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC CONDUCTANCE (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
lc_acy_null <- lmer(leaf_specific_conductance_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "ACY",])
lc_acy_m1 <- update(lc_acy_null, .~ . + drought_phase)
lc_acy_m2 <- update(lc_acy_m1, .~ . + co2)
lc_acy_m3 <- update(lc_acy_m2, .~ . + drought_phase:co2)

anova(lc_acy_null, lc_acy_m1, lc_acy_m2, lc_acy_m3)

lc_acy_final <- lmer(leaf_specific_conductance_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "ACY",])
anova(lc_acy_final)
summary(lc_acy_final)
plot(simulateResiduals(lc_acy_final))

# DPE ----
lc_dpe_null <- lmer(leaf_specific_conductance_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "DPE",])
lc_dpe_m1 <- update(lc_dpe_null, .~ . + drought_phase)
lc_dpe_m2 <- update(lc_dpe_m1, .~ . + co2)
lc_dpe_m3 <- update(lc_dpe_m2, .~ . + drought_phase:co2)

anova(lc_dpe_null, lc_dpe_m1, lc_dpe_m2, lc_dpe_m3)

lc_dpe_final <- lmer(leaf_specific_conductance_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "DPE",])
anova(lc_dpe_final)
summary(lc_dpe_final)
plot(simulateResiduals(lc_dpe_final))

# LST ----
lc_lst_null <- lmer(leaf_specific_conductance_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "LST",])
lc_lst_m1 <- update(lc_lst_null, .~ . + drought_phase)
lc_lst_m2 <- update(lc_lst_m1, .~ . + co2)
lc_lst_m3 <- update(lc_lst_m2, .~ . + drought_phase:co2)

anova(lc_lst_null, lc_lst_m1, lc_lst_m2, lc_lst_m3)

lc_lst_final <- lmer(leaf_specific_conductance_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "LST",])
anova(lc_lst_final)
summary(lc_lst_final)
plot(simulateResiduals(lc_lst_final))

# CAL ----
lc_cal_null <- lmer(leaf_specific_conductance_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "CAL",])
lc_cal_m1 <- update(lc_cal_null, .~ . + drought_phase)
lc_cal_m2 <- update(lc_cal_m1, .~ . + co2)
lc_cal_m3 <- update(lc_cal_m2, .~ . + drought_phase:co2)

anova(lc_cal_null, lc_cal_m1, lc_cal_m2, lc_cal_m3)

lc_cal_final <- lmer(leaf_specific_conductance_log ~ drought_phase + co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "CAL",])
anova(lc_cal_final)
summary(lc_cal_final)
plot(simulateResiduals(lc_cal_final))

# SRO ----
lc_sro_null <- lmer(leaf_specific_conductance_log ~ 1 + (1|greenhouse/plant), 
                    data = data_water_relations[data_water_relations$species == "SRO",])
lc_sro_m1 <- update(lc_sro_null, .~ . + drought_phase)
lc_sro_m2 <- update(lc_sro_m1, .~ . + co2)
lc_sro_m3 <- update(lc_sro_m2, .~ . + drought_phase:co2)

anova(lc_sro_null, lc_sro_m1, lc_sro_m2, lc_sro_m3)

lc_sro_final <- lmer(leaf_specific_conductance_log ~ drought_phase * co2 + 
                       (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "SRO",])
anova(lc_sro_final)
summary(lc_sro_final)
plot(simulateResiduals(lc_sro_final))

# Post-hoc comparisons ----
lc_acy_emmeans <- emmeans(lc_acy_final, pairwise ~ co2 | drought_phase)
lc_dpe_emmeans <- emmeans(lc_dpe_final, pairwise ~ co2 | drought_phase)
lc_lst_emmeans <- emmeans(lc_lst_final, pairwise ~ co2 | drought_phase)
lc_cal_emmeans <- emmeans(lc_cal_final, pairwise ~ co2 | drought_phase)
lc_sro_emmeans <- emmeans(lc_sro_final, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_lc <- ggpredict(lc_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_lc <- ggpredict(lc_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_lc <- ggpredict(lc_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_lc <- ggpredict(lc_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_lc <- ggpredict(lc_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
lc_predictions <- bind_rows(pred_acy_lc, pred_dpe_lc, pred_lst_lc, 
                            pred_cal_lc, pred_sro_lc) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

lc_predictions$species <- factor(lc_predictions$species, 
                                 levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Save outputs ----
saveRDS(list(acy = lc_acy_final, dpe = lc_dpe_final, lst = lc_lst_final, 
             cal = lc_cal_final, sro = lc_sro_final), 
        here::here("outputs", "models", "lc_models.rds"))
saveRDS(list(acy = lc_acy_emmeans, dpe = lc_dpe_emmeans, lst = lc_lst_emmeans,
             cal = lc_cal_emmeans, sro = lc_sro_emmeans),
        here::here("outputs", "models", "lc_emmeans.rds"))
saveRDS(lc_predictions, 
        here::here("outputs", "models", "lc_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC CONDUCTIVITY (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
lconductivity_acy_null <- lmer(leaf_specific_conductivity_log ~ 1 + (1|greenhouse/plant), 
                               data = data_water_relations[data_water_relations$species == "ACY",])
lconductivity_acy_m1 <- update(lconductivity_acy_null, .~ . + drought_phase)
lconductivity_acy_m2 <- update(lconductivity_acy_m1, .~ . + co2)
lconductivity_acy_m3 <- update(lconductivity_acy_m2, .~ . + drought_phase:co2)

anova(lconductivity_acy_null, lconductivity_acy_m1, lconductivity_acy_m2, lconductivity_acy_m3)

lconductivity_acy_final <- lmer(leaf_specific_conductivity_log ~ drought_phase + co2 + 
                                  (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "ACY",])
anova(lconductivity_acy_final)
summary(lconductivity_acy_final)
plot(simulateResiduals(lconductivity_acy_final))

# DPE ----
lconductivity_dpe_null <- lmer(leaf_specific_conductivity_log ~ 1 + (1|greenhouse/plant), 
                               data = data_water_relations[data_water_relations$species == "DPE",])
lconductivity_dpe_m1 <- update(lconductivity_dpe_null, .~ . + drought_phase)
lconductivity_dpe_m2 <- update(lconductivity_dpe_m1, .~ . + co2)
lconductivity_dpe_m3 <- update(lconductivity_dpe_m2, .~ . + drought_phase:co2)

anova(lconductivity_dpe_null, lconductivity_dpe_m1, lconductivity_dpe_m2, lconductivity_dpe_m3)

lconductivity_dpe_final <- lmer(leaf_specific_conductivity_log ~ drought_phase + co2 + 
                                  (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "DPE",])
anova(lconductivity_dpe_final)
summary(lconductivity_dpe_final)
plot(simulateResiduals(lconductivity_dpe_final))

# LST ----
lconductivity_lst_null <- lmer(leaf_specific_conductivity_log ~ 1 + (1|greenhouse/plant), 
                               data = data_water_relations[data_water_relations$species == "LST",])
lconductivity_lst_m1 <- update(lconductivity_lst_null, .~ . + drought_phase)
lconductivity_lst_m2 <- update(lconductivity_lst_m1, .~ . + co2)
lconductivity_lst_m3 <- update(lconductivity_lst_m2, .~ . + drought_phase:co2)

anova(lconductivity_lst_null, lconductivity_lst_m1, lconductivity_lst_m2, lconductivity_lst_m3)

lconductivity_lst_final <- lmer(leaf_specific_conductivity_log ~ drought_phase + co2 + 
                                  (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "LST",])
anova(lconductivity_lst_final)
summary(lconductivity_lst_final)
plot(simulateResiduals(lconductivity_lst_final))

# CAL ----
lconductivity_cal_null <- lmer(leaf_specific_conductivity_log ~ 1 + (1|greenhouse/plant), 
                               data = data_water_relations[data_water_relations$species == "CAL",])
lconductivity_cal_m1 <- update(lconductivity_cal_null, .~ . + drought_phase)
lconductivity_cal_m2 <- update(lconductivity_cal_m1, .~ . + co2)
lconductivity_cal_m3 <- update(lconductivity_cal_m2, .~ . + drought_phase:co2)

anova(lconductivity_cal_null, lconductivity_cal_m1, lconductivity_cal_m2, lconductivity_cal_m3)

lconductivity_cal_final <- lmer(leaf_specific_conductivity_log ~ drought_phase + co2 + 
                                  (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "CAL",])
anova(lconductivity_cal_final)
summary(lconductivity_cal_final)
plot(simulateResiduals(lconductivity_cal_final))

# SRO ----
lconductivity_sro_null <- lmer(leaf_specific_conductivity_log ~ 1 + (1|greenhouse/plant), 
                               data = data_water_relations[data_water_relations$species == "SRO",])
lconductivity_sro_m1 <- update(lconductivity_sro_null, .~ . + drought_phase)
lconductivity_sro_m2 <- update(lconductivity_sro_m1, .~ . + co2)
lconductivity_sro_m3 <- update(lconductivity_sro_m2, .~ . + drought_phase:co2)

anova(lconductivity_sro_null, lconductivity_sro_m1, lconductivity_sro_m2, lconductivity_sro_m3)

lconductivity_sro_final <- lmer(leaf_specific_conductivity_log ~ drought_phase * co2 + 
                                  (1|greenhouse/plant), 
                                data = data_water_relations[data_water_relations$species == "SRO",])
anova(lconductivity_sro_final)
summary(lconductivity_sro_final)
plot(simulateResiduals(lconductivity_sro_final))

# Post-hoc comparisons ----
lconductivity_acy_emmeans <- emmeans(lconductivity_acy_final, pairwise ~ co2 | drought_phase)
lconductivity_dpe_emmeans <- emmeans(lconductivity_dpe_final, pairwise ~ co2 | drought_phase)
lconductivity_lst_emmeans <- emmeans(lconductivity_lst_final, pairwise ~ co2 | drought_phase)
lconductivity_cal_emmeans <- emmeans(lconductivity_cal_final, pairwise ~ co2 | drought_phase)
lconductivity_sro_emmeans <- emmeans(lconductivity_sro_final, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_lconductivity <- ggpredict(lconductivity_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_lconductivity <- ggpredict(lconductivity_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_lconductivity <- ggpredict(lconductivity_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_lconductivity <- ggpredict(lconductivity_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_lconductivity <- ggpredict(lconductivity_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
lconductivity_predictions <- bind_rows(pred_acy_lconductivity, pred_dpe_lconductivity, 
                                       pred_lst_lconductivity, pred_cal_lconductivity, 
                                       pred_sro_lconductivity) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

lconductivity_predictions$species <- factor(lconductivity_predictions$species, 
                                            levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Save outputs ----
saveRDS(list(acy = lconductivity_acy_final, dpe = lconductivity_dpe_final, 
             lst = lconductivity_lst_final, cal = lconductivity_cal_final, 
             sro = lconductivity_sro_final), 
        here::here("outputs", "models", "lconductivity_models.rds"))
saveRDS(list(acy = lconductivity_acy_emmeans, dpe = lconductivity_dpe_emmeans, 
             lst = lconductivity_lst_emmeans, cal = lconductivity_cal_emmeans, 
             sro = lconductivity_sro_emmeans),
        here::here("outputs", "models", "lconductivity_emmeans.rds"))
saveRDS(lconductivity_predictions, 
        here::here("outputs", "models", "lconductivity_predictions.rds"))


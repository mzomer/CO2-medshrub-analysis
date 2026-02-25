# ─────────────────────────────────────────────────────────────────────
# 03c_models_water_status.R — Statistical models for water status
# ─────────────────────────────────────────────────────────────────────
# Models for: Midday water potential, Osmotic potential
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data ----
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))

# ═════════════════════════════════════════════════════════════════════
# MIDDAY WATER POTENTIAL (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
ymd_acy_null <- lmer(water_potential_midday_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "ACY",])
ymd_acy_m1 <- update(ymd_acy_null, .~ . + drought_phase)
ymd_acy_m2 <- update(ymd_acy_m1, .~ . + co2)
ymd_acy_m3 <- update(ymd_acy_m2, .~ . + drought_phase:co2)

anova(ymd_acy_null, ymd_acy_m1, ymd_acy_m2, ymd_acy_m3) #***, ns, ns

ymd_acy <- lmer(water_potential_midday_log ~ drought_phase + co2 + 
                  (1|greenhouse/plant), 
                data = data_water_relations[data_water_relations$species == "ACY",])
anova(ymd_acy)
summary(ymd_acy)
plot(simulateResiduals(ymd_acy))

# DPE ----
ymd_dpe_null <- lmer(water_potential_midday_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "DPE",])
ymd_dpe_m1 <- update(ymd_dpe_null, .~ . + drought_phase)
ymd_dpe_m2 <- update(ymd_dpe_m1, .~ . + co2)
ymd_dpe_m3 <- update(ymd_dpe_m2, .~ . + drought_phase:co2)

anova(ymd_dpe_null, ymd_dpe_m1, ymd_dpe_m2, ymd_dpe_m3) #***, ns, ns

ymd_dpe <- lmer(water_potential_midday_log ~ drought_phase + co2 + 
                  (1|greenhouse/plant), 
                data = data_water_relations[data_water_relations$species == "DPE",])
anova(ymd_dpe)
summary(ymd_dpe)
plot(simulateResiduals(ymd_dpe))

# LST ----
ymd_lst_null <- lmer(water_potential_midday_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "LST",])
ymd_lst_m1 <- update(ymd_lst_null, .~ . + drought_phase)
ymd_lst_m2 <- update(ymd_lst_m1, .~ . + co2)
ymd_lst_m3 <- update(ymd_lst_m2, .~ . + drought_phase:co2)

anova(ymd_lst_null, ymd_lst_m1, ymd_lst_m2, ymd_lst_m3) #***, ns, .

ymd_lst <- lmer(water_potential_midday_log ~ drought_phase + co2 + 
                  (1|greenhouse/plant), 
                data = data_water_relations[data_water_relations$species == "LST",])
anova(ymd_lst)
summary(ymd_lst)
plot(simulateResiduals(ymd_lst))

# CAL ----
ymd_cal_null <- lmer(water_potential_midday_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "CAL",])
ymd_cal_m1 <- update(ymd_cal_null, .~ . + drought_phase)
ymd_cal_m2 <- update(ymd_cal_m1, .~ . + co2)
ymd_cal_m3 <- update(ymd_cal_m2, .~ . + drought_phase:co2)

anova(ymd_cal_null, ymd_cal_m1, ymd_cal_m2, ymd_cal_m3) #***, **, ns

ymd_cal <- lmer(water_potential_midday_log ~ drought_phase + co2 + 
                  (1|greenhouse/plant), 
                data = data_water_relations[data_water_relations$species == "CAL",])
anova(ymd_cal)
summary(ymd_cal)
plot(simulateResiduals(ymd_cal))

# SRO ----
ymd_sro_null <- lmer(water_potential_midday_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "SRO",])
ymd_sro_m1 <- update(ymd_sro_null, .~ . + drought_phase)
ymd_sro_m2 <- update(ymd_sro_m1, .~ . + co2)
ymd_sro_m3 <- update(ymd_sro_m2, .~ . + drought_phase:co2)

anova(ymd_sro_null, ymd_sro_m1, ymd_sro_m2, ymd_sro_m3) #***, ns, .

ymd_sro <- lmer(water_potential_midday_log ~ drought_phase + co2 + 
                  (1|greenhouse/plant), 
                data = data_water_relations[data_water_relations$species == "SRO",])
anova(ymd_sro)
summary(ymd_sro)
plot(simulateResiduals(ymd_sro))

# Post-hoc comparisons ----
ymd_acy_emmeans <- emmeans(ymd_acy, pairwise ~ co2 | drought_phase)
ymd_dpe_emmeans <- emmeans(ymd_dpe, pairwise ~ co2 | drought_phase)
ymd_lst_emmeans <- emmeans(ymd_lst, pairwise ~ co2 | drought_phase)
ymd_cal_emmeans <- emmeans(ymd_cal, pairwise ~ co2 | drought_phase)
ymd_sro_emmeans <- emmeans(ymd_sro, pairwise ~ co2 | drought_phase)

ymd_acy_emmeans
ymd_dpe_emmeans
ymd_lst_emmeans
ymd_cal_emmeans
ymd_sro_emmeans


# Generate predictions ----
pred_acy <- ggpredict(ymd_acy, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe <- ggpredict(ymd_dpe, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst <- ggpredict(ymd_lst, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal <- ggpredict(ymd_cal, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro <- ggpredict(ymd_sro, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
ymd_predictions <- bind_rows(pred_acy, pred_dpe, pred_lst, pred_cal, pred_sro) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

ymd_predictions$species <- factor(ymd_predictions$species, 
                                  levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Convert to regular data frame before saving
ymd_predictions <- as.data.frame(ymd_predictions)

# Save outputs ----
saveRDS(list(acy = ymd_acy, dpe = ymd_dpe, lst = ymd_lst, 
             cal = ymd_cal, sro = ymd_sro), 
        here::here("outputs", "models", "ymd_models.rds"))
saveRDS(list(acy = ymd_acy_emmeans, dpe = ymd_dpe_emmeans, lst = ymd_lst_emmeans,
             cal = ymd_cal_emmeans, sro = ymd_sro_emmeans),
        here::here("outputs", "models", "ymd_emmeans.rds"))
saveRDS(ymd_predictions, 
        here::here("outputs", "models", "ymd_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# OSMOTIC POTENTIAL (by species) ----
# ═════════════════════════════════════════════════════════════════════

# ACY ----
osm_acy_null <- lmer(osmotic_potential_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "ACY",])
osm_acy_m1 <- update(osm_acy_null, .~ . + drought_phase)
osm_acy_m2 <- update(osm_acy_m1, .~ . + co2)
osm_acy_m3 <- update(osm_acy_m2, .~ . + drought_phase:co2)

anova(osm_acy_null, osm_acy_m1, osm_acy_m2, osm_acy_m3) #***, ns, ns

osm_acy_final <- lmer(osmotic_potential_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "ACY",])
anova(osm_acy_final)
summary(osm_acy_final)
plot(simulateResiduals(osm_acy_final))

# DPE ----
osm_dpe_null <- lmer(osmotic_potential_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "DPE",])
osm_dpe_m1 <- update(osm_dpe_null, .~ . + drought_phase)
osm_dpe_m2 <- update(osm_dpe_m1, .~ . + co2)
osm_dpe_m3 <- update(osm_dpe_m2, .~ . + drought_phase:co2)

anova(osm_dpe_null, osm_dpe_m1, osm_dpe_m2, osm_dpe_m3) # ***, ns, *** interaction significant

osm_dpe_final <- lmer(osmotic_potential_log ~ drought_phase * co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "DPE",])
anova(osm_dpe_final)
summary(osm_dpe_final)
plot(simulateResiduals(osm_dpe_final))

# LST ----
osm_lst_null <- lmer(osmotic_potential_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "LST",])
osm_lst_m1 <- update(osm_lst_null, .~ . + drought_phase)
osm_lst_m2 <- update(osm_lst_m1, .~ . + co2)
osm_lst_m3 <- update(osm_lst_m2, .~ . + drought_phase:co2)

anova(osm_lst_null, osm_lst_m1, osm_lst_m2, osm_lst_m3) #***, ns, ns

osm_lst_final <- lmer(osmotic_potential_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "LST",])
anova(osm_lst_final)
summary(osm_lst_final)
plot(simulateResiduals(osm_lst_final))

# CAL ----
osm_cal_null <- lmer(osmotic_potential_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "CAL",])
osm_cal_m1 <- update(osm_cal_null, .~ . + drought_phase)
osm_cal_m2 <- update(osm_cal_m1, .~ . + co2)
osm_cal_m3 <- update(osm_cal_m2, .~ . + drought_phase:co2)

anova(osm_cal_null, osm_cal_m1, osm_cal_m2, osm_cal_m3) #**, *, ns

osm_cal_final <- lmer(osmotic_potential_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "CAL",])
anova(osm_cal_final)
summary(osm_cal_final)
plot(simulateResiduals(osm_cal_final))

# SRO ----
osm_sro_null <- lmer(osmotic_potential_log ~ 1 + (1|greenhouse/plant), 
                     data = data_water_relations[data_water_relations$species == "SRO",])
osm_sro_m1 <- update(osm_sro_null, .~ . + drought_phase)
osm_sro_m2 <- update(osm_sro_m1, .~ . + co2)
osm_sro_m3 <- update(osm_sro_m2, .~ . + drought_phase:co2)

anova(osm_sro_null, osm_sro_m1, osm_sro_m2, osm_sro_m3) #***, *, ** (interaction has poor model fit)

osm_sro_final <- lmer(osmotic_potential_log ~ drought_phase + co2 + 
                        (1|greenhouse/plant), 
                      data = data_water_relations[data_water_relations$species == "SRO",])
anova(osm_sro_final)
summary(osm_sro_final)
plot(simulateResiduals(osm_sro_final))

# Post-hoc comparisons ----
osm_acy_emmeans <- emmeans(osm_acy_final, pairwise ~ co2 | drought_phase)
osm_dpe_emmeans <- emmeans(osm_dpe_final, pairwise ~ co2 | drought_phase)
osm_lst_emmeans <- emmeans(osm_lst_final, pairwise ~ co2 | drought_phase)
osm_cal_emmeans <- emmeans(osm_cal_final, pairwise ~ co2 | drought_phase)
osm_sro_emmeans <- emmeans(osm_sro_final, pairwise ~ co2 | drought_phase)

# Generate predictions ----
pred_acy_osm <- ggpredict(osm_acy_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "ACY")
pred_dpe_osm <- ggpredict(osm_dpe_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "DPE")
pred_lst_osm <- ggpredict(osm_lst_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "LST")
pred_cal_osm <- ggpredict(osm_cal_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "CAL")
pred_sro_osm <- ggpredict(osm_sro_final, terms = c("drought_phase", "co2")) %>% 
  mutate(species = "SRO")

# Combine predictions
osm_predictions <- bind_rows(pred_acy_osm, pred_dpe_osm, pred_lst_osm, 
                             pred_cal_osm, pred_sro_osm) %>%
  drop_na() %>%
  rename(drought_phase = x, co2 = group)

osm_predictions$species <- factor(osm_predictions$species, 
                                  levels = c("ACY", "DPE", "LST", "CAL", "SRO"))


# Convert to regular data frame before saving
osm_predictions <- as.data.frame(osm_predictions)


# Save outputs ----
saveRDS(list(acy = osm_acy_final, dpe = osm_dpe_final, lst = osm_lst_final, 
             cal = osm_cal_final, sro = osm_sro_final), 
        here::here("outputs", "models", "osm_models.rds"))
saveRDS(list(acy = osm_acy_emmeans, dpe = osm_dpe_emmeans, lst = osm_lst_emmeans,
             cal = osm_cal_emmeans, sro = osm_sro_emmeans),
        here::here("outputs", "models", "osm_emmeans.rds"))
saveRDS(osm_predictions, 
        here::here("outputs", "models", "osm_predictions.rds"))


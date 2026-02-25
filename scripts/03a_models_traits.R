# ─────────────────────────────────────────────────────────────────────
# 03a_models_traits.R — Statistical models for plant traits
# ─────────────────────────────────────────────────────────────────────
# Models for: Stomatal density, SLA, Non-structural carbohydrates
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data ----
data_traits_biomass <- readRDS(here::here("data", "processed", "data_traits_biomass.rds"))
data_stomatal_density <- readRDS(here::here("data", "processed", "data_stomatal_density.rds"))
data_carbohydrates <- readRDS(here::here("data", "processed", "data_carbohydrates.rds"))


# ═════════════════════════════════════════════════════════════════════
# STOMATAL DENSITY ----
# ═════════════════════════════════════════════════════════════════════

## Model selection ----
stomata_null <- lmer(mean_stomatal_density ~ 1 + (1|greenhouse), 
                     data = data_stomatal_density)
stomata_m1 <- update(stomata_null, .~ . + species)
stomata_m2 <- update(stomata_m1, .~ . + co2)
stomata_m3 <- update(stomata_m2, .~ . + species:co2)

anova(stomata_null, stomata_m1, stomata_m2, stomata_m3) # ***, ., ns

## Final model ----
stomata_final_model <- lmer(mean_stomatal_density ~ species + co2 + (1|greenhouse), 
                            data = data_stomatal_density)

# Model diagnostics
anova(stomata_final_model)
summary(stomata_final_model)
plot(simulateResiduals(stomata_final_model))

# Post-hoc comparisons
stomata_emmeans <- emmeans(stomata_final_model, pairwise ~ co2 | species, 
                           adjust = "bonferroni")
stomata_emmeans

# Generate predictions for plotting
stomata_predictions <- ggpredict(stomata_final_model, 
                                 terms = c("species", "co2"))

# Save outputs
saveRDS(stomata_final_model, 
        here::here("outputs", "models", "stomata_model.rds"))
saveRDS(stomata_emmeans, 
        here::here("outputs", "models", "stomata_emmeans.rds"))
saveRDS(stomata_predictions, 
        here::here("outputs", "models", "stomata_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# SPECIFIC LEAF AREA (SLA) ----
# ═════════════════════════════════════════════════════════════════════

## Model selection ----
sla_null <- lmer(SLA ~ 1 + (1|greenhouse), data = data_traits_biomass)
sla_m1 <- update(sla_null, .~ . + species)
sla_m2 <- update(sla_m1, .~ . + co2)
sla_m3 <- update(sla_m2, .~ . + species:co2)

anova(sla_null, sla_m1, sla_m2, sla_m3) # ***, ***, ns

## Final model ----
sla_final_model <- lmer(SLA ~ species + co2 + (1|greenhouse), 
                        data = data_traits_biomass)

# Model diagnostics
anova(sla_final_model)
summary(sla_final_model)
plot(simulateResiduals(sla_final_model))

# Post-hoc comparisons
sla_emmeans <- emmeans(sla_final_model, pairwise ~ co2 | species, 
                       adjust = "bonferroni")
sla_emmeans

# Generate predictions for plotting
sla_predictions <- ggpredict(sla_final_model, 
                             terms = c("species", "co2"))

# Save outputs
saveRDS(sla_final_model, 
        here::here("outputs", "models", "sla_model.rds"))
saveRDS(sla_emmeans, 
        here::here("outputs", "models", "sla_emmeans.rds"))
saveRDS(sla_predictions, 
        here::here("outputs", "models", "sla_predictions.rds"))

# ═════════════════════════════════════════════════════════════════════
# CARBOHYDRATES ----
# ═════════════════════════════════════════════════════════════════════

## STARCH ----
### Model selection ----
starch_null <- lmer(starch ~ 1 + (1|greenhouse), data = data_carbohydrates)
starch_m1 <- update(starch_null, .~ . + species)
starch_m2 <- update(starch_m1, .~ . + co2)
starch_m3 <- update(starch_m2, .~ . + species:co2)

anova(starch_null, starch_m1, starch_m2, starch_m3) # *, ns, ns

### Final model -----
starch_final_model <- lmer(starch ~ species + (1|greenhouse), 
                           data = data_carbohydrates)

# Model diagnostics
anova(starch_final_model)
summary(starch_final_model)
plot(simulateResiduals(starch_final_model))

# Post-hoc comparisons
starch_emmeans <- emmeans(starch_final_model, pairwise ~ species, 
                          adjust = "bonferroni")
starch_emmeans

# Generate predictions for plotting
starch_predictions <- ggpredict(starch_final_model, terms = c("species"))

# Save outputs
saveRDS(starch_final_model, here::here("outputs", "models", "starch_model.rds"))
saveRDS(starch_emmeans, here::here("outputs", "models", "starch_emmeans.rds"))
saveRDS(starch_predictions, here::here("outputs", "models", "starch_predictions.rds"))

## SOLUBLE SUGARS ----
###  Model selection ----
sugars_null <- lmer(solublesugars_total ~ 1 + (1|greenhouse), data = data_carbohydrates)
sugars_m1 <- update(sugars_null, .~ . + species)
sugars_m2 <- update(sugars_m1, .~ . + co2)
sugars_m3 <- update(sugars_m2, .~ . + species:co2)

anova(sugars_null, sugars_m1, sugars_m2, sugars_m3) #ns, ns, ns

###  Final model----
sugars_final_model <- lmer(solublesugars_total ~ species + (1|greenhouse), 
                           data = data_carbohydrates)

# Model diagnostics
anova(sugars_final_model)
summary(sugars_final_model)
plot(simulateResiduals(sugars_final_model))

# Post-hoc comparisons
sugars_emmeans <- emmeans(sugars_final_model, pairwise ~ species, 
                          adjust = "bonferroni")
sugars_emmeans

# Generate predictions for plotting
sugars_predictions <- ggpredict(sugars_final_model, terms = c("species"))

# Save outputs
saveRDS(sugars_final_model, here::here("outputs", "models", "sugars_model.rds"))
saveRDS(sugars_emmeans, here::here("outputs", "models", "sugars_emmeans.rds"))
saveRDS(sugars_predictions, here::here("outputs", "models", "sugars_predictions.rds"))

## NON-STRUCTURAL CARBOHYDRATES (NSC) ----
###  Model selection ----
nsc_null <- lmer(NSC ~ 1 + (1|greenhouse), data = data_carbohydrates)
nsc_m1 <- update(nsc_null, .~ . + species)
nsc_m2 <- update(nsc_m1, .~ . + co2)
nsc_m3 <- update(nsc_m2, .~ . + species:co2)

anova(nsc_null, nsc_m1, nsc_m2, nsc_m3) #ns, ns, ns

###  Final model ----
nsc_final_model <- lmer(NSC ~ species + (1|greenhouse), 
                        data = data_carbohydrates)

# Model diagnostics
anova(nsc_final_model)
summary(nsc_final_model)
plot(simulateResiduals(nsc_final_model))

# Post-hoc comparisons
nsc_emmeans <- emmeans(nsc_final_model, pairwise ~ species, 
                       adjust = "bonferroni")
nsc_emmeans

# Generate predictions for plotting
nsc_predictions <- ggpredict(nsc_final_model, terms = c("species"))

# Save outputs
saveRDS(nsc_final_model, here::here("outputs", "models", "nsc_model.rds"))
saveRDS(nsc_emmeans, here::here("outputs", "models", "nsc_emmeans.rds"))
saveRDS(nsc_predictions, here::here("outputs", "models", "nsc_predictions.rds"))


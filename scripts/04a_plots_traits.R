# ─────────────────────────────────────────────────────────────────────
# 04a_plots_traits.R — Plots for plant traits
# ─────────────────────────────────────────────────────────────────────
# Plots for: Stomatal density, SLA
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data for raw data points ----
data_traits_biomass <- readRDS(here::here("data", "processed", "data_traits_biomass.rds"))
data_stomatal_density <- readRDS(here::here("data", "processed", "data_stomatal_density.rds"))
data_carbohydrates <- readRDS(here::here("data", "processed", "data_carbohydrates.rds"))

data_traits_biomass %>%
  summarise(
    n_SLA = sum(!is.na(SLA)),
    n_aboveground = sum(!is.na(total_aboveground_biomass)),
    n_belowground = sum(!is.na(total_belowground_biomass)),
    n_root_shoot = sum(!is.na(root_shoot_ratio))
  )

data_stomatal_density %>%
  summarise(n_stomatal = sum(!is.na(mean_stomatal_density)))

# Load predictions from models ----
stomata_predictions <- readRDS(here::here("outputs", "models", "stomata_predictions.rds"))
sla_predictions <- readRDS(here::here("outputs", "models", "sla_predictions.rds"))
starch_predictions <- readRDS(here::here("outputs", "models", "starch_predictions.rds"))
sugars_predictions <- readRDS(here::here("outputs", "models", "sugars_predictions.rds"))
nsc_predictions <- readRDS(here::here("outputs", "models", "nsc_predictions.rds"))

# Rename variables from ggpredict output ----
names(stomata_predictions)[names(stomata_predictions) == "x"] <- "species"
names(stomata_predictions)[names(stomata_predictions) == "group"] <- "co2"

names(sla_predictions)[names(sla_predictions) == "x"] <- "species"
names(sla_predictions)[names(sla_predictions) == "group"] <- "co2"

names(starch_predictions)[names(starch_predictions) == "x"] <- "species"

names(sugars_predictions)[names(sugars_predictions) == "x"] <- "species"

names(nsc_predictions)[names(nsc_predictions) == "x"] <- "species"

# Plot settings ----
my_pal_co2 <- c("#EBCB8B", "#BF616A")
pd <- position_dodge(.5)

# ═════════════════════════════════════════════════════════════════════
# STOMATAL DENSITY PLOT ----
# ═════════════════════════════════════════════════════════════════════

stomata_plot <- ggplot(stomata_predictions, aes(x = species, y = predicted, color = co2, shape = species)) +
  # Error bars for predicted 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, size = 1, alpha = 1, position = pd) +
  
  # Raw data points filled with CO2 color scheme
  geom_point(data = data_stomatal_density, 
             aes(x = species, y = mean_stomatal_density, color = co2, shape = species, fill = co2),
             position = position_jitterdodge(dodge.width = 0.5), 
             size = 2.5, alpha = 0.5, stroke = 1) +
  
  # Predicted points with SOLID white fill for ACY & DPE (triangles)
  geom_point(data = stomata_predictions, 
             aes(x = species, y = predicted, color = co2, shape = species), fill = "white",
             size = 5, alpha = 1, position = pd, stroke = 1.5) +
  
  theme_bw(base_size = 20) +
  
  # CO2 color mapping
  scale_color_manual(name = expression(CO[2]), labels = c("Ambient", "Elevated"), values = my_pal_co2) +
  
  # Shape mapping: Ensure ACY & DPE are triangles
  scale_shape_manual(name = NULL, 
                     values = c("ACY" = 24, "DPE" = 24, "CAL" = 18, "LST" = 18, "SRO" = 18),
                     guide = "none") +
  
  # Fill mapping for raw data using CO2 color scheme
  scale_fill_manual(values = my_pal_co2) +
  
  # Remove the duplicate legend for fill
  guides(fill = "none") +
  
  labs(x = "Species", y = bquote(Mean~Stomata~"(" * mm^{-2} * ")"), title = "") +
  
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

print(stomata_plot)

# ═════════════════════════════════════════════════════════════════════
# SLA PLOT ----
# ═════════════════════════════════════════════════════════════════════

# Define annotation data for stars
annotation_data_sla <- data.frame(
  species = c("ACY", "DPE", "LST", "CAL", "SRO"),
  y = c(35, 35, 35, 35, 35),
  label = c("*", "*", "*", "*", "*"),
  size = c(6, 6, 6, 6, 6)
)

sla_plot <- ggplot(sla_predictions, aes(x = species, y = predicted, color = co2, shape = species)) +
  # Error bars for predicted SLA
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, size = 1, alpha = 1, position = pd) +
  
  # Raw SLA data points filled with CO2 color scheme
  geom_point(data = data_traits_biomass, 
             aes(x = species, y = SLA, color = co2, shape = species, fill = co2),
             position = position_jitterdodge(dodge.width = 0.5), 
             size = 2.5, alpha = 0.5, stroke = 1) +
  
  # Predicted SLA points with SOLID white fill for ACY & DPE (triangles)
  geom_point(data = sla_predictions, 
             aes(x = species, y = predicted, color = co2, shape = species), fill = "white",
             size = 5, alpha = 1, position = pd, stroke = 1.5) +
  
  geom_text(data = annotation_data_sla, aes(x = species, y = y, label = label), 
            size = 6, fontface = "bold", color = "black") +
  
  theme_bw(base_size = 20) +
  scale_y_continuous(breaks = seq(5, 35, 10), limits = c(5, 35)) +
  
  # CO2 color mapping
  scale_color_manual(name = expression(CO[2]), labels = c("Ambient", "Elevated"), values = my_pal_co2) +
  
  # Shape mapping: Ensure ACY & DPE are triangles
  scale_shape_manual(name = NULL, 
                     values = c("ACY" = 24, "DPE" = 24, "CAL" = 18, "LST" = 18, "SRO" = 18),
                     guide = "none") +
  
  # Fill mapping for raw data using CO2 color scheme
  scale_fill_manual(values = my_pal_co2) +
  
  # Remove the duplicate legend for fill
  guides(fill = "none") +
  
  labs(x = "Species", y = expression(paste("SLA (", mm^2, mg^{-1}, ")")), title = "") +
  
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

print(sla_plot)



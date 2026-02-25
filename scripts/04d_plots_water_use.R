# ─────────────────────────────────────────────────────────────────────
# 04d_plots_water_use.R — Plots for water use and hydraulics
# ─────────────────────────────────────────────────────────────────────
# Plots for: Transpiration, hydraulic conductance/conductivity (whole-plant & leaf-specific)
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data for raw data points ----
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))

# Load predictions from models ----
wt_predictions <- readRDS(here::here("outputs", "models", "wt_predictions.rds"))
wpc_predictions <- readRDS(here::here("outputs", "models", "wpc_predictions.rds"))
wpconductivity_predictions <- readRDS(here::here("outputs", "models", "wpconductivity_predictions.rds"))
leaf_trans_predictions <- readRDS(here::here("outputs", "models", "leaf_trans_predictions.rds"))
lc_predictions <- readRDS(here::here("outputs", "models", "lc_predictions.rds"))
lconductivity_predictions <- readRDS(here::here("outputs", "models", "lconductivity_predictions.rds"))


# Plot settings ----
my_pal_species <- c(ACY = "#8C510A", DPE = "#DFC27D", LST = "#C7EAE5", 
                    CAL = "#35978F", SRO = "#003C30")

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT TRANSPIRATION PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_wt <- c(5, 10, 50, 100, 200, 300, 500)
log_breaks_wt <- log(desired_breaks_wt)
log_labels_wt <- format(sprintf("%.0f", exp(log_breaks_wt)))

wt_plot_species <- ggplot() +  
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = transpiration_mean_log, 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = wt_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +  
  
  geom_point(data = wt_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_wt, labels = log_labels_wt, limits = c(1.3, 6.5)) +
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = expression("WP Transpiration (g d"^-1*")")) +
  
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = 6.5, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = 6.5, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(wt_plot_species)

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT HYDRAULIC CONDUCTANCE PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_wpc <- c(0.001, 0.01, 0.1, 0.3, 0.6)
log_breaks_wpc <- log(desired_breaks_wpc)
log_labels_wpc <- format(desired_breaks_wpc, scientific = FALSE, trim = TRUE)

wpc_plot_species <- ggplot() +  
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = whole_plant_conductance_log, 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = wpc_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +  
  
  geom_point(data = wpc_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_wpc, labels = log_labels_wpc, limits = c(-6.90, -0.3)) +
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = expression("WP Hydraulic Conductance (g d"^-1*" kPa"^-1*")")) +
  
  theme_bw(base_size = 20, base_family = "Helvetica") +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = -0.3, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = -0.3, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(wpc_plot_species)

# ═════════════════════════════════════════════════════════════════════
# WHOLE-PLANT HYDRAULIC CONDUCTIVITY PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_wpconductivity <- c(0.05, 0.5, 5, 10, 25)
log_breaks_wpconductivity <- log(desired_breaks_wpconductivity)
log_labels_wpconductivity <- format(desired_breaks_wpconductivity, scientific = FALSE, trim = TRUE)

wpconductivity_plot_species <- ggplot() +  
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = whole_plant_conductivity_log, 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = wpconductivity_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +  
  
  geom_point(data = wpconductivity_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_wpconductivity, labels = log_labels_wpconductivity, 
                     limits = c(-2.99, 3.4)) +
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +

  labs(x = "Drought Phase", y = expression("WP Hydraulic Conductivity (g d"^-1*" cm kPa"^-1*")")) +
  
  theme_bw(base_size = 20, base_family = "Helvetica") +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = 3.4, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = 3.4, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(wpconductivity_plot_species)

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC TRANSPIRATION PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_leaf_trans <- c(0.002, 0.01, 0.1,0.3, 0.5)


log_breaks_leaf_trans <- log(desired_breaks_leaf_trans)
log_labels_leaf_trans <- format(desired_breaks_leaf_trans, scientific = FALSE, trim = TRUE)

leaf_trans_plot_species <- ggplot() +  
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = leaf_specific_transpiration_log, 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = leaf_trans_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +  
  
  geom_point(data = leaf_trans_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_leaf_trans, labels = log_labels_leaf_trans, 
                     limits = c(-6.2, -0.5)) +
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = expression("Leaf-Specific Transpiration (g d"^-1*" cm"^-2*")")) +
  
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = -0.5, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = -0.5, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(leaf_trans_plot_species)


# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC CONDUCTANCE PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_lc <- c(1e-07, 1e-06, 1e-05, 1e-04, 2e-04)
log_breaks_lc <- log(desired_breaks_lc)
log_labels_lc <- format(desired_breaks_lc, scientific = TRUE)

lc_plot_species <- ggplot() +  
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = leaf_specific_conductance_log, 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = lc_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +  
  
  geom_point(data = lc_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_lc, labels = log_labels_lc,limits = c(-16.5, -7.8)) +
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = expression("LS Hydraulic Conductance (g d"^-1*" kPa"^-1*" cm"^-2*")")) +
  
  theme_bw(base_size = 20, base_family = "Helvetica") +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = -7.8, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = -7.8, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(lc_plot_species)

# ═════════════════════════════════════════════════════════════════════
# LEAF-SPECIFIC CONDUCTIVITY PLOT ----
# ═════════════════════════════════════════════════════════════════════


desired_breaks_lconductivity <- c(0.00001, 0.0001, 0.001, 0.005, 0.01, 0.02)
log_breaks_lconductivity <- log(desired_breaks_lconductivity)
log_labels_lconductivity <- format(desired_breaks_lconductivity)

lconductivity_plot_species <- ggplot() +  
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = leaf_specific_conductivity_log, 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = lconductivity_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +  
  
  geom_point(data = lconductivity_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_lconductivity, labels = log_labels_lconductivity, 
                     limits = c(-11.8, -3.8)) +
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = expression("Leaf-Specific Conductivity (g d"^-1*" cm"^-1*" kPa"^-1*")")) +

  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = -3.8, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = -3.8, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(lconductivity_plot_species)

# ═════════════════════════════════════════════════════════════════════
# RESPROUTER PLOTS ----
# ═════════════════════════════════════════════════════════════════════

# Load resprouter predictions ----
wt_ro_predictions <- readRDS(here::here("outputs", "models", "wt_ro_predictions.rds"))
wpc_ro_predictions <- readRDS(here::here("outputs", "models", "wpc_ro_predictions.rds"))
wpconductivity_ro_predictions <- readRDS(here::here("outputs", "models", "wpconductivity_ro_predictions.rds"))
leaf_trans_ro_predictions <- readRDS(here::here("outputs", "models", "leaf_trans_ro_predictions.rds"))
lc_ro_predictions <- readRDS(here::here("outputs", "models", "lc_ro_predictions.rds"))
lconductivity_ro_predictions <- readRDS(here::here("outputs", "models", "lconductivity_ro_predictions.rds"))

# ─────────────────────────────────────────────────────────────────────
# WHOLE-PLANT TRANSPIRATION (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(wt_ro_predictions)[names(wt_ro_predictions) == "x"] <- "drought_phase"
names(wt_ro_predictions)[names(wt_ro_predictions) == "group"] <- "resprouter"

# Order factors
wt_ro_predictions$resprouter <- factor(wt_ro_predictions$resprouter, levels = c("R+", "R-"))

# Define breaks (same as species)
desired_breaks_wt_ro <- c(5, 10, 50, 100, 300, 200, 500)
log_breaks_wt_ro <- log(desired_breaks_wt_ro)
log_labels_wt_ro <- format(sprintf("%.0f", exp(log_breaks_wt_ro)))

# WT (whole-plant transpiration)
label_data_wt_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(6.5, 6.5, 6.5),
  label = c("a", "a", "b"),
  size = c(8, 8, 8)
)

# Plot
wt_plot_ro <- ggplot(wt_ro_predictions, aes(x = drought_phase, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1) +
  geom_point(aes(color = "All Species"), size = 5, shape = 16) +  # Add color aesthetic for legend
  
  # Annotations
  geom_text(data = label_data_wt_ro,
            mapping = aes(x = drought_phase, y = y_position, label = label),
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  
  scale_y_continuous(breaks = log_breaks_wt_ro, labels = log_labels_wt_ro, limits = c(1.3, 6.5)) +
  scale_color_manual(values = c("All Species" = "black"), name = "") +  # Create legend entry
  
  labs(x = "Drought Phase", y = "", title = "") +
  
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank())

print(wt_plot_ro)

# ─────────────────────────────────────────────────────────────────────
# WHOLE-PLANT HYDRAULIC CONDUCTANCE (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(wpc_ro_predictions)[names(wpc_ro_predictions) == "x"] <- "drought_phase"
names(wpc_ro_predictions)[names(wpc_ro_predictions) == "group"] <- "resprouter"

# Order factors
wpc_ro_predictions$resprouter <- factor(wpc_ro_predictions$resprouter, levels = c("R+", "R-"))

# Define breaks (same as species)
desired_breaks_wpc_ro <- c(0.001, 0.01, 0.1, 0.3, 0.6)
log_breaks_wpc_ro <- log(desired_breaks_wpc_ro)
log_labels_wpc_ro <- format(desired_breaks_wpc_ro, scientific = FALSE, trim = TRUE)

# Annotation data
label_data_wpc_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(-0.3, -0.3, -0.3),
  label = c("a ns A", "a ns A", "b ns B"),
  size = c(8,8,8)
)

# Plot
wpc_plot_ro <- ggplot(wpc_ro_predictions, aes(x = drought_phase, y = predicted,
                                              color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1,
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  geom_text(data = label_data_wpc_ro,
            mapping = aes(x = drought_phase, y = y_position, label = label),
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_wpc_ro, labels = log_labels_wpc_ro, limits = c(-6.90, -0.3)) +
  scale_color_manual(values = c("R+" = "black", "R-" = "black"), labels = c("R+", "R-")) +
  scale_fill_manual(values = c("R+" = "white", "R-" = "white"), labels = c("R+", "R-")) +
  scale_shape_manual(values = c("R+" = 24, "R-" = 18), labels = c("R+", "R-")) +
  labs(x = "Drought Phase", y = "", title = "") +
  guides(shape = guide_legend(title = "Resprout"),
         color = guide_legend(title = "Resprout"),
         fill = guide_legend(title = "Resprout")) +
  theme_bw(base_size = 20, base_family = "Helvetica") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_blank())

print(wpc_plot_ro)

# ─────────────────────────────────────────────────────────────────────
# WHOLE-PLANT HYDRAULIC CONDUCTIVITY (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(wpconductivity_ro_predictions)[names(wpconductivity_ro_predictions) == "x"] <- "drought_phase"
names(wpconductivity_ro_predictions)[names(wpconductivity_ro_predictions) == "group"] <- "resprouter"

# Order factors
wpconductivity_ro_predictions$resprouter <- factor(wpconductivity_ro_predictions$resprouter,
                                                   levels = c("R+", "R-"))

# Define breaks (same as species)
desired_breaks_wpconductivity_ro <- c(0.05, 0.5, 5, 10, 25)
log_breaks_wpconductivity_ro <- log(desired_breaks_wpconductivity_ro)
log_labels_wpconductivity_ro <- format(desired_breaks_wpconductivity_ro, scientific = FALSE, trim = TRUE)

# WP Conductivity
label_data_wpconductivity_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(3.4, 3.4, 3.4),
  label = c("a ns A", "a ns A", "b ns B"),
  size = c(8,8,8)
)

# Plot
wpconductivity_plot_ro <- ggplot(wpconductivity_ro_predictions,
                                 aes(x = drought_phase, y = predicted,
                                     color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1,
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  # For wpconductivity_plot_ro (whole-plant conductivity)
  geom_text(data = label_data_wpconductivity_ro,
            mapping = aes(x = drought_phase, y = y_position, label = label),
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_wpconductivity_ro, labels = log_labels_wpconductivity_ro,
                     limits = c(-2.99, 3.4)) +
  scale_color_manual(values = c("R+" = "black", "R-" = "black"), labels = c("R+", "R-")) +
  scale_fill_manual(values = c("R+" = "white", "R-" = "white"), labels = c("R+", "R-")) +
  scale_shape_manual(values = c("R+" = 24, "R-" = 18), labels = c("R+", "R-")) +
  labs(x = "Drought Phase", y = "", title = "") +
  guides(shape = guide_legend(title = "Resprout"),
         color = guide_legend(title = "Resprout"),
         fill = guide_legend(title = "Resprout")) +
  theme_bw(base_size = 20, base_family = "Helvetica") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank())

print(wpconductivity_plot_ro)

# ─────────────────────────────────────────────────────────────────────
# LEAF-SPECIFIC TRANSPIRATION (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(leaf_trans_ro_predictions)[names(leaf_trans_ro_predictions) == "x"] <- "drought_phase"
names(leaf_trans_ro_predictions)[names(leaf_trans_ro_predictions) == "group"] <- "resprouter"

# Order factors
leaf_trans_ro_predictions$resprouter <- factor(leaf_trans_ro_predictions$resprouter,
                                               levels = c("R+", "R-"))

# Define breaks (same as species)
desired_breaks_leaf_trans_ro <- c(0.002, 0.01, 0.1,0.3, 0.5)
log_breaks_leaf_trans_ro <- log(desired_breaks_leaf_trans_ro)
log_labels_leaf_trans_ro <- format(desired_breaks_leaf_trans_ro, scientific = FALSE, trim = TRUE)


# Leaf transpiration
label_data_leaf_trans_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(-0.5, -0.5, -0.5),
  label = c("a ns A", "a ns B", "b ns C"),
  size = c(8,8,8)
)


# Plot
leaf_trans_plot_ro <- ggplot(leaf_trans_ro_predictions,
                             aes(x = drought_phase, y = predicted,
                                 color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1,
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  # For leaf_trans_plot_ro (leaf-specific transpiration)
  geom_text(data = label_data_leaf_trans_ro,
            mapping = aes(x = drought_phase, y = y_position, label = label),
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_leaf_trans_ro, labels = log_labels_leaf_trans_ro,
                     limits = c(-6.2, -0.5)) +
  scale_color_manual(values = c("R+" = "black", "R-" = "black"), labels = c("R+", "R-")) +
  scale_fill_manual(values = c("R+" = "white", "R-" = "white"), labels = c("R+", "R-")) +
  scale_shape_manual(values = c("R+" = 24, "R-" = 18), labels = c("R+", "R-")) +
  labs(x = "Drought Phase", y = "", title = "") +
  guides(shape = guide_legend(title = "Resprout"),
         color = guide_legend(title = "Resprout"),
         fill = guide_legend(title = "Resprout")) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank())

print(leaf_trans_plot_ro)

# ─────────────────────────────────────────────────────────────────────
# LEAF-SPECIFIC CONDUCTANCE (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(lc_ro_predictions)[names(lc_ro_predictions) == "x"] <- "drought_phase"
names(lc_ro_predictions)[names(lc_ro_predictions) == "group"] <- "resprouter"

# Order factors
lc_ro_predictions$resprouter <- factor(lc_ro_predictions$resprouter, levels = c("R+", "R-"))

# Define breaks (same as species)
desired_breaks_lc_ro <- c(1e-07, 1e-06, 1e-05, 1e-04, 2e-04)
log_breaks_lc_ro <- log(desired_breaks_lc_ro)
log_labels_lc_ro <- format(desired_breaks_lc_ro)

# Annotation data
label_data_lc_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(-7.8, -7.8, -7.8),
  label = c("a ns A", "a ns A", "b ns B"),
  size = c(8,8,8)
)

# Plot
lc_plot_ro <- ggplot(lc_ro_predictions, aes(x = drought_phase, y = predicted,
                                            color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1,
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  geom_text(data = label_data_lc_ro,
            mapping = aes(x = drought_phase, y = y_position, label = label),
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_lc_ro, labels = log_labels_lc_ro, limits = c(-16.5, -7.8)) +
  scale_color_manual(values = c("R+" = "black", "R-" = "black"), labels = c("R+", "R-")) +
  scale_fill_manual(values = c("R+" = "white", "R-" = "white"), labels = c("R+", "R-")) +
  scale_shape_manual(values = c("R+" = 24, "R-" = 18), labels = c("R+", "R-")) +
  labs(x = "Drought Phase", y = "", title = "") +
  guides(shape = guide_legend(title = "Resprout"),
         color = guide_legend(title = "Resprout"),
         fill = guide_legend(title = "Resprout")) +
  theme_bw(base_size = 20, base_family = "Helvetica") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_blank())

print(lc_plot_ro)

# ─────────────────────────────────────────────────────────────────────
# LEAF-SPECIFIC CONDUCTIVITY (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(lconductivity_ro_predictions)[names(lconductivity_ro_predictions) == "x"] <- "drought_phase"
names(lconductivity_ro_predictions)[names(lconductivity_ro_predictions) == "group"] <- "resprouter"

# Order factors
lconductivity_ro_predictions$resprouter <- factor(lconductivity_ro_predictions$resprouter,
                                                  levels = c("R+", "R-"))

# Define breaks (same as species)
desired_breaks_lconductivity_ro <- c(0.00001, 0.0001, 0.001, 0.005, 0.01, 0.02)
log_breaks_lconductivity_ro <- log(desired_breaks_lconductivity_ro)
log_labels_lconductivity_ro <- format(desired_breaks_lconductivity_ro)

# Leaf Conductivity
label_data_lconductivity_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(-3.8, -3.8, -3.8),
  label = c("a ns A", "a ns A", "b * B"),
  size = c(8,8,8)
)

# Plot
lconductivity_plot_ro <- ggplot(lconductivity_ro_predictions,
                                aes(x = drought_phase, y = predicted,
                                    color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1,
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  geom_text(data = label_data_lconductivity_ro,
            mapping = aes(x = drought_phase, y = y_position, label = label),
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_lconductivity_ro, labels = log_labels_lconductivity_ro,
                     limits = c(-11.8, -3.8)) +
  scale_color_manual(values = c("R+" = "black", "R-" = "black"), labels = c("R+", "R-")) +
  scale_fill_manual(values = c("R+" = "white", "R-" = "white"), labels = c("R+", "R-")) +
  scale_shape_manual(values = c("R+" = 24, "R-" = 18), labels = c("R+", "R-")) +
  labs(x = "Drought Phase", y = "", title = "") +
  guides(shape = guide_legend(title = "Resprout"),
         color = guide_legend(title = "Resprout"),
         fill = guide_legend(title = "Resprout")) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank())

print(lconductivity_plot_ro)

# ═════════════════════════════════════════════════════════════════════
# COMBINED PLOTS (Species + Resprouter) ----
# ═════════════════════════════════════════════════════════════════════

# Whole-plant transpiration combined
final_plot_wt <- (wt_plot_species + wt_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_wt)

# Whole-plant conductance combined
final_plot_wpc <- (wpc_plot_species + wpc_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_wpc)


# Whole-plant conductivity combined
final_plot_wpconductivity <- (wpconductivity_plot_species + wpconductivity_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_wpconductivity)


# Leaf-specific transpiration combined
final_plot_leaf_trans <- (leaf_trans_plot_species + leaf_trans_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_leaf_trans)


# Leaf-specific conductance combined
final_plot_lc <- (lc_plot_species + lc_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_lc)


# Leaf-specific conductivity combined
final_plot_lconductivity <- (lconductivity_plot_species + lconductivity_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_lconductivity)



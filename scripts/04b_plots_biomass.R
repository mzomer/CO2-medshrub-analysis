# ─────────────────────────────────────────────────────────────────────
# 04b_plots_biomass.R — Plots for biomass allocation
# ─────────────────────────────────────────────────────────────────────
# Plots for: Aboveground, belowground, R:S, leaf area
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data for raw data points ----
data_traits_biomass <- readRDS(here::here("data", "processed", "data_traits_biomass.rds"))
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))

# Load predictions from models ----
tab_predictions <- readRDS(here::here("outputs", "models", "tab_predictions.rds"))
tbb_predictions <- readRDS(here::here("outputs", "models", "tbb_predictions.rds"))
rs_predictions <- readRDS(here::here("outputs", "models", "rs_predictions.rds"))
lfarea_predictions <- readRDS(here::here("outputs", "models", "lfarea_predictions.rds"))

# Rename variables from ggpredict output ----
names(tab_predictions)[names(tab_predictions) == "x"] <- "species"
names(tab_predictions)[names(tab_predictions) == "group"] <- "co2"

names(tbb_predictions)[names(tbb_predictions) == "x"] <- "species"
names(tbb_predictions)[names(tbb_predictions) == "group"] <- "co2"

names(rs_predictions)[names(rs_predictions) == "x"] <- "species"
names(rs_predictions)[names(rs_predictions) == "group"] <- "co2"

# Plot settings ----
my_pal_co2 <- c("#EBCB8B", "#BF616A")
my_pal_species <- c(ACY = "#8C510A", DPE = "#DFC27D", LST = "#C7EAE5", 
                    CAL = "#35978F", SRO = "#003C30")
pd <- position_dodge(.5)

# ═════════════════════════════════════════════════════════════════════
# ABOVEGROUND BIOMASS PLOT ----
# ═════════════════════════════════════════════════════════════════════

# Define the desired breakpoints in the original scale
desired_breaks_tab <- c(10, 50, 100, 150)
log_breaks_tab <- log(desired_breaks_tab)
log_labels_tab <- format(desired_breaks_tab, scientific = FALSE, trim = TRUE)

# Define annotation data for stars
annotation_data_tab <- data.frame(
  species = c("ACY", "CAL", "SRO"),
  y = c(5, 5, 5),
  label = c("***", "*", "***"),
  size = c(6, 6, 6)
)

tab_plot_species <- ggplot(tab_predictions, aes(x = species, y = predicted, color = co2, shape = species)) +
  # Error bars for predicted tab
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, size = 1, alpha = 1, position = pd) +
  
  # Raw tab data points filled with CO2 color scheme
  geom_point(data = data_traits_biomass, 
             aes(x = species, y = total_aboveground_biomass_log, color = co2, shape = species, fill = co2),
             position = position_jitterdodge(dodge.width = 0.5), 
             size = 2.5, alpha = 0.5, stroke = 1) +
  
  # Predicted tab points with SOLID white fill for ACY & DPE (triangles)
  geom_point(data = tab_predictions, 
             aes(x = species, y = predicted, color = co2, shape = species), fill = "white",
             size = 5, alpha = 1, position = pd, stroke = 1.5) +
  
  geom_text(data = annotation_data_tab, aes(x = species, y = y, label = label), 
            size = 6, fontface = "bold", color = "black") +
  
  theme_bw(base_size = 20) +
  scale_y_continuous(breaks = log_breaks_tab, labels = log_labels_tab, limits = c(2.3, 5)) +
  
  # CO2 color mapping
  scale_color_manual(name = expression(CO[2]), labels = c("Ambient", "Elevated"), values = my_pal_co2) +
  
  # Shape mapping
  scale_shape_manual(name = NULL, 
                     values = c("ACY" = 24, "DPE" = 24, "CAL" = 18, "LST" = 18, "SRO" = 18),
                     guide = "none") +
  
  # Fill mapping for raw data
  scale_fill_manual(values = my_pal_co2) +
  guides(fill = "none") +
  
  labs(x = "Species", y = "Aboveground Biomass (g)", title = "") +
  
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

print(tab_plot_species)

# ═════════════════════════════════════════════════════════════════════
# BELOWGROUND BIOMASS PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_tbb <- c(5, 10, 20, 30, 50)
log_breaks_tbb <- log(desired_breaks_tbb)
log_labels_tbb <- format(desired_breaks_tbb, scientific = FALSE, trim = TRUE)

annotation_data_tbb <- data.frame(
  species = c("ACY", "SRO"),
  y = c(4, 4),
  label = c("**", "**"),
  size = c(6, 6)
)

tbb_plot_species <- ggplot(tbb_predictions, aes(x = species, y = predicted, color = co2, shape = species)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, size = 1, alpha = 1, position = pd) +
  
  geom_point(data = data_traits_biomass, 
             aes(x = species, y = total_belowground_biomass_log, color = co2, shape = species, fill = co2),
             position = position_jitterdodge(dodge.width = 0.5), 
             size = 2.5, alpha = 0.5, stroke = 1) +
  
  geom_point(data = tbb_predictions, 
             aes(x = species, y = predicted, color = co2, shape = species), fill = "white",
             size = 5, alpha = 1, position = pd, stroke = 1.5) +
  
  geom_text(data = annotation_data_tbb, aes(x = species, y = y, label = label), 
            size = 6, fontface = "bold", color = "black") +
  
  theme_bw(base_size = 20) +
  scale_y_continuous(breaks = log_breaks_tbb, labels = log_labels_tbb, limits = c(1.5, 4)) +
  
  scale_color_manual(name = expression(CO[2]), labels = c("Ambient", "Elevated"), values = my_pal_co2) +
  scale_shape_manual(name = NULL, 
                     values = c("ACY" = 24, "DPE" = 24, "CAL" = 18, "LST" = 18, "SRO" = 18),
                     guide = "none") +
  scale_fill_manual(values = my_pal_co2) +
  guides(fill = "none") +
  
  labs(x = "Species", y = "Belowground Biomass (g)", title = "") +
  
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

print(tbb_plot_species)

# ═════════════════════════════════════════════════════════════════════
# ROOT:SHOOT RATIO PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_rs <- c(0.1, 0.2, 0.3, 0.4, 0.5)
log_breaks_rs <- log(desired_breaks_rs)
log_labels_rs <- format(desired_breaks_rs, scientific = FALSE, trim = TRUE)

rs_plot_species <- ggplot(rs_predictions, aes(x = species, y = predicted, color = co2, shape = species)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, size = 1, alpha = 1, position = pd) +
  
  geom_point(data = data_traits_biomass, 
             aes(x = species, y = root_shoot_ratio_log, color = co2, shape = species, fill = co2),
             position = position_jitterdodge(dodge.width = 0.5), 
             size = 2.5, alpha = 0.5, stroke = 1) +
  
  geom_point(data = rs_predictions, 
             aes(x = species, y = predicted, color = co2, shape = species), fill = "white",
             size = 5, alpha = 1, position = pd, stroke = 1.5) +
  
  theme_bw(base_size = 20) +
  scale_y_continuous(breaks = log_breaks_rs, labels = log_labels_rs, limits = c(-2.3, -0.69)) +
  
  scale_color_manual(name = expression(CO[2]), labels = c("Ambient", "Elevated"), values = my_pal_co2) +
  scale_shape_manual(name = NULL, 
                     values = c("ACY" = 24, "DPE" = 24, "CAL" = 18, "LST" = 18, "SRO" = 18),
                     guide = "none") +
  scale_fill_manual(values = my_pal_co2) +
  guides(fill = "none") +
  
  labs(x = "Species", y = "R:S", title = "") +
  
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank())

print(rs_plot_species)


# ═════════════════════════════════════════════════════════════════════
# LEAF BIOMASS CHANGE PLOT ----
# ═════════════════════════════════════════════════════════════════════

# Prepare data - convert to long format
data_biomass_change <- data_traits_biomass %>%
  select(species, co2, greenhouse, plant, 
         percentage_biomass_change_1_2, percentage_biomass_change_2_3) %>%
  pivot_longer(cols = c(percentage_biomass_change_1_2, percentage_biomass_change_2_3),
               names_to = "phase_comparison", 
               values_to = "biomass_change") %>%
  mutate(phase_comparison = recode(phase_comparison,
                                   "percentage_biomass_change_1_2" = "Phase 1 -> 2",
                                   "percentage_biomass_change_2_3" = "Phase 2 -> 3")) %>%
  filter(!is.na(biomass_change))

# Calculate summary statistics for plotting
summary_biomass_change <- data_biomass_change %>%
  group_by(species, co2, phase_comparison) %>%
  summarise(
    mean_change = mean(biomass_change, na.rm = TRUE),
    se = sd(biomass_change, na.rm = TRUE) / sqrt(n()),
    conf.low = mean_change - 1.96 * se,
    conf.high = mean_change + 1.96 * se,
    .groups = "drop"
  )

# Dodge settings
pd <- position_dodge(width = 0.5)

# Determine y-axis limits for label placement
ylims_biomass <- c(-100, 500)

# Create the plot
biomass_change_plot <- ggplot() +
  
  # Error bars for mean biomass change
  geom_errorbar(data = summary_biomass_change, 
                aes(x = species, y = mean_change, 
                    ymin = conf.low, ymax = conf.high, color = co2), 
                width = 0.3, size = 1, position = pd) +
  
  # Raw data points
  geom_point(data = data_biomass_change, aes(x = species, y = biomass_change, 
                 color = co2, shape = species, fill = co2), position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2), 
                 size = 2.5, alpha = 0.5, stroke = 1) +
  
  # Mean points (white-filled for resprouters)
  geom_point(data = summary_biomass_change, aes(x = species, y = mean_change, color = co2, shape = species), 
             fill = "white", size = 5, position = pd, stroke = 1.5) +
  
  # Add horizontal line at 0 (no change)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Facet by phase comparison
  facet_wrap(~ phase_comparison, ncol = 2) +
  
  # Scales
  scale_y_continuous(breaks = seq(-100, 600, 100), limits = ylims_biomass) +
  scale_color_manual(name = expression(CO[2]), labels = c("Ambient", "Elevated"), values = my_pal_co2) +
  scale_shape_manual(name = NULL, values = c("ACY" = 24, "DPE" = 24, 
                                "CAL" = 18, "LST" = 18, "SRO" = 18),guide = "none") +
  scale_fill_manual(values = my_pal_co2) +
  guides(fill = "none") +
  
  # Labels
  labs(x = "Species", 
       y = "Leaf Biomass Change (%)", 
       title = "") +
  
  # Theme
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),  # Remove default facet labels
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  
  # Add custom facet labels inside each panel (like your other plots)
  geom_text(data = data.frame(phase_comparison = "Phase 1 -> 2"), 
            aes(x = 5.45, y = ylims_biomass[2], label = "Phase 1 -> 2"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  geom_text(data = data.frame(phase_comparison = "Phase 2 -> 3"), 
            aes(x = 5.45, y = ylims_biomass[2], label = "Phase 2 -> 3"), 
            fontface = "bold.italic", hjust = 1, size = 6)

print(biomass_change_plot)

# Save plot
ggsave(here::here("outputs", "figures", "FigureS7_leafbiomass_change.pdf"), 
       biomass_change_plot, width = 12, height = 8)


# ═════════════════════════════════════════════════════════════════════
# LEAF AREA PLOT ----
# ═════════════════════════════════════════════════════════════════════

desired_breaks_area <- c(500, 1000, 2000, 5000, 10000, 25000)
log_breaks_area <- log(desired_breaks_area)
log_labels_area <- format(desired_breaks_area, scientific = FALSE, trim = TRUE)
ylims_area <- c(min(log_breaks_area), max(log_breaks_area) + 0.15)

area_plot_species <- ggplot() +
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = whole_plant_leaf_area_log, color = species, shape = species, fill = species),
             alpha = 0.15, size = 3, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +
  
  geom_errorbar(data = lfarea_predictions, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.7)) +
  
  geom_point(data = lfarea_predictions, 
             aes(x = drought_phase, y = predicted, color = species, shape = species),
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.7)) +
  
  scale_y_continuous(breaks = log_breaks_area, labels = log_labels_area, limits = ylims_area) +
  scale_color_manual(values = my_pal_species, name = "Species") +
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), name = "Species") +
  
  guides(shape = guide_legend(title = "Species"), 
         color = guide_legend(title = "Species"), 
         fill = guide_legend(title = "Species")) +
  
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = "WP Leaf Area (cm²)", title = "") +
  
  theme_bw(base_size = 24) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = ylims_area[2], label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 8) +
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = ylims_area[2], label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 8)

print(area_plot_species)


ggsave(here::here("outputs", "figures", "FigureS8_leafarea.pdf"), area_plot_species, 
       width = 13, height = 10)
ggsave(here::here("outputs", "figures", "FigureS8_leafarea.png"), area_plot_species, 
       width = 13, height = 10, dpi = 300)


# ═════════════════════════════════════════════════════════════════════
# RESPROUTER PLOTS ----
# ═════════════════════════════════════════════════════════════════════

# Load resprouter predictions ----
rs_ro_predictions <- readRDS(here::here("outputs", "models", "rs_ro_predictions.rds"))

# ─────────────────────────────────────────────────────────────────────
# ROOT:SHOOT RATIO (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(rs_ro_predictions)[names(rs_ro_predictions) == "x"] <- "resprouter"
names(rs_ro_predictions)[names(rs_ro_predictions) == "group"] <- "co2"

# Order factors
rs_ro_predictions$resprouter <- factor(rs_ro_predictions$resprouter, levels = c("R+", "R-"))
rs_ro_predictions$co2 <- factor(rs_ro_predictions$co2, levels = c("ambient", "elevated"),
                                labels = c("Ambient", "Elevated"))

# Define breaks for R:S (same as species)
desired_breaks_rs_ro <- c(0.1, 0.2, 0.3, 0.4, 0.5)
log_breaks_rs_ro <- log(desired_breaks_rs_ro)
log_labels_rs_ro <- format(desired_breaks_rs_ro, scientific = FALSE, trim = TRUE)

# Annotation data
annotation_data_rs_ro <- data.frame(
  resprouter = c("R+", "R-"),
  y = c(-0.7, -0.69),
  label = c("*", ""),
  size = c(8, 6)
)

# Plot
rs_plot_ro <- ggplot(rs_ro_predictions, aes(x = resprouter, y = predicted,
                                            color = co2, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1,
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  geom_text(data = annotation_data_rs_ro, aes(x = resprouter, y = y, label = label),
            fontface = "bold", color = "black", size = annotation_data_rs_ro$size, inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_rs_ro, labels = log_labels_rs_ro, limits = c(-2.3, -0.69)) +
  scale_color_manual(values = my_pal_co2) +
  scale_shape_manual(values = c("R+" = 24, "R-" = 18)) +
  scale_fill_manual(values = c("R+" = "white", "R-" = my_pal_co2[1])) +
  guides(shape = guide_legend(title = "Resprout"), fill = "none", color = "none") +
  labs(x = "Resprout", y = "R:S") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank())

print(rs_plot_ro)

# ═════════════════════════════════════════════════════════════════════
# COMBINED PLOTS (Species + Resprouter) ----
# ═════════════════════════════════════════════════════════════════════

# R:S combined
final_plot_rs <- (rs_plot_species + rs_plot_ro) +
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_rs)



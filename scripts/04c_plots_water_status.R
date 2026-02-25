# ─────────────────────────────────────────────────────────────────────
# 04c_plots_water_status.R — Plots for water status
# ─────────────────────────────────────────────────────────────────────
# Plots for: Midday water potential, Osmotic potential
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Load cleaned data for raw data points ----
data_water_relations <- readRDS(here::here("data", "processed", "data_water_relations.rds"))

# Load predictions from models ----
ymd_predictions <- readRDS(here::here("outputs", "models", "ymd_predictions.rds"))
osm_predictions <- readRDS(here::here("outputs", "models", "osm_predictions.rds"))

str(ymd_predictions)
# Plot settings ----
my_pal_species <- c(ACY = "#8C510A", DPE = "#DFC27D", LST = "#C7EAE5", 
                    CAL = "#35978F", SRO = "#003C30")

# ═════════════════════════════════════════════════════════════════════
# MIDDAY WATER POTENTIAL PLOT ----
# ═════════════════════════════════════════════════════════════════════

# Back-transform the log values to the original scale
back_pred_ymd <- ymd_predictions %>%
  mutate(predicted = -exp(predicted),
         conf.low = -exp(conf.low),
         conf.high = -exp(conf.high))

# Determine the range for breaks
min_raw_data <- min(-exp(data_water_relations$water_potential_midday_log), na.rm = TRUE)
max_prediction <- max(-exp(ymd_predictions$predicted), na.rm = TRUE)

log_breaks <- seq(log(-max_prediction), log(-min_raw_data), length.out = 5)
log_breaks <- -exp(log_breaks)
log_labels <- format(sprintf("%.0f", log_breaks))

ymd_plot_species <- ggplot() +  
  # raw data points
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = -exp(water_potential_midday_log), 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.85, jitter.width = 0.2)) +
  
  # species model predictions
  geom_errorbar(data = back_pred_ymd, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.85)) +  
  
  # mean predicted point
  geom_point(data = back_pred_ymd, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.85)) +
  
  # y axis log scale 
  scale_y_continuous(breaks = log_breaks, labels = log_labels) +  
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  # facet and labels
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = "Midday Water Potential (MPa)", title = "") +
  
  # theme
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  # "ambient" inside left facet
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = -0.65, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  # "elevated" inside right facet
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = -0.65, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

# Add green stars for CAL in elevated panel
cal_errorbar_data <- as_tibble(back_pred_ymd) %>%
  filter(species == "CAL", co2 == "elevated") %>%
  mutate(
    x_adjusted = as.numeric(drought_phase) + 0.17,
    y_position = case_when(
      drought_phase == 1 ~ predicted + 0.35,
      drought_phase == 2 ~ predicted + 0.35,
      drought_phase == 3 ~ predicted + 0.7,
      TRUE ~ predicted
    )
  )

ymd_plot_species <- ymd_plot_species + 
  geom_point(data = cal_errorbar_data, 
             aes(x = x_adjusted, y = y_position), 
             shape = 8, color = "#35978F", size = 3, stroke = 1)

print(ymd_plot_species)

# ═════════════════════════════════════════════════════════════════════
# OSMOTIC POTENTIAL PLOT ----
# ═════════════════════════════════════════════════════════════════════

# Back-transform the log values to the original scale
back_pred_osm <- osm_predictions %>%
  mutate(predicted = -exp(predicted),
         conf.low = -exp(conf.low),
         conf.high = -exp(conf.high))

# Determine the range for breaks
min_raw_data_osm <- min(-exp(data_water_relations$osmotic_potential_log), na.rm = TRUE)
max_prediction_osm <- max(-exp(osm_predictions$predicted), na.rm = TRUE)

log_breaks_osm <- seq(log(-max_prediction_osm), log(-min_raw_data_osm), length.out = 4)
log_breaks_osm <- -exp(log_breaks_osm)
log_labels_osm <- format(sprintf("%.0f", log_breaks_osm))

osm_plot_species <- ggplot() +  
  # raw data points
  geom_point(data = data_water_relations, 
             aes(x = drought_phase, y = -exp(osmotic_potential_log), 
                 color = species, shape = species, fill = species), 
             alpha = 0.15, size = 3, 
             position = position_jitterdodge(dodge.width = 0.85, jitter.width = 0.2)) +
  
  # species model predictions
  geom_errorbar(data = back_pred_osm, 
                aes(x = drought_phase, y = predicted, ymin = conf.low, ymax = conf.high, 
                    group = species, color = species),
                width = 0.5, size = 1, position = position_dodge(width = 0.85)) +  
  
  # mean predicted point
  geom_point(data = back_pred_osm, 
             aes(x = drought_phase, y = predicted, color = species, shape = species), 
             fill = "white", size = 5, stroke = 1, position = position_dodge(width = 0.85)) +
  
  # y axis log scale 
  scale_y_continuous(breaks = log_breaks_osm, labels = log_labels_osm) +  
  
  scale_color_manual(values = my_pal_species, name = "Species") +  
  scale_fill_manual(values = my_pal_species) +
  scale_shape_manual(values = c(ACY = 24, DPE = 24, LST = 18, CAL = 18, SRO = 18), 
                     name = "Species") +
  guides(shape = guide_legend(title = "Species"),
         color = guide_legend(title = "Species"),
         fill = guide_legend(title = "Species")) +
  
  # facet and labels
  facet_grid(~ co2) +
  labs(x = "Drought Phase", y = "Osmotic Potential (MPa)", title = "") +
  
  # theme
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),  
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  
  # "ambient" inside left facet
  geom_text(data = data.frame(co2 = "ambient"), 
            aes(x = 3.45, y = -0.65, label = "ambient"), 
            fontface = "bold.italic", hjust = 1, size = 6) +
  # "elevated" inside right facet
  geom_text(data = data.frame(co2 = "elevated"), 
            aes(x = 3.45, y = -0.65, label = "elevated"), 
            fontface = "bold.italic", hjust = 1, size = 6)

# Add stars for significant species
cal_errorbar_data_osm <- as_tibble(back_pred_osm) %>%
  filter(species == "CAL", co2 == "elevated") %>%
  mutate(
    x_adjusted = as.numeric(drought_phase) + 0.17,
    y_position = case_when(
      drought_phase == 1 ~ predicted + 0.3,
      drought_phase == 2 ~ predicted + 0.3,
      drought_phase == 3 ~ predicted + 0.4,
      TRUE ~ predicted
    )
  )

dpe_errorbar_data_osm <- as_tibble(back_pred_osm) %>%
  filter(species == "DPE", co2 == "elevated", drought_phase == "3") %>%
  mutate(
    x_adjusted = as.numeric(drought_phase) - 0.17,
    y_position = predicted + 0.6
  )

osm_plot_species <- osm_plot_species + 
  geom_point(data = cal_errorbar_data_osm, 
             aes(x = x_adjusted, y = y_position), 
             shape = 8, color = "#35978F", size = 3, stroke = 1) +
  geom_point(data = dpe_errorbar_data_osm, 
             aes(x = x_adjusted, y = y_position), 
             shape = 8, color = "#DFC27D", size = 3, stroke = 1)

print(osm_plot_species)


# ═════════════════════════════════════════════════════════════════════
# RESPROUTER PLOTS ----
# ═════════════════════════════════════════════════════════════════════

# Load resprouter predictions ----
ymd_ro_predictions <- readRDS(here::here("outputs", "models", "ymd_ro_predictions.rds"))
osm_ro_predictions <- readRDS(here::here("outputs", "models", "osm_ro_predictions.rds"))

# ─────────────────────────────────────────────────────────────────────
# MIDDAY WATER POTENTIAL (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(ymd_ro_predictions)[names(ymd_ro_predictions) == "x"] <- "drought_phase"
names(ymd_ro_predictions)[names(ymd_ro_predictions) == "group"] <- "resprouter"

# Order factors
ymd_ro_predictions$resprouter <- factor(ymd_ro_predictions$resprouter, levels = c("R+", "R-"))

# Back-transform predictions
ymd_ro_predictions <- ymd_ro_predictions %>%
  mutate(predicted = -exp(predicted),
         conf.low = -exp(conf.low),
         conf.high = -exp(conf.high))

# Define breaks (matching species plot)
min_raw_data_ymd <- min(-exp(data_water_relations$water_potential_midday_log), na.rm = TRUE)
max_prediction_ymd <- max(ymd_ro_predictions$predicted, na.rm = TRUE)

log_breaks_ymd_ro <- seq(log(-max_prediction_ymd), log(-min_raw_data_ymd), length.out = 5)
log_breaks_ymd_ro <- -exp(log_breaks_ymd_ro)
log_labels_ymd_ro <- format(sprintf("%.0f", log_breaks_ymd_ro))

# Annotation data
label_data_ymd_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(-0.65, -0.65, -0.65),
  label = c("a * A", "a * A", "b ns B"),
  size = c(8, 8, 8)
)

# Plot
ymd_plot_ro <- ggplot(ymd_ro_predictions, aes(x = drought_phase, y = predicted, 
                                              color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1, 
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  geom_text(data = label_data_ymd_ro, 
            mapping = aes(x = drought_phase, y = y_position, label = label), 
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks, labels = log_labels, limits = c(-7.5, -0.65)) +
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

print(ymd_plot_ro)

# ─────────────────────────────────────────────────────────────────────
# OSMOTIC POTENTIAL (resprouter) ----
# ─────────────────────────────────────────────────────────────────────

# Rename columns
names(osm_ro_predictions)[names(osm_ro_predictions) == "x"] <- "drought_phase"
names(osm_ro_predictions)[names(osm_ro_predictions) == "group"] <- "resprouter"

# Order factors
osm_ro_predictions$resprouter <- factor(osm_ro_predictions$resprouter, levels = c("R+", "R-"))

# Back-transform predictions
osm_ro_predictions <- osm_ro_predictions %>%
  mutate(predicted = -exp(predicted),
         conf.low = -exp(conf.low),
         conf.high = -exp(conf.high))

# Define breaks (matching species plot)
min_raw_data_osm <- min(-exp(data_water_relations$osmotic_potential_log), na.rm = TRUE)
max_prediction_osm <- max(osm_ro_predictions$predicted, na.rm = TRUE)

log_breaks_osm_ro <- seq(log(-max_prediction_osm), log(-min_raw_data_osm), length.out = 4)
log_breaks_osm_ro <- -exp(log_breaks_osm_ro)
log_labels_osm_ro <- format(sprintf("%.0f", log_breaks_osm_ro))

# Annotation data
label_data_osm_ro <- data.frame(
  drought_phase = c(1, 2, 3),
  y_position = c(-0.65, -0.65, -0.65),
  label = c("a * A", "b * B", "c ns C"),
  size = c(8, 8, 8)
)

# Plot
osm_plot_ro <- ggplot(osm_ro_predictions, aes(x = drought_phase, y = predicted, 
                                              color = resprouter, shape = resprouter)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.2, size = 1, 
                position = position_dodge(0.5)) +
  geom_point(aes(fill = resprouter), size = 5, stroke = 1.2, position = position_dodge(0.5)) +
  geom_text(data = label_data_osm_ro, 
            mapping = aes(x = drought_phase, y = y_position, label = label), 
            fontface = "bold", size = 4, color = "black", inherit.aes = FALSE) +
  scale_y_continuous(breaks = log_breaks_osm, labels = log_labels_osm, limits = c(-6, -0.65)) +
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

print(osm_plot_ro)

# ═════════════════════════════════════════════════════════════════════
# COMBINED PLOTS (Species + Resprouter) ----
# ═════════════════════════════════════════════════════════════════════

# YMD combined
final_plot_ymd <- (ymd_plot_species + ymd_plot_ro) + 
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_ymd)


# OSM combined
final_plot_osm <- (osm_plot_species + osm_plot_ro) + 
  plot_layout(widths = c(2.5, 1)) &
  theme(legend.position = "bottom", legend.box = "horizontal")

print(final_plot_osm)





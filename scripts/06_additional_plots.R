# ═════════════════════════════════════════════════════════════════════
#  06_additional_plots.R - SUPPLEMENTARY FIGURES 
# Greenhouse climate, SWC, Root size, Soil water retention curve
# ═════════════════════════════════════════════════════════════════════

# Load Setup ----
source(here::here("scripts", "setup.R"))

# ═════════════════════════════════════════════════════════════════════
# GLOBAL SETTINGS ----
# ═════════════════════════════════════════════════════════════════════

# Color palette - used across all plots
my_pal_co2 <- c("#EBCB8B", "#BF616A")  # Ambient, Elevated

# Position dodge for boxplots
pd <- position_dodge(0.8)

# ═════════════════════════════════════════════════════════════════════
# FIGURE S3: GREENHOUSE CLIMATE ----
# ═════════════════════════════════════════════════════════════════════

# Load data
data_gh_climate <- read.csv(here::here("data", "raw", "data_greenhouse_climate.csv"))

# Data preparation
data_gh_climate$treatment <- factor(data_gh_climate$treatment, levels = c("Ambient", "Elevated"))
data_gh_climate$month <- factor(data_gh_climate$month, levels = c("Aug-Sept", "Nov-Dec"))
data_gh_climate$co2 <- as.numeric(data_gh_climate$co2)
data_gh_climate$light <- as.numeric(data_gh_climate$light)

data_sum <- data_gh_climate %>%
  group_by(treatment, month) %>%
  dplyr::summarise( 
    co2_mean = mean(co2),
    co2_sd = sd(co2),
    temp_mean = mean(temp),
    temp_sd = sd(temp),
    light_mean = mean(light),
    light_sd = sd(light),
    humidity_mean = mean(humidity),
    humidity_sd = sd(humidity),
    .groups = "drop")


print(data_sum)

# Overall mean CO2 by treatment (across both months)
overall_co2 <- data_gh_climate %>%
  group_by(treatment) %>%
  summarise(
    co2_mean = mean(co2, na.rm = TRUE),
    co2_sd = sd(co2, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(overall_co2)

# Create plots
# CO2 plot 
co2_plot <- ggplot(data_gh_climate, aes(x = month, y = co2, fill = treatment)) +
  geom_boxplot(position = pd) + 
  scale_fill_manual(name = expression(CO[2]), values = my_pal_co2) +
  labs(x = "", y = expression(CO[2]~"(ppm)")) + 
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Temperature plot
temp_plot <- ggplot(data_gh_climate, aes(x = month, y = temp, fill = treatment)) +
  geom_boxplot(position = pd) + 
  scale_fill_manual(name = expression(CO[2]), values = my_pal_co2) +
  labs(x = "", y = "Temperature (°C)") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Light plot 
light_plot <- ggplot(data_gh_climate, aes(x = month, y = light, fill = treatment)) +
  geom_boxplot(position = pd) + 
  scale_fill_manual(name = expression(CO[2]), values = my_pal_co2) +
  labs(x = "", y = "Relative light intensity") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Humidity plot 
humidity_plot <- ggplot(data_gh_climate, aes(x = month, y = humidity, fill = treatment)) +
  geom_boxplot(position = pd) + 
  scale_fill_manual(name = expression(CO[2]), values = my_pal_co2) +
  labs(x = "", y = "Relative humidity %") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Combined climate plot
climate_combined <- (co2_plot + temp_plot) / (light_plot + humidity_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

print(climate_combined)

# ═════════════════════════════════════════════════════════════════════
# FIGURE S4: SOIL WATER CONTENT ----
# ═════════════════════════════════════════════════════════════════════

# Load data
data_swc <- read.csv(here::here("data", "raw", "swc_estimates.csv"))

# Data preparation
data_swc <- data_swc %>% mutate(species = recode(species, "ROF" = "SRO"))
data_swc$drought_phase <- factor(data_swc$drought_phase, levels = c("1", "2", "3"))
data_swc$co2 <- factor(data_swc$co2, levels = c("ambient", "elevated"))
data_swc$species <- factor(data_swc$species, levels = c("ACY", "DPE", "LST", "CAL", "SRO"))
data_swc$greenhouse <- factor(data_swc$greenhouse)

# Calculate mean SWC per plant per phase
swc_summary <- data_swc %>%
  group_by(plant_no, species, greenhouse, drought_phase, co2) %>%
  dplyr::summarise(swc_mean = mean(result_swc_standardized, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(swc_mean))

# Create plot
swc_plot <- ggplot(swc_summary, aes(x = drought_phase, y = swc_mean, fill = co2)) +
  geom_boxplot(position = pd) + 
  scale_fill_manual(name = expression(CO[2]), 
                    labels = c("Ambient", "Elevated"), 
                    values = my_pal_co2) +
  labs(x = "Drought Phase", y = "Gravimetric soil water content %") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

print(swc_plot)

# Statistical models
swc_1 <- lm(log(swc_mean) ~ co2 * species, data = swc_summary[swc_summary$drought_phase == "1",])
swc_2 <- lm(log(swc_mean) ~ co2 * species, data = swc_summary[swc_summary$drought_phase == "2",])
swc_3 <- lm(log(swc_mean) ~ co2 * species, data = swc_summary[swc_summary$drought_phase == "3",])


print(anova(swc_1))
print(anova(swc_2))
print(anova(swc_3))

# Model diagnostics
plot(simulateResiduals(swc_1), main = "SWC Phase 1")
plot(simulateResiduals(swc_2), main = "SWC Phase 2")
plot(simulateResiduals(swc_3), main = "SWC Phase 3")

# ═════════════════════════════════════════════════════════════════════
# FIGURE S5: SOIL WATER RETENTION CURVE ----
# ═════════════════════════════════════════════════════════════════════

# Load data
swrc_data <- read_csv(here::here("data", "raw", "soil_water_retention_curve.csv"))

# Data preparation
swrc_data$Replicate <- factor(swrc_data$Replicate)

# Reshape to long format
swrc_long <- swrc_data %>%
  pivot_longer(cols = c(humidity_volumetric_measured, humidity_volumetric_predicted),
               names_to = "Type",
               values_to = "Volumetric_Water_Content") %>%
  mutate(Type = recode(Type,
                       "humidity_volumetric_measured" = "Measured",
                       "humidity_volumetric_predicted" = "Predicted")) %>%
  filter(!is.na(Replicate), !is.na(Volumetric_Water_Content))

# Color palette for replicates
replicate_colors <- c("#88C0D0", "#81A1C1", "#5E81AC")

# Create plot
swrc_plot <- ggplot(swrc_long, 
                    aes(x = Volumetric_Water_Content, 
                        y = kPa, 
                        color = Replicate, 
                        shape = Type)) +
  geom_point(size = 4, stroke = 1.5,
             aes(fill = ifelse(Type == "Measured", "white", as.character(Replicate)))) +
  scale_color_manual(name = "Replicate",
                     values = replicate_colors) +
  scale_fill_manual(values = c("white" = "white", 
                               "1" = replicate_colors[1],
                               "2" = replicate_colors[2], 
                               "3" = replicate_colors[3]),
                    guide = "none") +
  scale_shape_manual(name = "",
                     values = c("Measured" = 21, "Predicted" = 17)) +
  labs(x = expression("Volumetric water content cm"^3*"/cm"^3),
       y = "Matric potential kPa") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.minor = element_blank())

print(swrc_plot)

# Create summary table
swrc_summary <- swrc_data %>%
  group_by(kPa) %>%
  summarise(
    Measured_Mean = mean(humidity_volumetric_measured, na.rm = TRUE),
    Predicted_Mean = mean(humidity_volumetric_predicted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Measured_Mean = round(Measured_Mean, 4),
    Predicted_Mean = round(Predicted_Mean, 4)
  )

print(swrc_summary)

# ═════════════════════════════════════════════════════════════════════
# FIGURE S6: ROOT MEASUREMENTS ----
# ═════════════════════════════════════════════════════════════════════

# Load data
swc_biomass <- read_csv(here::here("data", "raw", "swc_biomass.csv"))

# Data preparation
swc_biomass <- swc_biomass %>% mutate(species = recode(species, "ROF" = "SRO"))
swc_biomass$co2 <- factor(swc_biomass$co2, levels = c("ambient", "elevated"))
swc_biomass$species <- factor(swc_biomass$species, levels = c("ACY", "DPE", "LST", "CAL", "SRO"))

# Create plots
# Root length plot
rl_plot <- ggplot(swc_biomass, 
                  aes(x = species, y = length_main_root_cm, fill = co2)) +
  geom_boxplot(position = pd, outlier.shape = NA) +
  scale_fill_manual(name = expression(CO[2]), 
                    labels = c("Ambient", "Elevated"), 
                    values = my_pal_co2) +
  labs(x = "", y = "Main root length (cm)") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

print(rl_plot)

# Root diameter plot
rd_plot <- ggplot(swc_biomass, 
                  aes(x = species, y = diameter_top_mainroot_mm, fill = co2)) +
  geom_boxplot(position = pd, outlier.shape = NA) +
  scale_fill_manual(name = expression(CO[2]), 
                    labels = c("Ambient", "Elevated"), 
                    values = my_pal_co2) +
  labs(x = "", y = "Main root diameter (mm)") +
  theme_bw(base_size = 20) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

print(rd_plot)

# Combined root measurements
root_combined <- rl_plot / rd_plot + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(root_combined)

# ═════════════════════════════════════════════════════════════════════
# SAVE ALL FIGURES ----
# ═════════════════════════════════════════════════════════════════════

# Figure S3: Greenhouse Climate
ggsave(here::here("outputs", "figures", "FigureS3_greenhouse_climate.pdf"), 
       climate_combined, width = 12, height = 10)

# Figure S4: Soil Water Content
ggsave(here::here("outputs", "figures", "FigureS4_soil_water_content.pdf"), 
       swc_plot, width = 15, height = 15, units = "cm")

# Figure S5: Soil Water Retention Curve
ggsave(here::here("outputs", "figures", "FigureS5_soil_water_retention_curve.pdf"), 
       swrc_plot, width = 10, height = 8.5, units = "cm")

# Figure S6: Root Measurements
ggsave(here::here("outputs", "figures", "FigureS6_root_measurements.pdf"), 
       root_combined, width = 10, height = 15, units = "cm")

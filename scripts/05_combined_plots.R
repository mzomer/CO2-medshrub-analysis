# ─────────────────────────────────────────────────────────────────────
# 05_combined_plots.R — Combined species + resprouter plots
# ─────────────────────────────────────────────────────────────────────
# Creates final publication-ready combined plots using patchwork
# ─────────────────────────────────────────────────────────────────────

# Load Setup ----
source(here::here("scripts", "setup.R"))

# Source individual plot scripts ----
# These scripts generate all the individual plots needed for the figures
source("scripts/04a_plots_traits.R")
source("scripts/04b_plots_biomass.R") #1 NA biomass, 15 NA leaf area 
source("scripts/04c_plots_water_status.R") #59 NA osmotic potential 
source("scripts/04d_plots_water_use.R")

## Figure 2: Plant traits and resource allocation ----
# Components:
# - SLA (specific leaf area)
# - Stomatal density
# - Aboveground biomass
# - Belowground biomass
# - Root-to-shoot ratio (species + resprouter panels combined)


fig2 <-  (sla_plot + stomata_plot) /
  (tab_plot_species + tbb_plot_species) /  # First row
  (rs_plot_species + rs_plot_ro)   +      # Second row
  # Third row
  plot_layout(guides = "collect", heights = c(1, 1, 1)) &  # Ensure equal height rows
  theme(legend.position = "bottom")  # Keep legend at the top

# Ensure equal widths for all columns
fig2 <- fig2 + plot_layout(widths = c(1, 1))

# Print the final layout
fig2


# Save Figure 2
ggsave("outputs/figures/Figure2_traits_biomass.pdf", fig2, 
       width = 10, height = 15, device = "pdf")
# Save Figure 2
ggsave("outputs/figures/Figure2_traits_biomass.png", fig2, 
       width = 10, height = 15, dpi=300)

## Figure 3: Water status and hydraulic behavior ----
# Components:
# - Leaf midday water potential (species + resprouter panels combined)
# - Whole-plant hydraulic conductance (species + resprouter panels combined)
# - Leaf-specific hydraulic conductance (species + resprouter panels combined)

fig3 <- final_plot_ymd / 
  (final_plot_wpc & guides(color = "none", fill = "none", shape = "none")) /
  (final_plot_lc & guides(color = "none", fill = "none", shape = "none")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Print to view
print(fig3)

# Save Figure 3
ggsave("outputs/figures/Figure3_water_hydraulics.pdf", fig3, 
       width = 12, height = 18, device = "pdf")

ggsave("outputs/figures/Figure3_water_hydraulics.png", fig3, 
       width = 12, height = 18, dpi = 300)

ggsave("outputs/figures/Figure3_water_hydraulics.eps", fig3, 
       width = 12, height = 18, device = cairo_ps)

## Supplementary Figure: Additional water satus and use variables ----
# Components:
# - Osmotic potential (species + resprouter panels combined)
# - Whole-plant transpiration (species + resprouter panels combined)
# - WP hydraulic conductivity (species + resprouter panels combined)
# - Leaf-specific transpiration (species + resprouter panels combined)
# - Leaf-specific hydraulic conductivity (species + resprouter panels combined)

# Supplementary Figure S9: Osmotic potential
fig_s9_osm <- final_plot_osm
print(fig_s9_osm)

# Supplementary Figure S9: Transpiration variables
fig_s9_transpiration <- (final_plot_wt / final_plot_leaf_trans) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
print(fig_s9_transpiration)


# Supplementary Figure S9: Conductivity variables  
fig_s9_conductivity <- (final_plot_wpconductivity / 
                          (final_plot_lconductivity & guides(color = "none", fill = "none", shape = "none"))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
print(fig_s9_conductivity)



#save plots 
ggsave("outputs/figures/FigureS9_osmotic_potential.pdf", fig_s9_osm, 
       width = 12, height = 8.5)
ggsave("outputs/figures/FigureS9_transpiration.pdf", fig_s9_transpiration, 
       width = 12.5, height = 15)
ggsave("outputs/figures/FigureS9_conductivity.pdf", fig_s9_conductivity, 
       width = 12, height = 15)


ggsave("outputs/figures/FigureS9_osmotic_potential.png", fig_s9_osm, 
       width = 12, height = 8.5, dpi=300)
ggsave("outputs/figures/FigureS9_transpiration.png", fig_s9_transpiration, 
       width = 12.5, height = 15,dpi=300)
ggsave("outputs/figures/FigureS9_conductivity.png", fig_s9_conductivity, 
       width = 12, height = 15,dpi=300)



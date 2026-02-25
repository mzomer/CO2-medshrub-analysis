# ────────────────────────────────────────────────────────────────
# setup.R — Load libraries, source functions, set project options
# ────────────────────────────────────────────────────────────────


## Load packages
library(tidyverse)       # Core tidyverse
library(lme4)            # Linear/mixed models
library(lmerTest)        # p-values for lmer models
library(DHARMa)          # Residual diagnostics for mixed models
library(emmeans)         # pairwise comparisons       
library(ggeffects)       # Marginal effects and predictions
library(here)            # Relative paths
library(patchwork)       # plot layout
library(RColorBrewer)    # colour palette 



## Global options
options(stringsAsFactors = FALSE)

## Set default plotting theme
theme_set(theme_minimal())

## Print session info 
message("R version: ", R.Version()$version.string) # R version 4.5.0 (2025-04-11)

sessionInfo()


## METADATA ====================================================================================
# SCRIPT: 04_SEMs_non-frugivorous.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# This script repeats the Structural Equation Models (SEMs) focusing solely on non-frugivorous animal species
# (i.e., those with 0% fruit in their diet). The aim is to evaluate whether the positive relationships observed 
# between frugivore FRic/SRic and Annonaceae FRic/SRic could be due to factors unrelated to frugivory,
# potentially indicating a Type I error. We expected that the absence of frugivory-related interactions 
# should lead to a negative or absence of relationship between mammal or bird FRic/SRic and Annonaceae 
# FRic/SRic in the SEMs.


# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(performance)
library(stats)

### Load data ####
df_f0 <- read.csv("Data/sem_f0_bc")


#######################################################################################
### Structural Equation Models ####

# Function to identify the weakest regression path
identify_weakest <- function(model_fit) {
  summary_fit <- summary(model_fit, stand = TRUE, rsq = TRUE, fit.measures = TRUE)
  reg_paths <- summary_fit$pe[summary_fit$pe$op == "~", ]
  if (nrow(reg_paths) > 0) {
    weakest_path <- reg_paths %>%
      filter(pvalue == max(pvalue, na.rm = TRUE)) %>%
      select(lhs, rhs, pvalue)
  } else {
    weakest_path <- data.frame(lhs = character(), rhs = character(), pvalue = numeric())
  }
  return(weakest_path)
}


#### GLOBAL ####

# Base model
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_fric_sqrt ~ brd_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_fric_sqrt ~ mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
annon_sric_sqrt ~ brd_sric_sqrt + mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt 
'

# Fit the SEM model
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f0)

# Check weakest regression path
identify_weakest(sem.fit)

# Check covariates
modindices(sem.fit)

# Fitted model 
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_fric_sqrt ~ brd_sric_sqrt + area_km2 + annual_prec + elev_range
mml_fric_sqrt ~ mml_sric_sqrt + area_km2 + prec_season
annon_sric_sqrt ~ brd_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season
brd_sric_sqrt ~ area_km2 + annual_temp + elev_range
mml_sric_sqrt ~ area_km2 + annual_prec + elev_range + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt
'
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f0)

# Model summary with standardized coefficients and fit measures
summary(sem.fit, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)
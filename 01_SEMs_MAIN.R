## METADATA ====================================================================================
# SCRIPT: 01_SEMs_MAIN.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# This script performs Structural Equation Models (SEMs, Rosseel, 2012) to investigate the 
# direct and indirect (cascading) effects of biotic and abiotic variables on Annonaceae species 
# richness (SRic) and frugivory-related functional richness (FRic). 

# METHODOLOGY:
# - Structural Equation Models (SEMs, Rosseel, 2012) were constructed based on theoretical 
#   expectations. We first included all hypothesized direct and indirect pathways among the predictor variables, 
#   and gradually removed paths with the least statistical significance until only significant paths (at p < 0.05) remained. 
# - Predictor variables include:
#   - Biotic: Frugivorous bird and mammal FRic, SRic of Annonaceae, birds, and mammals.
#   - Abiotic: Elevation range, NPP range, mean annual temperature, mean annual 
#     precipitation, mean precipitation seasonality, and area size.
# - Square root-transformed was applied to the following variables to improve residual 
#   normality of the models: FRic of birds and mammals, and SRic of Annonaceae, birds, and mammals.
# - All predictors were re-scaled to a range between 0 and 1 to facilitate comparison of their effects.
# - SEMs were evaluated based on the following fit indices (Schumacker & Lomax, 2004):
#   - Chi-square test (p > 0.05)
#   - Comparative Fit Index (CFI > 0.90)
#   - Root Mean Square Error of Approximation (RMSEA < 0.05 or 0.08)
# - A covariance parameter between bird and mammal SRic was included a priori due to their high Pearson’s 
#   correlation (r = 0.8654914). The model's modification indices were evaluated, and when necessary, a 
#   posteriori covariance parameters were incorporated to improve the overall model fit. 

# REFERENCES:
# - Rosseel Y. 2012. lavaan: an R package for structural equation modeling. Journal of Statistical Software 48: 1–36.
# - Schumacker RE, Lomax RG. 2004. A beginner's guide to structural equation modeling. Second edition. Mahwah, NJ: Lawrence Erlbaum Associates.



# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(performance)

### Load data ####
df_f50 <- read.csv("Data/sem_f50_bc")

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
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)

# Check weakest regression path
identify_weakest(sem.fit)

# Check covariates
modindices(sem.fit)

# Fitted model 
mod <- 'annon_fric ~ annon_sric_sqrt + mml_fric_sqrt
brd_fric_sqrt ~ brd_sric_sqrt + npp_range
mml_fric_sqrt ~ mml_sric_sqrt
annon_sric_sqrt ~ mml_sric_sqrt + area_km2 + annual_prec
brd_sric_sqrt ~ area_km2 + annual_temp + prec_season + elev_range
mml_sric_sqrt ~ area_km2 + annual_temp + prec_season + elev_range
brd_sric_sqrt ~~ mml_sric_sqrt 
brd_fric_sqrt ~~ mml_fric_sqrt
mml_fric_sqrt ~~ mml_sric_sqrt
mml_fric_sqrt ~~ annon_sric_sqrt
annon_fric ~~ brd_fric_sqrt
brd_fric_sqrt ~~ mml_sric_sqrt
annon_sric_sqrt ~~ mml_sric_sqrt
annon_sric_sqrt ~~ brd_sric_sqrt 
'
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)

# Model summary with standardized coefficients and fit measures
summary(sem.fit, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)



##### Checking variance explained when including only abiotic environment as predictors #####
# Removing the biotic effects, fitting again and checking the variance explained 

# Base model 
mod <- 'annon_fric ~ annon_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
annon_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
'
sem.fit.red <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)

# Fitted model 
mod <- 'annon_fric ~ annon_sric_sqrt
annon_sric_sqrt ~ area_km2 + annual_temp + annual_prec + elev_range
'
sem.fit.red <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)

# Model summary with standardized coefficients and fit measures
summary(sem.fit.red, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit.red, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)
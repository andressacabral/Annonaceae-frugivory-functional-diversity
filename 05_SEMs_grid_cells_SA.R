## METADATA ====================================================================================
# SCRIPT: 05_SEMs_grid_cells_SA.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# Here, we evaluated whether the positive effects of frugivore FRic/SRic on Annonaceae FRic/SRic 
# is influenced by the spatial scale of analysis. To test this, we repeated the
# global SEM using a finer spatial resolution (grid cells with about 110-km resolution with 
# Behrmann equal area projection). This approach allows us to determine whether observed patterns 
# are driven by differences in the area size of assemblages.



# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(performance)

### Load data ####
df_f50 <- read.csv("Data/sem_f50_grid")


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
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + annual_temp + temp_season + annual_prec + prec_season
brd_fric_sqrt ~ brd_sric_sqrt + annual_temp + temp_season + annual_prec + prec_season
mml_fric_sqrt ~ mml_sric_sqrt + annual_temp + temp_season + annual_prec + prec_season
annon_sric_sqrt ~ brd_sric_sqrt + mml_sric_sqrt + annual_temp + temp_season + annual_prec + prec_season
brd_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
mml_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
brd_sric_sqrt ~~ mml_sric_sqrt 
'

# Fit the SEM model
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)

# Check weakest regression path
identify_weakest(sem.fit)

# Check covariates
modindices(sem.fit)

# Fitted model
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + temp_season + annual_prec
brd_fric_sqrt ~ brd_sric_sqrt + temp_season + annual_prec + prec_season
mml_fric_sqrt ~ mml_sric_sqrt + annual_temp + temp_season
annon_sric_sqrt ~ mml_sric_sqrt + annual_prec
brd_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
mml_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
brd_sric_sqrt ~~ mml_sric_sqrt 
brd_fric_sqrt ~~ mml_fric_sqrt
mml_fric_sqrt ~~ brd_sric_sqrt 
mml_fric_sqrt ~~ mml_sric_sqrt
brd_fric_sqrt ~~ brd_sric_sqrt 
brd_fric_sqrt ~~ mml_sric_sqrt
mml_fric_sqrt ~~ annon_sric_sqrt 
brd_fric_sqrt ~~ annon_sric_sqrt
annon_fric ~~ brd_sric_sqrt
'
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)

# Model summary with standardized coefficients and fit measures
summary(sem.fit, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)
## METADATA ====================================================================================
# SCRIPT: 08_SEMs_MAIN_realms.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# In this script, we investigate potential differences in the effects of frugivore FRic on Annonaceae FRic across 
# biogeographical realms. To this end, we repeated the SEMs approach for the Afrotropics, Asia-Pacific, and Neotropics.


# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(performance)

### Load data ####
df_f50 <- read.csv("Data/sem_f50_bc")

### Assign realms based on continents ####
unique(df_f50$continent)
#[1] "AFRICA"           "ASIA-TROPICAL"    "SOUTHERN AMERICA" "ASIA-TEMPERATE"  
#[5] "NORTHERN AMERICA" "AUSTRALASIA" "PACIFIC"  
df_f50 <- df_f50 %>%
  mutate(realm = case_when(
    continent %in% c('ASIA-TEMPERATE', 'ASIA-TROPICAL', 'AUSTRALASIA', 'PACIFIC') ~ 'Asia-Pacific',
    continent %in% c('SOUTHERN AMERICA', 'NORTHERN AMERICA') ~ 'Neotropics',
    continent == 'AFRICA' ~ 'Africa'
  ))

### Filter realms ####
afr <- df_f50 %>% filter(realm == 'Africa')
asia.pac <- df_f50 %>% filter(realm == 'Asia-Pacific')
neot <- df_f50 %>% filter(realm == 'Neotropics')


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

#### AFROTROPICS ####

# Base model
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_fric_sqrt ~ brd_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_fric_sqrt ~ mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
annon_sric_sqrt ~ brd_sric_sqrt + mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt # following global model
'
# Fit the SEM model
sem.fit.afr <- sem(mod, data = afr)

# Check weakest regression path
identify_weakest(sem.fit.afr)

# Check covariates
modindices(sem.fit.afr)

# Fitted model
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + annual_prec + elev_range
brd_fric_sqrt ~ brd_sric_sqrt + annual_prec
mml_fric_sqrt ~ mml_sric_sqrt + area_km2
annon_sric_sqrt ~ mml_sric_sqrt
brd_sric_sqrt ~ area_km2 + annual_temp + prec_season + elev_range
mml_sric_sqrt ~ area_km2 + annual_prec + prec_season + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt # following global model
'
sem.fit.afr <- sem(mod, data = afr)

# Model summary with standardized coefficients and fit measures
summary(sem.fit.afr, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit.afr, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)



#### ASIA-PACIFIC ####

# Base model
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_fric_sqrt ~ brd_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_fric_sqrt ~ mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
annon_sric_sqrt ~ brd_sric_sqrt + mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt # following global model
'
# Fit the SEM model
sem.fit.asia.pac <- sem(mod, data = asia.pac)

# Check weakest regression path
identify_weakest(sem.fit.asia.pac)

# Check covariates
modindices(sem.fit.asia.pac)

# Fitted model 
mod <- 'annon_fric ~ annon_sric_sqrt + prec_season + elev_range
brd_fric_sqrt ~ brd_sric_sqrt
mml_fric_sqrt ~ mml_sric_sqrt
annon_sric_sqrt ~ brd_sric_sqrt + elev_range
brd_sric_sqrt ~ annual_temp + elev_range
mml_sric_sqrt ~ area_km2 + annual_temp + prec_season + elev_range
brd_sric_sqrt ~~ mml_sric_sqrt # following global model
mml_fric_sqrt ~~ annon_sric_sqrt
'
sem.fit.asia.pac <- sem(mod, data = asia.pac)

# Model summary with standardized coefficients and fit measures
summary(sem.fit.asia.pac, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit.asia.pac, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)


#### NEOTROPICS ####

# Base model
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_fric_sqrt ~ brd_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_fric_sqrt ~ mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
annon_sric_sqrt ~ brd_sric_sqrt + mml_sric_sqrt + area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
mml_sric_sqrt ~ area_km2 + annual_temp + annual_prec + prec_season + elev_range + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt # following global model
'

# Fit the SEM model
sem.fit.neot <- sem(mod, data = neot)

# Check weakest regression path
identify_weakest(sem.fit.neot)

# Check covariates
modindices(sem.fit.neot)

# Fitted model 
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + elev_range + npp_range
brd_fric_sqrt ~ brd_sric_sqrt + npp_range
mml_fric_sqrt ~ mml_sric_sqrt + area_km2 + annual_temp + npp_range
annon_sric_sqrt ~ mml_sric_sqrt + area_km2 + annual_temp + annual_prec + elev_range
brd_sric_sqrt ~ area_km2 + annual_temp + prec_season + elev_range + npp_range
mml_sric_sqrt ~ area_km2 + annual_temp + prec_season + elev_range + npp_range
brd_sric_sqrt ~~ mml_sric_sqrt # following global model
brd_fric_sqrt ~~ mml_fric_sqrt 
brd_fric_sqrt ~~ annon_sric_sqrt 
mml_fric_sqrt ~~ annon_sric_sqrt 
'
sem.fit.neot <- sem(mod, data = neot)

# Model summary with standardized coefficients and fit measures
summary(sem.fit.neot, stand = TRUE, rsq = TRUE, fit.measures = TRUE)

# Plot the SEM model with standardized coefficients
lavaanPlot(model = sem.fit.neot, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)


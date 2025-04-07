## METADATA ====================================================================================
# SCRIPT: 03_Null_models_MAIN.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# Here, we evaluate whether the observed effects of frugivore functional 
# richness (FRic) and species richness (SRic) on Annonaceae FRic/SRic deviate 
# from random expectations. A null model approach is implemented by randomly 
# shuffling Annonaceae FRic/SRic across botanical country assemblages, followed 
# by re-running the Structural Equation Model (SEM) 1000 times. We then compare 
# the empirical model's effect sizes and p-values against the null distribution 
# to determine statistical significance.



# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(performance)

### Load data ####
df_f50 <- read.csv("Data/sem_f50_bc")


### NULL MODEL ####

#### Shuffle Annonaceae data ####
df2 <- df_f50
result_table_p <- vector()
result_table_std.est <- vector()
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
for(i in 1:1000){
  set.seed(i)
  df2$LEVEL_3_CO_rand <- sample(df2$LEVEL_3_CO)
  df2 <- df2[,c("LEVEL_3_CO_rand","annon_fric","annon_sric_sqrt")]
  colnames(df2)[1] <- "LEVEL_3_CO"
  df3 <- df_f50[ , -which(names(df_f50) %in% c("annon_fric","annon_sric_sqrt"))]
  dd.shuffle <- merge(df2,df3, by="LEVEL_3_CO")
  sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data=dd.shuffle)
  result <- summary(sem.fit, stand=T, rsq=T, fit.measures=T)
  result_p <- result$pe[c(2,6),8] # significance (pvalue)
  result_std.est <- result$pe[c(2,6),10] # extract the effect size (std.all). In this case, effect of animals on Annonaceae FRIC and SRIC
  result_table_p[i] <- paste(result_p[1],result_p[2],sep=";")
  result_table_std.est[i] <- paste(result_std.est[1],result_std.est[2],sep=";")
}
result_table_sig <- as.numeric(unlist(strsplit(result_table_p, ";")))
result_table_sig 
result_table_std.est <- as.numeric(unlist(strsplit(result_table_std.est, ";")))
result_table_std.est 

# Checking the order of relationships
summary(sem.fit, stand=T, rsq=T, fit.measures=T)$pe[c(2,6),3] #"mml_fric_sqrt" "mml_sric_sqrt"

# Separating datasets
index_data1 <- seq(1, length(result_table_sig), by = 2)
index_data2 <- seq(2, length(result_table_sig), by = 2)
sig_mml_fric_sqrt <- result_table_sig[index_data1]
sig_mml_sric_sqrt <- result_table_sig[index_data2]
std.est_mml_fric_sqrt <- result_table_std.est[index_data1]
std.est_mml_sric_sqrt <- result_table_std.est[index_data2]


### SEM empirical data ####
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
sem.fit.emp <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)
empirical <- summary(sem.fit.emp, standardized = T)
empirical.sig <- empirical$pe[c(2,6),8]
empirical.est <- empirical$pe[c(2,6),10]


### Comparing estimates of simulated and empirical data ####

##### FRic #####

# Visualize effect sizes 
hist(std.est_mml_fric_sqrt, xlim=c(-0.3, 0.3), main="Simulation mammal FRic global")
abline(v=c(empirical.est[1]), col="red") 

# Check if empirical value falls outside the approximately 95% distribution of the simulated data
# Calculate the third quartile (Q3)
q3 <- quantile(std.est_mml_fric_sqrt, 0.95)
# Check if my_value falls outside Q3
outside_q3 <- empirical.est[1] > q3
if (outside_q3) {
  cat("My value falls outside the approximately 95% distribution.\n")
} else {
  cat("My value is within the approximately 95% distribution.\n")
}

##### SRIC #####

# Visualize effect sizes 
hist(std.est_mml_sric_sqrt, xlim=c(-1, 1), main="Simulation mammal SRic global")
abline(v=c(empirical.est[2]), col="red") #plot observed effect size as v=XXX

# Check if empirical value falls outside the approximately 95% distribution of the simulated data
# Calculate the third quartile (Q3)
q3 <- quantile(std.est_mml_sric_sqrt, 0.95)
# Check if my_value falls outside Q3
outside_q3 <- empirical.est[2] > q3
if (outside_q3) {
  cat("My value falls outside the approximately 95% distribution.\n")
} else {
  cat("My value is within the approximately 95% distribution.\n")
}


### Comparing p-values of simulated and empirical data ####

##### FRIC #####
hist(sig_mml_fric_sqrt, xlim=c(-0.5, 1.5), main="Simulation mammal FRic global")
abline(v=c(empirical.sig[1]), col="red") 
sum(sig_mml_fric_sqrt>0.05)

##### SRIC #####
hist(sig_mml_sric_sqrt, xlim=c(-0.5, 1.5), main="Simulation mammal SRic global")
abline(v=c(empirical.sig[2]), col="red")
sum(sig_mml_sric_sqrt>0.05)

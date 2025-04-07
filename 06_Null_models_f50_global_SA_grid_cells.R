## METADATA ====================================================================================
# SCRIPT: 06_NULL_models_f50_global_SA_grid_cells.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# This script evaluates whether the observed effects of frugivore functional 
# richness (FRic) and species richness (SRic) on Annonaceae FRic/SRic deviate 
# from random expectations when using a finer spatial resolution (grid cells with 
# about 110-km resolution with Behrmann equal area projection). A null model approach 
# is implemented by randomly shuffling Annonaceae FRic/SRic across grid cells, 
# followed by re-running the Structural Equation Model (SEM) 1000 times. We then 
# compare the empirical model's effect sizes and p-values against the null 
# distribution to determine statistical significance.



# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(performance)

### Load data ####
df_f50 <- read.csv("Data/sem_f50_grid")


#### SEM empirical data ####
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + annual_prec
brd_fric_sqrt ~ brd_sric_sqrt + annual_temp
mml_fric_sqrt ~ mml_sric_sqrt + annual_temp + temp_season
annon_sric_sqrt ~ mml_sric_sqrt + annual_prec + prec_season
brd_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
mml_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
brd_sric_sqrt ~~ mml_sric_sqrt 
mml_fric_sqrt ~~ brd_sric_sqrt
brd_fric_sqrt ~~ mml_sric_sqrt
mml_fric_sqrt ~~ mml_sric_sqrt
brd_fric_sqrt ~~ brd_sric_sqrt
brd_fric_sqrt ~~ mml_fric_sqrt
mml_fric_sqrt ~~ annon_sric_sqrt
brd_fric_sqrt ~~ annon_sric_sqrt 
'
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)


### NULL MODEL ####

#### Shuffle Annonaceae data ####
df2 <- df_f50
result_table_p <- vector()
result_table_std.est <- vector()
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + annual_prec
brd_fric_sqrt ~ brd_sric_sqrt + annual_temp
mml_fric_sqrt ~ mml_sric_sqrt + annual_temp + temp_season
annon_sric_sqrt ~ mml_sric_sqrt + annual_prec + prec_season
brd_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
mml_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
brd_sric_sqrt ~~ mml_sric_sqrt 
mml_fric_sqrt ~~ brd_sric_sqrt
brd_fric_sqrt ~~ mml_sric_sqrt
mml_fric_sqrt ~~ mml_sric_sqrt
brd_fric_sqrt ~~ brd_sric_sqrt
brd_fric_sqrt ~~ mml_fric_sqrt
mml_fric_sqrt ~~ annon_sric_sqrt
brd_fric_sqrt ~~ annon_sric_sqrt 
'
for(i in 1:1000){
  set.seed(i)
  df2$X_rand <- sample(df2$X)
  df2 <- df2[,c("X_rand","annon_fric","annon_sric_sqrt")]
  colnames(df2)[1] <- "X"
  df3 <- df_f50[ , -which(names(df_f50) %in% c("annon_fric","annon_sric_sqrt"))]
  dd.shuffle <- merge(df2,df3, by="X")
  sem.fit_rdm <- sem(mod, missing = "fiml", estimator = "MLR", data=dd.shuffle)
  result <- summary(sem.fit_rdm, stand=T, rsq=T, fit.measures=T)
  result_p <- result$pe[c(2,3,10),8] # significance (pvalue)
  result_std.est <- result$pe[c(2,3,10),10] # extract the effect size (std.all). In this case, effect of animals on Annonaceae FRIC and SRIC
  result_table_p[i] <- paste(result_p[1],result_p[2],result_p[3], sep=";")
  result_table_std.est[i] <- paste(result_std.est[1],result_std.est[2],result_std.est[3], sep=";")
}
result_table_sig <- as.numeric(unlist(strsplit(result_table_p, ";")))
result_table_sig 
result_table_std.est <- as.numeric(unlist(strsplit(result_table_std.est, ";")))
result_table_std.est 

# Checking the order of relationships
summary(sem.fit, stand=T, rsq=T, fit.measures=T)$pe[c(2,3,10),3]#"brd_fric_sqrt" "mml_fric_sqrt" "mml_sric_sqrt"

# Separating datasets
index_data1 <- seq(1, length(result_table_sig), by = 3)
index_data2 <- seq(2, length(result_table_sig), by = 3)
index_data3 <- seq(3, length(result_table_sig), by = 3)
sig_brd_fric_sqrt <- result_table_sig[index_data1]
sig_mml_fric_sqrt <- result_table_sig[index_data2]
sig_mml_sric_sqrt <- result_table_sig[index_data3]
std.est_brd_fric_sqrt <- result_table_std.est[index_data1]
std.est_mml_fric_sqrt <- result_table_std.est[index_data2]
std.est_mml_sric_sqrt <- result_table_std.est[index_data3]


#### SEM empirical data ####
mod <- 'annon_fric ~ annon_sric_sqrt + brd_fric_sqrt + mml_fric_sqrt + annual_prec
brd_fric_sqrt ~ brd_sric_sqrt + annual_temp
mml_fric_sqrt ~ mml_sric_sqrt + annual_temp + temp_season
annon_sric_sqrt ~ mml_sric_sqrt + annual_prec + prec_season
brd_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
mml_sric_sqrt ~ annual_temp + temp_season + annual_prec + prec_season
brd_sric_sqrt ~~ mml_sric_sqrt 
mml_fric_sqrt ~~ brd_sric_sqrt
brd_fric_sqrt ~~ mml_sric_sqrt
mml_fric_sqrt ~~ mml_sric_sqrt
brd_fric_sqrt ~~ brd_sric_sqrt
brd_fric_sqrt ~~ mml_fric_sqrt
mml_fric_sqrt ~~ annon_sric_sqrt
brd_fric_sqrt ~~ annon_sric_sqrt 
'
sem.fit <- sem(mod, missing = "fiml", estimator = "MLR", data = df_f50)
empirical_data <- summary(sem.fit, stand = TRUE, rsq = TRUE, fit.measures = TRUE)
empirical_data.sig <- empirical_data$pe[c(2,3,10),8]
empirical_data.est <- empirical_data$pe[c(2,3,10),10]


### Comparing estimates of simulated and empirical data ####


##### FRic #####

###### Bird FRIC on Annonaceae FRIC #####

# Visualize effect sizes 
hist(std.est_brd_fric_sqrt, xlim=c(-0.13, 0.13), main="simulation bird FRic global")
abline(v=c(empirical_data.est[1]), col="red") #plot observed effect size as v=XXX

# Check if empirical value falls outside the approximately 95% distribution of the simulated data
# Calculate the third quartile (Q3)
q3 <- quantile(std.est_brd_fric_sqrt, 0.95)
# Check if my_value falls outside Q3
outside_q3 <- empirical_data.est[1] > q3
if (outside_q3) {
  cat("My value falls outside the approximately 95% distribution.\n")
} else {
  cat("My value is within the approximately 95% distribution.\n")
}
#My value falls outside the approximately 95% distribution.

###### Mammal FRIC on Annonaceae FRIC #####

# Visualize effect sizes 
hist(std.est_mml_fric_sqrt, xlim=c(-0.07, 0.07), main="simulation mammal FRic global")
abline(v=c(empirical_data.est[2]), col="red") #plot observed effect size as v=XXX

# Check if empirical value falls outside the approximately 95% distribution of the simulated data
# Calculate the third quartile (Q3)
q3 <- quantile(std.est_mml_fric_sqrt, 0.95)
# Check if my_value falls outside Q3
outside_q3 <- empirical_data.est[2] > q3
if (outside_q3) {
  cat("My value falls outside the approximately 95% distribution.\n")
} else {
  cat("My value is within the approximately 95% distribution.\n")
}
#My value falls outside the approximately 95% distribution.


##### SRic #####

###### Mammal SRIC on Annonaceae SRIC #####

# Visualize effect sizes 
hist(std.est_mml_sric_sqrt, xlim=c(-0.3, 0.3), main="simulation mammal SRic global")
abline(v=c(empirical_data.est[3]), col="red") #plot observed effect size as v=XXX

# Check if empirical value falls outside the approximately 95% distribution of the simulated data
# Calculate the third quartile (Q3)
q3 <- quantile(std.est_mml_sric_sqrt, 0.95)
# Check if my_value falls outside Q3
outside_q3 <- empirical_data.est[3] > q3
if (outside_q3) {
  cat("My value falls outside the approximately 95% distribution.\n")
} else {
  cat("My value is within the approximately 95% distribution.\n")
}
#My value falls outside the approximately 95% distribution.



### Comparing p-values of simulated and empirical data ####

##### BIRD FRIC on Annonaceae FRIC #####
hist(sig_brd_fric_sqrt, xlim=c(-0.5, 1.5), main="simulation bird FRic global")
abline(v=c(empirical_data.sig[1]), col="red") #plot observed effect size as v=XXX
sum(sig_brd_fric_sqrt>0.05)

##### MAMMAL FRIC on Annonaceae FRIC #####
hist(sig_mml_fric_sqrt, xlim=c(-0.5, 1.5), main="simulation mammal FRic global")
abline(v=c(empirical_data.sig[2]), col="red") #plot observed effect size as v=XXX
sum(sig_mml_fric_sqrt>0.05)

##### Mammal SRIC on Annonaceae SRIC #####
hist(sig_mml_sric_sqrt, xlim=c(-0.5, 1.5), main="simulation mammal SRic global")
abline(v=c(empirical_data.sig[3]), col="red") #plot observed effect size as v=XXX
sum(sig_mml_sric_sqrt>0.05)


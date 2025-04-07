## METADATA ====================================================================================
# SCRIPT: 10_Fourth-corner_MAIN.R
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# This script performs a modified fourth-corner analysis (Legendre et al., 1997; Dray & Legendre, 2008; 
# Dehling et al., 2014) to investigate trait matching between co-occurring Annonaceae and mammals aligned
# with functional diversity relationships from the global model.
# METHODOLOGY:
# - The fourth-corner analysis estimates trait matching between Annonaceae and frugivores using three matrices:
#   - Species interaction matrix (L): Co-occurring Annonaceae and frugivore species in botanical countries.
#   - Annonaceae trait matrix (R): Annonaceae species × trait data.
#   - Frugivore trait matrix (Q): Frugivorous mammal species × trait data.
# - Analysis focused on hypothesized trait relationships.
# - Statistical significance was assessed using permutation model 6 (Dray & Legendre, 2008; ter Braak et al., 2012).

# REFERENCES:
# - Dray S, Legendre P. 2008. Testing the species traits–environment relationships: the fourth‐corner problem revisited. Ecology 89(12): 3400–3412.
# - Dehling DM, Töpfer T, Schaefer HM, Jordano P, Böhning‐Gaese K, Schleuning M. 2014. Functional relationships beyond species richness patterns: trait matching in plant–bird mutualisms across scales. Global Ecology and Biogeography 23(10): 1085–1093.
# - Legendre P, Galzin R, Harmelin-Vivien ML. 1997. Relating behavior to habitat: solutions to the fourth‐corner problem. Ecology 78(2): 547–562.
# - ter Braak CJ, Cormont A, Dray S. 2012. Improved testing of species traits–environment relationships in the fourth‐corner problem. Ecology 93(7): 1525–1526.



# Clean R environment
rm(list=ls())

### Load required packages ####
library(ade4)
library(dplyr)
library(plyr)
library(reshape2)

### Load data ####
dummy_interac_ann_mml <- read.csv("Data/binary_cooccurrence_FC")#presence and absence of co-occurrence
traits_annon <- read.csv("Data/traits_annon_FC")# Annonaceae traits
traits_mml_f50 <- read.csv("Data/traits_mammals_FC")# Frugivorous mammals traits

### Adjusting tables ####
rownames(dummy_interac_ann_mml) <- dummy_interac_ann_mml$X
dummy_interac_ann_mml$X <- NULL
rownames(traits_annon) <- traits_annon$X
traits_annon$X <- NULL
rownames(traits_mml_f50) <- traits_mml_f50$X
traits_mml_f50$X <- NULL

# Checking data structure
str(traits_annon)
str(traits_mml_f50)

# Adjusting trait class
traits_annon[ ,c(2:4)] <- lapply(traits_annon[ ,c(2:4)], as.factor)
traits_mml_f50[ ,c(2:4)] <- lapply(traits_mml_f50[ ,c(2:4)], as.factor)

# Checking number of levels per trait
str(traits_annon)
str(traits_mml_f50)

# Checking again if species names are matching in spatial and trait data
length(rownames(traits_annon))==length(rownames(dummy_interac_ann_mml))#TRUE
length(rownames(traits_mml_f50))==length(colnames(dummy_interac_ann_mml))#TRUE

# Reorder rows of dummy_interac_ann_mml to match the row order in traits_annon
dummy_interac_ann_mml <- dummy_interac_ann_mml[match(rownames(traits_annon), rownames(dummy_interac_ann_mml)), ]
# Reorder columns of dummy_interac_ann_mml to match the column order in traits_mml_f50
dummy_interac_ann_mml <- dummy_interac_ann_mml[, match(rownames(traits_mml_f50), colnames(dummy_interac_ann_mml))]

# Checking order of the names, they should also match
head(rownames(traits_annon))
head(rownames(dummy_interac_ann_mml))
head(rownames(traits_mml_f50))
head(colnames(dummy_interac_ann_mml))


### Fourth corner analysis ####
set.seed(123)
result_FC_ann_a2_mml <- fourthcorner(traits_annon, dummy_interac_ann_mml, traits_mml_f50, modeltype=6, nrepet = 1000, p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")
result_FC <- summary(result_FC_ann_a2_mml)
#write.csv(result_FC,"result_FC_ann_mml")
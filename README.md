**Data and codes for:** ‘Seed-dispersing vertebrates and the abiotic environment shape functional diversity of the pantropical Annonaceae’

**Article DOI:** 10.1111/nph.70113

**Journal:** New Phytologist

**Authors:** Andressa Cabral, Irene Bender, Thomas Couvreur, Søren Faurby, Oskar Hagen, Isabell Hensen, Ingolf Kühn, Carlos Rodriguez-Vaz, Hervé Sauquet, Joseph A. Tobias & Renske E. Onstein


----
Scripts:

> 01_SEMs_MAIN.R

Structural Equation Models (SEMs) to evaluate the effects of biotic and abiotic factors on Annonaceae species richness (SRic) and frugivory-related functional richness (FRic).

> 02_Spatial_autocorrelation_MAIN.R

Spatial autocorrelation analyses using simultaneous autoregressive error models and Moran’s I spatial correlograms.

> 03_Null_models_MAIN.R

Null models for SEM outcomes, used to evaluate whether the effects of frugivore FRic/SRic on Annonaceae FRic/SRic deviate from random expectations.

> 04_SEMs_non-frugivorous.R

SEMs using only non-frugivorous species. Used to investigate whether the SEM results for frugivorous species hold when using non-frugivorous species.

> 05_SEMs_grid_cells_SA.R

Sensitivity analyses: SEMs to evaluate the effects of biotic and abiotic factors on Annonaceae FRic using a more refined spatial resolution (i.e., grid cells instead of botanical countries).

> 06_Null_models_f50_global_SA_grid_cells.R

Sensitivity analyses: Null models for SEM outcomes, using a more refined spatial resolution (i.e., grid cells instead of botanical countries). Used to evaluate whether the effects of frugivore FRic/SRic on Annonaceae FRic/SRic deviate from random expectations.

> 07_SEMs_grid_cells_non-frugivorous_SA.R

Sensitivity analyses: SEMs using only non-frugivorous species and a more refined spatial resolution (i.e., grid cells instead of botanical countries). Used to investigate whether the SEM results for frugivorous species hold when using non-frugivorous species.

> 08_SEMs_MAIN_realms.R

SEMs to investigate differences in the effects of frugivore FRic on Annonaceae FRic across biogeographical realms.

> 09_SEMs_non-frugivorous_realms.R

SEMs using only non-frugivorous species for each biogeographical realm. Used to investigate whether differences in the effects of frugivore FRic on Annonaceae FRic across biogeographical realms result from factors other than frugivory-related interactions.

> 10_Fourth-corner_MAIN.R

Fourth-corner analysis to assess trait matching between Annonaceae and frugivorous mammals.



----
Description of data: 
 

- **Data/Annonaceae_trait_matrix**
  
  Table containing raw trait data for 1,895 Annonaceae species across 109 genera.
  
- **Data/Annonaceae_reference_entries**
  
  Explicit references for each data entry in ‘Annonaceae_trait_matrix’.

- **Data/Annonaceae_reference_details**
  
  Full citation details for ‘Annonaceae_reference_entries’.

- **Data/sem_f50_bc**
  
  Input for the Structural Equation Models focused exclusively on frugivorous animals.
This table includes species richness and functional richness for Annonaceae, frugivorous birds, and mammals, as well as abiotic environmental data for each botanical country.
A square root transformation was applied to the following variables to improve normality of residuals in the models: functional richness of birds and mammals, and species richness of Annonaceae, birds, and mammals.
All predictors were scaled to a range between 0 and 1 to facilitate comparison of their effects.

- **Data/sem_f50_grid**
  
  Same as 'sem_f50_bc' but with a more refined spatial resolution than botanical countries (i.e., using grid cells with approximately 110-km2 resolution, based on the Behrmann cylindrical equal-area projection with standard parallels at 30°).

- **Data/sem_f0_bc**
  
  Input for the Structural Equation Models focused exclusively on non-frugivorous animals.
This table includes species richness and functional richness for Annonaceae, non-frugivorous birds, and mammals, as well as abiotic environmental data for each botanical country.
A square root transformation was applied to the following variables to improve normality of residuals in the models: functional richness of birds and mammals, and species richness of Annonaceae, birds, and mammals.
All predictors were scaled to a range between 0 and 1 to facilitate comparison of their effects.

- **Data/sem_f0_grid**
  
  Same as 'sem_f0_bc' but with a more refined spatial resolution than botanical countries (i.e., using grid cells with approximately 110-km2 resolution, based on the Behrmann cylindrical equal-area projection with standard parallels at 30°).

- **Data/traits_annon_FC**
  
  Annonaceae trait dataset used in the fourth-corner analyses. 
Continuous traits were processed as follows: when a range of values was reported, the mean was taken; unique values per entry were retained as originally recorded. These traits were then log-transformed (log1p) and scaled to a range of 0 to 1. 
Categorical traits were converted into a presence-absence format for each trait state.

- **Data/traits_mammals_FC**
  
  Trait dataset for frugivorous mammals used in the fourth-corner analyses. 
Continuous traits were log-transformed (log1p) and scaled to a range of 0 to 1. 
Categorical traits were converted into a presence-absence format for each trait state.

- **Data/binary_cooccurrence_FC**
  
  Presence-absence matrix for co-occurrence of Annonaceae and frugivorous mammals in botanical countries. Data used in the fourth-corner analyses.

- **Data/TDWG_level3_shp**

  Shapefile of botanical countries, input for the spatial autocorrelation analyses.


----

**Other files:** 


----

**Distribution data of Annonaceae from the Global Biodiversity Information Facility (GBIF)** 

  GBIF. 2022. GBIF.org GBIF Occurrence Download [WWW document] 
URL: https://doi.org/10.15468/dl.ddz83q 
Download date: 18 April 2022.

----

**Cleaned and curated occurrence records for Annonaceae species** 

  Erkens RH, Blanpain LM, Jara IC, Runge K, Verspagen N, Cosiaux A, Couvreur TL. 2022. Spatial distribution of Annonaceae across biomes and anthromes: Knowledge gaps in spatial and ecological data. Plants, People, Planet 5(4): 520–535.

----

**Occurrence of Annonaceae in botanical countries** 

  Govaerts R, Nic Lughadha E, Black N, Turner R, Paton A. 2021. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. Scientific Data 8: 1–10.

----

**Distribution range maps for birds**

  BirdLife International
URL: http://datazone.birdlife.org/species/requestdis
Download date: 14 February 2022.

----

**Distribution range maps for mammals**

  International Union for Conservation of Nature (IUCN)
URL: https://www.iucnredlist.org/resources/spatial-data-download
Download date: 3 May 2022.

----

**Trait and diet data for animals**
  
 - Faurby S, Davis M, Pedersen RØ, Schowanek SD, Antonelli A, Svenning JC. 2018. PHYLACINE 1.2: the phylogenetic atlas of mammal macroecology. Ecology 99(11): 2626.
 - Onstein RE, Vink DN, Veen J, Barratt CD, Flantua SGA, Wich SA, Kissling WD. 2020. Palm fruit colours are linked to the broad-scale distribution and diversification of primate colour vision systems. Proceedings of the Royal Society B: Biological Sciences 287: 20192731.
 - Tobias JA, Sheard C, Pigot AL, Devenish AJ, Yang J, Sayol F, Neate-Clegg MHC, Alioravainen N, Weeks TL, Barber RA et al. 2022. AVONET: morphological, ecological and geographical data for all birds. Ecology Letters 25(3): 581–597.
 - Wilman H, Belmaker J, Simpson J, de la Rosa C, Rivadeneira MM, Jetz W. 2014. EltonTraits 1.0: Species‐level foraging attributes of the world's birds and mammals: Ecological Archives E095‐178. Ecology 95(7): 2027–2027.

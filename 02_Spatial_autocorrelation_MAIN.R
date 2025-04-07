## METADATA ====================================================================================
# SCRIPT: 02_Spatial_autocorrelation_MAIN.r
# AUTHOR: Andressa Cabral
# DATE: 2024-02-27
# CONTACT: acabral@outlook.com.br
# R VERSION: 4.4.1 (2024-06-14)

# DESCRIPTION:
# In this script we evaluate the effect of spatial autocorrelation on the outcomes from
# the Structural Equation Models at global scale. Here we apply Simultaneous Autoregressive 
# Error (SARerr) models and compute a spatial correlogram of Moran's I vs. lag-distance. 
# This script was adapted from Onstein et al. (2020).

# REFERENCE:
# Onstein RE, Vink DN, Veen J, Barratt CD, Flantua SGA, Wich SA, Kissling WD. 2020. Palm fruit colours are linked to the broad-scale distribution and diversification of primate colour vision systems. Proceedings of the Royal Society B: Biological Sciences 287: 20192731.


# Clean R environment
rm(list=ls())

### Load required packages ####
library(dplyr)
library(rgdal)
library(spdep)
library(ncf)
library(spatialreg)


### Load data ####
df_f50 <- read.csv("Data/sem_f50_bc")
shape <- readOGR("Data/TDWG_level3_shp")

### Handle data ####
x<-coordinates(shape) 
x<-as.data.frame(x)
x$X<-x[,1]
x$Y<-x[,2]
x$Level3<-shape@data$ LEVEL_3_CO
str(x)

# Matching the X and Y with df_f50
df_f50$X<-x$X[match(df_f50$LEVEL_3_CO, x$Level3)] 
df_f50$Y<-x$Y[match(df_f50$LEVEL_3_CO, x$Level3)] 
annon <- na.omit(df_f50)

# Fitted model (GLOBAL SEMs)
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
# Equation 1: annon_fric ~ annon_sric_sqrt + mml_fric_sqrt
# Equation 2: annon_sric_sqrt ~ mml_sric_sqrt + area_km2 + annual_prec


### Spatial autocorrelation analyses ####


#### Equation 1: Annonaceae FRic as response variable ####

# Make a regression model to explain "annon_fric" including the variables that had a significant 
# effect in the fitted SEM (i.e., "annon_sric_sqrt" and "mml_fric_sqrt")
lm_frugi<-lm(annon_fric ~ annon_sric_sqrt + mml_fric_sqrt, data=annon)
summary(lm_frugi)


##### Spatial structure of residuals ####

# Correlograms with latlon = FALSE
cor.OBL<-correlog(annon$X, annon$Y, z=annon$annon_fric, na.rm=T, increment=1, resamp=1, latlon = FALSE)
cor.OBL
plot(cor.OBL$correlation, type="b", pch=16, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)  

# With latlon = TRUE and increment=1
cor.OBL_latlon<-correlog(annon$X, annon$Y, z=annon$annon_fric, na.rm=T, increment=1, resamp=1, latlon = TRUE)  #uses km because latlon = TRUE
plot(cor.OBL_latlon$correlation, type="b", pch=16, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)                  

# With latlon = TRUE and increment=1595.997 
cor.OBL_1000<-correlog(annon$X, annon$Y, z=annon$annon_fric, na.rm=T, increment=1595.997, resamp=999, latlon = TRUE)  #uses km because latlon = TRUE
plot(cor.OBL_1000$correlation, type="b", pch=16, cex=1.2, lwd=1.5,  xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0) 
summary(cor.OBL_1000)
cor.OBL_1000$mean.of.class
cor.OBL_1000$n
cor.OBL_1000$correlation      
cor.OBL_1000$p    

# Correlogram for residuals
cor.res_1000<-correlog(annon$X, annon$Y, z=residuals(lm_frugi), na.rm=T, increment=1595.997, resamp=999, latlon = TRUE)

# Plot both residuals and raw data
plot(cor.OBL_1000$correlation, type="b", pch=16, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)                  
points(cor.res_1000$correlation, pch=1, cex=1.2)
lines(cor.res_1000$correlation, lwd=1.5)


##### Implementing a spatial model ####

# Make coordinate list
coords_annon<-as.matrix(cbind(annon$X,annon$Y))
plot(coords_annon)

# Minimum distance to connect to at least one neighbor
annon_knear <- knn2nb(knearneigh(coords_annon, k=1))
summary(annon_knear)
dsts_annon<-unlist(nbdists(annon_knear, coords_annon, longlat = TRUE))
summary(dsts_annon)
max(dsts_annon)#1595.997

# Neighbour defined by dnearneigh()
annon_nb<-dnearneigh(coords_annon,0,max(dsts_annon), longlat=T)
par(mfrow=c(1,1))
plot(annon_nb, coords_annon, pch=20, lwd=2)
summary(annon_nb)

annon_nb_100<-dnearneigh(coords_annon,0,100, longlat=T)
summary(annon_nb_100)
plot(annon_nb_100, coords_annon, pch=20, lwd=2)

# Defining the spatial weights matrix
nb1_w<-nb2listw(annon_nb, glist=NULL, style="W", zero.policy=TRUE)
summary(nb1_w, zero.policy=TRUE)
plot(nb1_w, coords_annon)


##### Spatial autoregressive error model ####

sem_error_nb1_w<-spatialreg::errorsarlm(lm_frugi, listw=nb1_w, zero.policy=TRUE) #zero.policy=FALSE when taking min distance to connect all cells
summary(sem_error_nb1_w)

# When taking min distance between cells 
sem_error_nb1_w<-spatialreg::errorsarlm(lm_frugi, listw=nb1_w, zero.policy=FALSE) #zero.policy=FALSE when taking min distance to connect all cells
summary(sem_error_nb1_w)


##### Testing for spatial structure in the residuals of the spatial model ####

# Correlogram for spatial model
cor.SEM_1000<-correlog(annon$X, annon$Y, z=residuals(sem_error_nb1_w), na.rm=T, increment=1595.997, resamp=999, latlon = TRUE)

# Plot residuals
plot(cor.res_1000$correlation, type="b", pch=1, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)                  
points(cor.SEM_1000$correlation, pch=15, cex=1.2)
lines(cor.SEM_1000$correlation, lwd=1.5)
# Statistics - 1 == perfect dispersion
# Statistics = 0 == perfect randomness
# Statistics + 1 == perfect clustering



#### Equation 2: Annonaceae SRic as response variable ####

# Make a regression model to explain "annon_sric_sqrt" including the variables that had a significant 
# effect in the fitted SEM (i.e., "mml_sric_sqrt", "AREA_KM2", "annual_prec")
lm_frugi<-lm(annon_sric_sqrt ~ mml_sric_sqrt + AREA_KM2 + annual_prec, data=annon)
summary(lm_frugi)


##### Spatial structure of residuals ####

# Correlograms with latlon = FALSE
cor.OBL<-correlog(annon$X, annon$Y, z=annon$annon_sric_sqrt, na.rm=T, increment=1, resamp=1, latlon = FALSE)
cor.OBL
plot(cor.OBL$correlation, type="b", pch=16, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)  

# With latlon = TRUE and increment=1
cor.OBL_latlon<-correlog(annon$X, annon$Y, z=annon$annon_sric_sqrt, na.rm=T, increment=1, resamp=1, latlon = TRUE)  #uses km because latlon = TRUE
plot(cor.OBL_latlon$correlation, type="b", pch=16, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)                  

# With latlon = TRUE and increment=1595.997 (this is the minimun distance between cells as calculated below under max(dsts_annon)), resampling 999 times
cor.OBL_1000<-correlog(annon$X, annon$Y, z=annon$annon_sric_sqrt, na.rm=T, increment=1595.997, resamp=999, latlon = TRUE)  #uses km because latlon = TRUE
plot(cor.OBL_1000$correlation, type="b", pch=16, cex=1.2, lwd=1.5,  xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0) 
summary(cor.OBL_1000)
cor.OBL_1000$mean.of.class
cor.OBL_1000$n
cor.OBL_1000$correlation      
cor.OBL_1000$p    

# Correlogram for residuals
cor.res_1000<-correlog(annon$X, annon$Y, z=residuals(lm_frugi), na.rm=T, increment=1595.997, resamp=999, latlon = TRUE)

# Plot both residuals and raw data
plot(cor.OBL_1000$correlation, type="b", pch=16, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)                  
points(cor.res_1000$correlation, pch=1, cex=1.2)
lines(cor.res_1000$correlation, lwd=1.5)


##### Implementing a spatial model ####

# Make coordinate list
coords_annon<-as.matrix(cbind(annon$X,annon$Y))
plot(coords_annon)

# Minimum distance to connect to at least one neighbor
annon_knear <- knn2nb(knearneigh(coords_annon, k=1))
summary(annon_knear)
dsts_annon<-unlist(nbdists(annon_knear, coords_annon, longlat = TRUE))
summary(dsts_annon)
max(dsts_annon)#1595.997

# Neighbour defined by dnearneigh()
annon_nb<-dnearneigh(coords_annon,0,max(dsts_annon), longlat=T)
par(mfrow=c(1,1))
plot(annon_nb, coords_annon, pch=20, lwd=2)
summary(annon_nb)

annon_nb_100<-dnearneigh(coords_annon,0,100, longlat=T)
summary(annon_nb_100)
plot(annon_nb_100, coords_annon, pch=20, lwd=2)

# Defining the spatial weights matrix
nb1_w<-nb2listw(annon_nb, glist=NULL, style="W", zero.policy=TRUE)
summary(nb1_w, zero.policy=TRUE)
plot(nb1_w, coords_annon)


##### Spatial autoregressive error model ####

sem_error_nb1_w<-spatialreg::errorsarlm(lm_frugi, listw=nb1_w, zero.policy=TRUE) #zero.policy=FALSE when taking min distance to connect all cells
summary(sem_error_nb1_w)

# When taking min distance between cells 
sem_error_nb1_w<-spatialreg::errorsarlm(lm_frugi, listw=nb1_w, zero.policy=FALSE) #zero.policy=FALSE when taking min distance to connect all cells
summary(sem_error_nb1_w)


##### Testing for spatial structure in the residuals of the spatial model ####

# Correlogram for spatial model
cor.SEM_1000<-correlog(annon$X, annon$Y, z=residuals(sem_error_nb1_w), na.rm=T, increment=1595.997, resamp=999, latlon = TRUE)

# Plot residuals
plot(cor.res_1000$correlation, type="b", pch=1, cex=1.2, lwd=1.5, ylim=c(-0.5, 1), xlab="Distance class", ylab="Moran's I", cex.lab=1.5, las=1, cex.axis=1.2)
abline(h=0)                  
points(cor.SEM_1000$correlation, pch=15, cex=1.2)
lines(cor.SEM_1000$correlation, lwd=1.5)
# Statistics - 1 == perfect dispersion
# Statistics = 0 == perfect randomness
# Statistics + 1 == perfect clustering
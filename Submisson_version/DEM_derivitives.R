#DEM derivitives calculation
#TRI and TPI

#Packages 
install.packages('terra', repos='https://rspatial.r-universe.dev')
library(tidyverse)
library(stringr)
library(rgdal)
library(terra)
library(terrainr)


#read in the dem
dem <- rast("data/output_COP30_5km_buffer.tif")
set.names(dem, "dem")

#Set up the different focal window sizes 
f_3<- matrix(1, nrow=3, ncol=3)
f_5<- matrix(1, nrow=5, ncol=5)
f_15<- matrix(1, nrow=15, ncol=15)
x <- dem

#calculate TRI at 3 scales
tr_3 <- terra::focal(x, w=f_3, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8)
tr_5 <- terra::focal(x, w=f_5, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8)
tr_15 <- terra::focal(x, w=f_15, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8)

tri_stack <- c(tr_3, tr_5, tr_15)

#set their names
names(tri_stack) <- c("tri_3", "tri_5", "tri_15")


#repeat for TPI
tpi_3 <- terra::focal(x, w=f_3, fun=function(x, ...) x[5] - mean(x[-5]))
tpi_5 <- terra::focal(x, w=f_5, fun=function(x, ...) x[5] - mean(x[-5]))

tpi_stack <- c(tpi_3, tpi_5, tpi_15)
#set their names
names(tpi_stack) <- c("tpi_3", "tpi_5", "tpi_5")


#Terrain roughness
rough_3 <- terra::focal(x, w=f_3, fun=function(x, ...) max(x) - min(x), na.rm=TRUE)
rough_5 <- terra::focal(x, w=f_5, fun=function(x, ...) max(x) - min(x), na.rm=TRUE)
rough_15 <- terra::focal(x, w=f_15, fun=function(x, ...) max(x) - min(x), na.rm=TRUE)

r_stack <- c(rough_3, rough_5, rough_15)
#set their names
names(r_stack) <- c("rough_3", "rough_5", "rough_15")

#Calculate slope and aspect 
slope <-  terrain(x = dem, v = "slope", neighbors = 8)
aspect <- terrain(x = dem, v = "aspect", neighbors = 8)

#import one of the other rasters as a template for reprojection (utm37n, 30m resolution)
y <- rast("data/dist_lakes.tif")

rasterlist <- list(tpi_stack, tri_stack, r_stack, dem, slope, aspect)
names(rasterlist) <- c("tpi_stack", "tri_stack", "r_stack", "dem", "slope", "aspect")

# first approach
utm37n_rasterlist <- lapply(rasterlist, function(x) project(x, y))

#crop them to study area!
study_area <- st_read("data/study_area.gpkg")

cropped_rasts <- lapply(utm37n_rasterlist, function (x) crop(x, study_area, mask = TRUE))

#using lapply didn't work 
lapply(cropped_rasts, function (x) writeRaster(x, filename=paste0("data/",names(x), ".tif")))


#, overwrite=TRUE

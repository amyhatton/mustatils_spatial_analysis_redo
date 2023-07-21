#mustatils analysis

#install relevant libraries
library(spatstat)
library(raster)
library(maptools)
library(rgdal)
library(sf)
library(viridisLite)
library(tmap)
library(tidyverse)
library(tmaptools)
library(here)
library(sp)

#import data
mustatils <- st_transform(st_read("data/mustatils_for_paper_final.gpkg"), crs = 32637)
dem <- raster("data/projected_utm37n/copdem30_5km_straight.tif")
study_area <- st_read("data/projected_utm37n/study_area_straightened_5km.gpkg")





#########################################
### Point pattern analysis with spatstat:
#########################################
#get the centroids for ppa
mustatils$centroids <- st_centroid(mustatils) %>% 
  st_geometry()
c <- st_set_geometry(mustatils, 'centroids')
#p_xy <- st_coordinates(c)
#Create a multitype pattern 
#set study area as boundary for analysis
m_owin <- as.owin(study_area)

#convert data to sp and ppp object
mustatils_sp <- as(c, "Spatial")
library(spbabel)
coords_temp <- spbabel::sptable(mustatils_sp)
mustatils_ppp <- ppp(x = coords_temp$x_, y = coords_temp$y_, window = m_owin)
plot(mustatils_ppp)


#Covariate data

#Geology

#dist to water


#tpi

#tri

#slope

#elevation

################
##KDE
################
#Kernel density plot

##Kernel Density and relative risks
#method to dtermine correct bandwidth - but still need to back it up
bw.diggle(sites_ppp)
#sigma of 30 will be good
par(mar=c(1,1,1,1))
dens_sites <- density(sites_ppp, sigma=20000, edge=TRUE)
plot(dens_sites, main="", border = NA)
plot(sites, add= TRUE, pch=21, col="black",bg="gray60", cex=0.6)
d.sites <- density.ppp(sites_ppp) # Overall density of all points in our data
tmap_mode("view")
tm_shape(borders)+
  tm_borders()+
  tm_shape(sites)+
  tm_symbols(size = 0.01)
plot(d.sites, main = "Density of all sites") # Plot interpolated values
plot(sites_ppp, add = TRUE, cex = 0.5)
st_transform(sites, wgs84_latlong)
st_write(sites, "data/sites.kml")


### Elevation Plots:

#first create a hillshade map to show elevation across the landscape

hillshade <- raster("data/rsa_proj_maps/coh/coh_buffer_hillshade.tif")
r_sites <- cave_sites %>% 
  filter(site_category != "random", drop = TRUE) 
r_sites$site_category <- as.factor(r_sites$site_category)
tmap_mode("plot")
tm_shape(hillshade)+
  tm_raster(breaks = c(seq(0, 1, 0.1)),
            palette = gray(0:10 / 10),
            style = 'cont', legend.show = FALSE)+
  tm_shape(elevation)+
  tm_raster(style = "quantile", n = 12,
            title = "Elevation (m)",
            palette = "viridis", alpha = 0.7,
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)+
  tm_shape(coh_poly)+
  tm_borders(col = "black")+
  tm_shape(r_sites)+
  tm_dots(shape = "site_category",shapes = c(1, 3, 2), col = "black",
          size = 0.2)

elev_rh_all <- rhohat(all_caves, elev, confidence=0.95)
plot(elev_rh_all, main="All Caves ", xlab="Elevation (m)", ylab="")
elev_rh_macro <- rhohat(macro_caves, elev, confidence=0.95)
plot(elev_rh_macro, main="Macro Fossil Caves ", xlab="Elevation (m)", ylab="")
elev_rh_hom <- rhohat(hom_caves, elev, confidence=0.95)
plot(elev_rh_hom, main="Hominin Caves ", xlab="Elevation (m)", ylab="")

### Slope: 

tmap_mode("plot")
tm_shape(hillshade)+
  tm_raster(breaks = c(seq(0, 1, 0.1)),
            palette = gray(0:10 / 10),
            style = 'cont', legend.show = FALSE)+
  tm_shape(slope)+
  tm_raster(style = "quantile", n = 5,
            title = "Slope (°)",
            palette = "viridis", alpha = 0.7,
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)+
  tm_shape(coh_poly)+
  tm_borders(col = "black")+
  tm_shape(r_sites)+
  tm_dots(shape = "site_category",shapes = c(1, 3, 2), col = "black",
          size = 0.2)
slope_rh_all <- rhohat(all_caves, slope_im, confidence=0.95)
plot(slope_rh_all, main="All Caves ", xlab="Slope (°)", ylab="")
slope_rh_macro <- rhohat(macro_caves, slope_im, confidence=0.95)
plot(slope_rh_macro, main="Macro Fossil Caves ", xlab="Slope (°)", ylab="")
slope_rh_hom <- rhohat(hom_caves, slope_im, confidence=0.95)
plot(slope_rh_hom, main="Hominin Caves ", xlab="Slope (°)", ylab="")


### Aspect:

tmap_mode("plot")
tm_shape(hillshade)+
  tm_raster(breaks = c(seq(0, 1, 0.1)),
            palette = gray(0:10 / 10),
            style = 'cont', legend.show = FALSE)+
  tm_shape(aspect)+
  tm_raster(style = "quantile", n = 5,
            title = "Aspect (°)",
            palette = "viridis", alpha = 0.7,
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)+
  tm_shape(coh_poly)+
  tm_borders(col = "black")+
  tm_shape(r_sites)+
  tm_dots(shape = "site_category",shapes = c(1, 3, 2), col = "black",
          size = 0.2)
aspect_rh_all <- rhohat(all_caves, aspect_im, confidence=0.95)
plot(aspect_rh_all, main="All Caves ", xlab="Aspect (°)", ylab="")
aspect_rh_macro <- rhohat(macro_caves, aspect_im, confidence=0.95)
plot(aspect_rh_macro, main="Macro Fossil Caves ", xlab="Aspect (°)", ylab="")
aspect_rh_hom <- rhohat(hom_caves, aspect_im, confidence=0.95)
plot(aspect_rh_hom, main="Hominin Caves ", xlab="Aspect (°)", ylab="")


### Distance from streams 500 threshold:

tmap_mode("plot")
tm_shape(hillshade)+
  tm_raster(breaks = c(seq(0, 1, 0.1)),
            palette = gray(0:10 / 10),
            style = 'cont', legend.show = FALSE)+
  tm_shape(dist_500)+
  tm_raster(style = "quantile", n = 12,
            title = "Distance from Streams (m)",
            palette = "viridis", alpha = 0.7,
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)+
  tm_shape(coh_poly)+
  tm_borders(col = "black")+
  tm_shape(r_sites)+
  tm_dots(shape = "site_category",shapes = c(1, 3, 2), col = "black",
          size = 0.2)

dist_500_rh_all <- rhohat(all_caves, dist_500_im, confidence=0.95)
plot(dist_500_rh_all, main="All Caves ", xlab="Distance from streams (m)", ylab="")
dist_500_rh_macro <- rhohat(macro_caves, dist_500_im, confidence=0.95)
plot(dist_500_rh_macro, main="Macro Fossil Caves ", xlab="Distance from streams (m)", ylab="")
dist_500_rh_hom <- rhohat(hom_caves, dist_500_im, confidence=0.95)
plot(dist_500_rh_hom, main="Hominin Caves ", xlab="Distance from streams (m)", ylab="")



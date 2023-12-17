## Univariate models to narrow down the useful variables
## You need to have already run Hatton_et_al_mustatils_spatial_analysis.Rmd so that you have all of the necessary variables loaded.


#create some nonsite points
non_site_area <- st_difference(study_area, st_union(mustatils))

# create random points in non-site area
#Create 5 * number of mustatils
t <- st_as_sf(st_sample(non_site_area, size = 5*169, type = "random"))
t <- st_transform(t, crs=32637)
non_sites <- t %>% 
  mutate(value = 0) %>% 
  dplyr::select(value, x)

#create the mustatil points (alternative to centroids below)
#generate 5 random point within each mustatil polygon to get better idea
m_points <- st_sample(mustatils, size = c(5,5), type = "random")  
m_points <- st_as_sf(m_points)

#extract values for mustatils
#m_points <- st_centroid(mustatils)
sites <- m_points %>% 
  dplyr::mutate(value = 1) %>% 
  dplyr::select(value)

#merge sites and non sites
all_sites <- rbind(sites, non_sites)
all_sites_v <- vect(all_sites)

locvals_all_sites <- data.frame(raster::extract(covariates, all_sites))
locvals_all_sites$site <- as.numeric(as.character(all_sites$value))

## Create univariate models to check predictive power
library(lmtest)
#slope
slope.m <- glm(site~slope.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(slope.m)
lrtest(slope.m) #very good lrt pvalue of 3.162e-06 *** ; standardised pvalue 0.0005068 ***

#aspect
aspect.m <- glm(site~aspect.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(aspect.m)
lrtest(aspect.m) #very good pvalue < 2.2e-16 ***; standardised pvalue 

#eastness 
eastness.m <- glm(site~eastness, data = locvals_all_sites, family=binomial(logit))
summary(eastness.m)
lrtest(eastness.m) #very good pvalue 2.2e-16 *** ; standardised pvalue 2.2e-16 ***

#northness
northness.m <- glm(site~northness, data = locvals_all_sites, family=binomial(logit))
summary(northness.m)
lrtest(northness.m) #bad p value 0.09371; standardised pvalue 

#tpi
tpi3.m <- glm(site~tpi_3, data = locvals_all_sites, family=binomial(logit))
summary(tpi3.m)
lrtest(tpi3.m) #very good pvalue 2.2e-16 ***; standardised pvalue < 2.2e-16 ***

#tpi
tpi5.m <- glm(site~tpi_5, data = locvals_all_sites, family=binomial(logit))
summary(tpi5.m)
lrtest(tpi5.m) #very good p-value 2.4e-12 ***; standardised pvalue < 2.2e-16 ***

#tpi
tpi15.m <- glm(site~tpi_15, data = locvals_all_sites, family=binomial(logit))
summary(tpi15.m)
lrtest(tpi15.m) #bad p-value 0.2751; standardised pvalue 

#tri
tri3.m <- glm(site~tri_3, data = locvals_all_sites, family=binomial(logit))
summary(tri3.m)
lrtest(tri3.m) #very good pvalue 3.648e-07 ***; standardised pvalue 3.294e-05 ***

#tri
tri5.m <- glm(site~tri_5, data = locvals_all_sites, family=binomial(logit))
summary(tri5.m)
lrtest(tri5.m) #very good p-value 9.357e-10 ***; standardised pvalue 2.119e-08 ***

#tri
tri15.m <- glm(site~tri_15, data = locvals_all_sites, family=binomial(logit))
summary(tri15.m)
lrtest(tri15.m) #very good p-value 3.598e-05 ***; standardised pvalue 

rough3.m <- glm(site~rough_3, data = locvals_all_sites, family=binomial(logit))
summary(rough3.m)
lrtest(rough3.m) #very good p-value 2.209e-07 ***; standardised pvalue 

rough5.m <- glm(site~rough_5, data = locvals_all_sites, family=binomial(logit))
summary(rough5.m)
lrtest(rough5.m) #very good p-value 2.167e-10 ***; standardised pvalue 

rough15.m <- glm(site~rough_15, data = locvals_all_sites, family=binomial(logit))
summary(rough15.m)
lrtest(rough15.m) #very good p-value 5.026e-09 ***; standardised pvalue 

#dist_lakes
dist_lakes.m <- glm(site~dist_lakes.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(dist_lakes.m)
lrtest(dist_lakes.m) #not good pvalue 0.8734; standardised pvalue 

#dem
dem.m <- glm(site~dem, data = locvals_all_sites, family=binomial(logit))
summary(dem.m)
lrtest(dem.m) # very good pvalue 2.628e-06 ***; standardised pvalue 

#dist_rivers
dist_rivers.m <- glm(site~dist_rivers.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(dist_rivers.m)
lrtest(dist_rivers.m) #very good pvalue 2.2e-16 ***; standardised pvalue 

#dist_deposits
dist_deposits.m <- glm(site~dist_deposits.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(dist_deposits.m)
lrtest(dist_deposits.m) #bad pvalue 0.6701; standardised pvalue 

#dist_lakes_rivers 
dist_lakes_rivers.m <- glm(site~dist_lakes_rivers.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(dist_lakes_rivers.m)
lrtest(dist_lakes_rivers.m) #very good pvalue 2.91e-12 ***; standardised pvalue 

#dist_deposits_rivers
dist_deposits_rivers.m <- glm(site~dist_deposits_rivers.PERMANENT, data = locvals_all_sites, family=binomial(logit))
summary(dist_deposits_rivers.m)
lrtest(dist_deposits_rivers.m) #very good pvalue 4.922e-13 ***; standardised pvalue 

#dist_sandstone_geo 
dist_sandstone_geo.m <- glm(site~dist_sandstone_geo , data = locvals_all_sites, family=binomial(logit))
summary(dist_sandstone_geo.m)
lrtest(dist_sandstone_geo.m) #very good pvalue 2.2e-16 ***; standardised pvalue 

#dist_non_quart_geo 
dist_non_quart_geo.m <- glm(site~dist_non_quart_geo , data = locvals_all_sites, family=binomial(logit))
summary(dist_non_quart_geo.m)
lrtest(dist_non_quart_geo.m) #very very good 2.2e-16 ***; standardised pvalue 

#viewshed
viewshed.m <- glm(site~t_vshed_whole_scaled, data=locvals_all_sites, family=binomial(logit))
summary(viewshed.m)
lrtest(viewshed.m) #very good pvalue < 2.2e-16 ***; standardised pvalue 

#dist to pekel water
pekel.m <- glm(site~dist_pekel_water, data=locvals_all_sites, family=binomial(logit))
summary(pekel.m)
lrtest(pekel.m) #very good pvalue < 2.2e-16 ***; standardised pvalue 
```

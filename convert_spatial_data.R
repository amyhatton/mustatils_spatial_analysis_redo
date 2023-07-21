# Import various spatial formats and convert to gpkg.

#KML for Hugh Thomas
library(sf)
aaksa_mustatils <- sf::st_read( "data/AAKSA_mustatils.kml")
sf::st_write(aaksa_mustatils, "data/AAKSA_mustatils.gpkg")

#create regular grid for survey of southeast and southwest areas
#convert to Saudi projection KSA-GRF17 epsg 9331
study_area <- sf::st_read("data/study_area.gpkg")
study_area <- sf::st_transform(study_area, crs=32637)
plot(study_area)
sf::st_length(study_area)
survey_grid <- sf::st_make_grid(x = study_area, cellsize = c(0.01, 0.01))
plot(survey_grid)
sf::st_write(survey_grid, "data/survey_grid.gpkg", append = FALSE)

#add buffer of 5km for viewshed analysis
study_area_buff <- st_buffer(study_area, 5000, endCapStyle = "SQUARE")
# add bigger buffer so I can crop to a rectangle shape later (distorted from changing projections)
study_area_buff20 <- 
study_area_buff <- st_transform(study_area_buff, crs=4326)
st_bbox(study_area_buff)
#get elevation data fro opentopo
sf::st_crs(study_area)
st_bbox(study_area)
library(elevatr)
library(rgdal)
dem <- get_elev_raster(locations = study_area, src = "gl1", clip = "locations", neg_to_na = TRUE)

mustatil_polys <- st_read("data/mustatil_polygons.gpkg")
plot(dem)
plot(mustatil_polys, add = TRUE)

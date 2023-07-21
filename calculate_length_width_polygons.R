#found this adapted version from https://stackoverflow.com/questions/67171573/is-there-a-way-to-determine-the-major-orientation-of-polygon-in-sf

# Copied function getMinBBox()
# from https://github.com/caiohamamura/flightplanning-R/blob/master/R/utils.R
# credit there given to: Daniel Wollschlaeger <https://github.com/ramnathv>


library(tidyverse)
library(sf)
library(sfheaders)

mustatils_df <- st_transform(st_read("data/mustatils_for_paper_final.gpkg"), crs = 32637) 

mustatils_df <- mustatils_df %>% 
  dplyr::select(-"id", -"id.code") %>% 
 tibble::rowid_to_column( "ID")

mustatils <-  mustatils_df %>%
  st_geometry() %>% st_as_sf()


getMinBBox <- function(x) {
  stopifnot(is.matrix(x), is.numeric(x), nrow(x) >= 2, ncol(x) == 2)
  
  ## rotating calipers algorithm using the convex hull
  H    <- grDevices::chull(x)      ## hull indices, vertices ordered clockwise
  n    <- length(H)      ## number of hull vertices
  hull <- x[H, ]        ## hull vertices
  
  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir  <- diff(rbind(hull, hull[1, ])) ## hull vertices are circular
  hLens <- sqrt(rowSums(hDir^2))        ## length of basis vectors
  huDir <- diag(1/hLens) %*% hDir       ## scaled to unit length
  
  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[ , 2], huDir[ , 1])
  
  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- rbind(huDir, ouDir) %*% t(hull)
  
  ## range of projections and corresponding width/height of bounding rectangle
  rangeH  <- matrix(numeric(n*2), ncol=2)  ## hull edge
  rangeO  <- matrix(numeric(n*2), ncol=2)  ## orthogonal subspace
  widths  <- numeric(n)
  heights <- numeric(n)
  
  for(i in seq(along=numeric(n))) {
    rangeH[i, ] <- range(projMat[  i, ])
    
    ## the orthogonal subspace is in the 2nd half of the matrix
    rangeO[i, ] <- range(projMat[n+i, ])
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }
  
  ## extreme projections for min-area rect in subspace coordinates
  ## hull edge leading to minimum-area
  eMin  <- which.min(widths*heights)
  hProj <- rbind(   rangeH[eMin, ], 0)
  oProj <- rbind(0, rangeO[eMin, ])
  
  ## move projections to rectangle corners
  hPts <- sweep(hProj, 1, oProj[ , 1], "+")
  oPts <- sweep(hProj, 1, oProj[ , 2], "+")
  
  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  ## basis formed by hull edge and orthogonal subspace
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))
  
  ## angle of longer edge pointing up
  dPts <- diff(pts)
  e    <- dPts[which.max(rowSums(dPts^2)), ] ## one of the longer edges
  eUp  <- e * sign(e[2])       ## rotate upwards 180 deg if necessary
  deg  <- atan2(eUp[2], eUp[1])*180 / pi     ## angle in degrees
  
  return(list(pts=pts, width=heights[eMin], height=widths[eMin], angle=deg))
}

##############
## Use getMinBBox in a custom function to return an sf object
##############

##Need to edit this to keep width and length 

min_box_sf <- function(x){
  crs <- st_crs(x)
  x_as_matrix <- st_coordinates(x)[,1:2]
  min_box <- getMinBBox(x_as_matrix)
  box <- sfheaders::sf_polygon(min_box$pts) %>%
    st_set_crs(crs)
  box$angle <- min_box$angle
  box
  
}

min_box_sf <- function(x){
  crs <- st_crs(x)
  x_as_matrix <- st_coordinates(x)[,1:2]
  min_box <- getMinBBox(x_as_matrix)
  box <- sfheaders::sf_polygon(min_box$pts) %>%
    st_set_crs(crs)
  box$angle <- min_box$angle
  box$width_x <- min_box$width
  box$length_x <- min_box$height
  box
  }


#Plotting mustatil 1 & the associated minimum bounding box
ggplot() + 
  geom_sf(data = mustatils[17,], 
          fill = 'red', 
          alpha = .2) + 
  geom_sf(data = min_box_sf(mustatils[17,]), 
          fill = NA) 

# Using the function on each row of an sf object.
#  note the crs is not retained.
test <- pmap_dfr(mustatils, min_box_sf)

#width and height is not same as width and length (where length is always larger than width)
#try to reorder data so that we have length and width based on the rectangle, not based on the xy axis 
mustatils_minbbox <- test %>% 
  mutate(length = if_else(width_x > length_x, width_x, length_x)) %>% 
  mutate(width = if_else(width_x > length_x, length_x, width_x)) %>% 
  dplyr::select(-width_x, -length_x)

#add the metrics we calculated as variable to mustatils df
library(tidyverse)
st_crs(mustatils_minbbox) <- st_crs(mustatils_df)
mustatils_minbbox_df <- mustatils_minbbox %>%  dplyr::select(-"id") %>% 
  st_drop_geometry(mustatils_minbbox)  %>% 
  tibble::rowid_to_column( "ID")

  
mustatils_df2 <- left_join(mustatils_df, mustatils_minbbox_df)

#export data
st_write(mustatils_df2, "data/projected_utm37n/mustatils_with_size_metrics.gpkg", append=FALSE)


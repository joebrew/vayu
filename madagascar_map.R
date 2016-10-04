library(raster)

thinnedSpatialPoly <- function(SP, tolerance, minarea) {
  if (!require(shapefiles)) stop("shapefiles package is required")
  stopifnot(inherits(SP, "SpatialPolygons"))
  
  # TODO: determine and set defaults for tolerance, minarea 
  # TODO: suppress warnings: "In Polygon(crds_s) : Non-finite label 
  # point detected and replaced"
  pls <- slot(SP, "polygons")
  pls_dp <- vector(mode="list", length=length(pls))
  for (i in 1:length(pls)) {
  Pls <- slot(pls[[i]], "Polygons")
  Pls_dp <- vector(mode="list", length=length(Pls))
  for (j in 1:length(Pls)) {
  crds <- slot(Pls[[j]], "coords")
  crds_s <- dp(list(x=crds[,1], y=crds[,2]), tolerance=tolerance)
  crds_s <- do.call("cbind", crds_s)
  if(!identical(crds_s[1,], crds_s[nrow(crds_s),]))
  crds_s <- rbind(crds_s, crds_s[1,])
  Pls_dp[[j]] <- Polygon(crds_s)
  }
  Keep <- logical(length(Pls_dp))
  for (j in 1:length(Pls_dp)) {
  Keep[j] <- TRUE
  if (slot(Pls_dp[[j]], "area") < minarea) Keep[j] <- FALSE
  }
  Pls_dp <- Pls_dp[Keep]
  pls_dp[[i]] <- Polygons(Pls_dp, ID=slot(pls[[i]], "ID"))
  }
  SP_dp <- SpatialPolygons(pls_dp, proj4string=slot(SP, "proj4string"))
  if(inherits(SP, "SpatialPolygonsDataFrame")) {
  data <- slot(SP, "data")
  SP_dp <- SpatialPolygonsDataFrame(SP_dp, data=data)
  }
  SP_dp
}
mad <- getData('GADM', country = 'MDG', level = 3)
mad_small <- thinnedSpatialPoly(mad, tolerance = 0.2, minarea=0)
plot(mad_small,
     fill = TRUE,
     col = adjustcolor('darkorange', alpha.f = 0.7),
     border = 'darkgreen')


# Get locations
library(rgdal)
tkml <- getKMLcoordinates(kmlfile="/home/joebrew/Documents/vayu/16_09_28 locations.kmz_FILES/doc.kml", ignoreAltitude=T)


results_list <- list()
for (i in 1:length(tkml)){
  x <- tkml[[i]]
  if(is.null(dim(x))){
    # points(x = x[1],
    #        y = x[2])  
    this_row <- data.frame(number = i,
                           x = x[1],
                           y = x[2],
                           multiple = FALSE)
  } else {
    # points(x = x[,1],
    #        y = x[,2],
    #        pch = '.')
    this_row <- data.frame(number = i,
                           x = x[,1],
                           y = x[,2],
                           multiple = TRUE)
  }
  results_list[[i]] <- this_row
}
places <- do.call('rbind', results_list)
# Keep only singular ones
places <- places[!places$multiple,]

places$lng <- places$x
places$lat <- places$y
coordinates(places) <- ~x+y
proj4string(places) <- proj4string(mad)


plot(mad_small,
     fill = TRUE,
     col = adjustcolor('darkorange', alpha.f = 0.7),
     border = 'darkgreen')

# Extract only polygon of interest
mad_tiny <- mad[sort(unique(over(places, polygons(mad)))),]
plot(mad_tiny, col = 'darkred', add = TRUE)

plot(mad_tiny,
     fill = TRUE,
     col = adjustcolor('darkorange', alpha.f = 0.7),
     border = 'darkgreen',
     xlim = c(min(places$lng),
          max(places$lng)),
     ylim = c(min(places$lat),
              max(places$lat)))
points(places,
       pch = 16,
       # cex = 0.5,
       col = adjustcolor('darkred', alpha.f = 0.6))

#' Extract values at a set of point locations
#'
#' Returns the value of a raster at a set of points.
#'
#' @param rasterFilename A string identifying the complete path and filename of a GDAL-readable raster
#' @param points_sf A 'sf' object containing the points at which zone calculations will be made
#'
#' @return rasterValues A scalar or array of raster values associated with the user-supplied points
#' @export
#' @importFrom magrittr %>%
#' @examples
#' # read in the precipitation raster and the basin outline shapefile
#' myRasterFile <- system.file("extdata",
#'                             "PRISM_ppt_stable_4kmM2_190004_bil.bil",
#'                              package="mapRandomForest")
#' myShapeFile <- system.file("extdata",
#'                            "drainage_area_polygons.shp",
#'                             package = "mapRandomForest")
#' myRaster <- velox::velox(myRasterFile)
#' myShapes <- sf::st_read(myShapeFile)
#' myCentroids <- sf::st_centroid(myShapes)
#'
#' # reproject the shapefile to lat/lon and extract centroid values
#' myShapes_latlon <- sf::st_transform(myCentroids, crs=myRaster@crs)
#'
#' # calculate the mean raster values associated with each polygon in shapefile
#' precip_mm <- getRasterMean(myRaster, myShapes_latlon)
#'
#' # tack the mean precipitation values onto the data table of the shapefile
#' myShapes@data <- cbind(myShapes@data,precip_mm)
getRasterPointVals <- function(rasterFilename, points_sf ) {
  #rasterMean <- raster::extract(rasterFile, shapeFile, fun = mean)
  veloxRaster <- velox::velox(rasterFilename)
  rasterValues <- veloxRaster$extract_points(sp=points_sf)
  return(rasterValues)
}

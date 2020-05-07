#' Calculate mean raster value for a polygon feature
#'
#' Returns the mean value of raster pixels that fall within a
#' simple shapefile boundary.
#'
#' @param rasterFilename A string identifying the complete path and filename of a GDAL-readable raster
#' @param shapeFile A shapefile object of class 'sf' containing the polygons for which zone calculations will be made
#'
#' @return rasterMean A scalar or array of raster mean values associated with the polygon areas
#' @export
#' @importFrom magrittr %>%
#' @examples
#' # read in the precipitation raster and the basin outline shapefile
#' myRasterFile <- system.file("extdata",
#'                             "PRISM_ppt_stable_4kmM2_190004_bil.bil",
#'                              package="mapRandomForest")
#' myShapeFile <- system.file("extdata",
#'                            "ungagedSheds.shp",
#'                             package = "mapRandomForest")
#' myRaster <- velox::velox(myRasterFile)
#' myShapes <- sf::st_read(myShapeFile)
#'
#' # reproject the shapefile to lat/lon and extract centroid values
#' myShapes_latlon <- sf::st_transform(myShapes, crs=myRaster@crs)
#'
#' # calculate the mean raster values associated with each polygon in shapefile
#' precip_mm <- getRasterMean(myRaster, myShapes_latlon)
#'
#' # tack the mean precipitation values onto the data table of the shapefile
#' myShapes@data <- cbind(myShapes@data,precip_mm)
getRasterMean <- function(rasterFilename, shapeFile ) {
  #rasterMean <- raster::extract(rasterFile, shapeFile, fun = mean)
  veloxRaster <- velox::velox(rasterFilename)
  rasterMean <- veloxRaster$extract(shapeFile, fun = function(x) mean(x, na.omit=TRUE), small=TRUE)
  return(rasterMean)
}

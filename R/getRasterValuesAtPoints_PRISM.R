#' Extract monthly mean PRISM value at a point for a given month and year
#'
#' @param shp_sf Single point feature in the form of an R "Simple Features" (sf) object
#' @param month Month value as an integer (1-12)
#' @param year Year value as an integer
#' @param dataname PRISM file dataname (for example, "ppt", "tmax", "tmean", or "tmin")
#' @param path_to_PRISM_data Fully qualified path to the local directory that contains PRISM monthly data
#'
#' @return Mean value of PRISM raster for the given month and year
#' @export
#'
#' @examples
getRasterValuesAtPoints_PRISM <- function(shp_sf, month, year, dataname='ppt', path_to_PRISM_data) {

  path <- paste(path_to_PRISM_data,'/',dataname,'/',sep="")
  raster_filename <- list.files(path,
                                pattern=utils::glob2rx(paste("*",dataname,"*stable*",as.character(year),
                                                             as.character(month),"*.bil",sep="")),
                                full.names=TRUE)

  # crude error trapping in the event that a particular PRISM image file cannot be found
  if (length(raster_filename) > 0 ) {
    if (file.exists( raster_filename) ) {
      value <- getRasterPointVals(raster_filename, shp_sf)
    } else {
      write(file="logfile.txt", append=TRUE, paste("**ERROR** - file doesn't exist:", raster_filename))
      value <- NA
    }
  } else {
    write(file="logfile.txt", append=TRUE, paste("**ERROR** - file not found:", raster_filename))
    value <- NA
  }

  return(as.numeric(value))

}

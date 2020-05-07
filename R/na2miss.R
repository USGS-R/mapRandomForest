#' na2miss
#'
#' Converts NA values to a numerical missing value code.
#'
#' This function is adapted from the USGS smwrBase package. It is
#' included here to enable the dvpart function to work without
#' installing the dozens of external dependencies required by
#' the smwrBase package.
#' @param x
#' @param to
#'
#' @return x
#' @export
na2miss <- function(x, to=-99999) {
  x[is.na(x)] <- to
  return(x)
}

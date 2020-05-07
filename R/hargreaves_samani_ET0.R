#' Calculate reference ET0 by means of the Hargreaves-Samani relation
#'
#' @param tmin Minimum air temperature in degrees C
#' @param tmax Maximum air temperature in degrees C
#' @param day_of_year Julian Day (day-of-year, January 1=1)
#' @param days_in_month Number of days in month associated with Julian Day
#' @param precipitation Precipitation in millimeters
#' @param lat Latitude in decimal degrees
#'
#' @return ET0 The reference ET0 value in millimeters per month
#' @export
#'
#' @examples
hargreavesET0 <- function (tmin, tmax, day_of_year, days_in_month, precipitation, lat) {
  ET0 <- tmin * NA
  tmean <- (tmin + tmax)/2
  tdelta <- tmax - tmin
  tdelta <- ifelse(tdelta < 0, 0, tdelta)
  J <- day_of_year
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  Dr <- 1 + 0.033 * cos(0.0172 * J)
  lat_radians <- lat/57.2957795
  sset <- -tan(lat_radians) * tan(delta)
  omegas <- sset * 0
  omegas[sset >= {
    -1
  } & sset <= 1] <- acos(sset[sset >= {
    -1
  } & sset <= 1])
  omegas[sset < {
    -1
  }] <- max(omegas)
  Ra <- 37.6 * Dr * (omegas * sin(lat_radians) * sin(delta) + cos(lat_radians) * cos(delta) * sin(omegas))
  Ra <- ifelse(Ra < 0, 0, Ra)
  ab <- tdelta - 0.0123 * precipitation
  ab <- ifelse( ab < 0, 0, ab)
  ET0 <- 0.0013 * 0.408 * Ra * (tmean + 17.8) * ab^0.76
  ET0[is.nan(ab^0.76)] <- 0
  ET0 <- ifelse(ET0 < 0, 0, ET0)
  ET0 <- ET0*days_in_month
  return(ET0)
}

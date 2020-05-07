#'Estimate total streamflow and baseflow by means of a random forest model.
#'
#'This package code and data used to generate estimates of total and baseflow
#'for surface waters within the Mississippi Embayment Regional Aquifer System
#'groundwater flow model boundary (Clark and Hart, 2009). This code is heavily adapted
#'from earlier unpublished work by Brian Breaker, formerly of the U.S. Geological
#'Survey. Additional code functions have been adapted from work by other current or
#'former USGS employees including Al Rutledge and Dave Lorenz.
#'
#' @name mapRandomForest-package
#' @aliases mapRandomForest-package mapRandomForest
#' @docType package
#' @author Steve Westenbroek
#'
#' @references Clark, B.R., and Hart, R.M., 2009, The Mississippi Embayment Regional Aquifer
#'             Study (MERAS): Documentation of a Groundwater-Flow Model Constructed to Assess
#'             Water Availability in the Mississippi Embayment: U. S. Geological Survey 2009–5172, 61 p.
#' @keywords package
NULL
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and
is subject to revision. It is being provided to meet
the need for timely best science. The information
has not received final approval by the U.S. Geological
Survey (USGS) and is provided on the condition that
neither the USGS nor the U.S. Government shall be held
liable for any damages resulting from the authorized
or unauthorized use of the information.")
}

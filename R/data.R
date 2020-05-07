#' @title [DATASET] USGS gage contributing areas
#' @description Shapefile of contributing areas associated with USGS gages
#' @format A shapefile and data frame with 316 rows and 3 variables.
#' \describe{
#'   \item{\code{site_no}}{character USGS gaging station ID}
#'   \item{\code{Area}}{double Contributing area in square miles}
#'}
#'
#' Attributes of this shapefile include:
#'
#' \describe{
#'   \item{\code{class}}{SpatialPolygonsDataFrame}
#'   \item{\code{features}}{316}
#'   \item{\code{extent}}{-36585, 970995, 791895, 1741095  (xmin, xmax, ymin, ymax)}
#'   \item{\code{crs}}{+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0}
#'   \item{\code{variables}}{2}
#'   \item{\code{names}}{site_no, Area}
#'   \item{\code{min values}}{02374250, 2.03630278442}
#'   \item{\code{max values}}{08041500, 18426.5671397}
#'}
#' @name sitesAll.shp
#' @usage raster::shapefile("data/sitesAll.shp")
#' @source \url{https://water.usgs.gov/lookup/getspatial?gagesII_Sept2011}
#' @examples
#' myShapeFile <- system.file("data", "sitesAll.shp", package = "mapRandomForest")
#'
#' myBasins <- raster::shapefile(myShapeFile)
#' plot(myBasins)
NULL

#' [DATASET] ORIGINAL monthly streamflow and baseflow summaries
#'
#' This dataset contains monthly streamflow and baseflow summaries for a list of USGS gage IDs pulled
#' from a shapefile of gage drainage basins. The dataset contains almost 21,300 missing or negative
#'  flow values, which were removed in subsequent processing in the original scripts.
#'
#' This dataset also includes results for additional gage IDs that were removed in the original scripts
#' before training the random forest trees. The sites removed in subsequent processing were:
#'
#'   "03320500","03383000","03434500","03436100","03438000","03438220",
#'   "03588000","03601990","03602500","03603000","03604400","03605555",
#'   "03436690","03436700","03584000","03584020","03588400","03588500",
#'   "03599450","03600500","03601630","03604000","03433641"
#'
#'   The variables are as follows:
#'
#' \itemize{
#'   \item siteNo. USGS gage ID
#'   \item Date. Year and month associated with summary statistic (YYYY-MM)
#'   \item Flow. Monthly mean streamflow in cubic feet per second
#'   \item baseFlow. Stream baseflow as estimated by the 'part' method of baseflow separation, in cubic feet per second
#'   \item isSubbed. Internal variable used by the original scripts to note whether a value is observed or imputed ('real' or 'fake')
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dvsMonthly.rds
#' @usage readr::read_rds("data/dvsMonthly.rds")
#' @format A file containing 180989 observations of 5 variables
NULL

#' [DATASET] Unedited ORIGINAL random forest model inputs
#'
#' This dataset was used as raw input to the random forest model scripts. The dataset combines the output
#' from several scripts, joining data flow and baseflow summaries with precipitation, air temperature,
#' location, elevation, and generalized geologic units.
#'
#' In later processing steps, the fields pertaining to the digital elevation map statistics and to
#' basin geology were dropped from the list of random forest model inputs.
#'
#' \itemize{
#' \item siteNo. USGS gage ID
#' \item Date. Date of discharge, precipitation, and temperature observation
#' \item Flow. Total daily average discharge, in cubic feet per second
#' \item baseFlow. Estimated daily average baseflow calculated by means of the HYSEP algorithm, cubic feet per second
#' \item precip. Daily average zonal precipitation calculated for the drainage basin associated with the gage, in millimeters
#' \item tmax. Daily average zonal maximum air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item tmean. Daily average zonal mean air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item tmin. Daily average zonal minimum air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item X. Longitude of gaging basin centroid, decimal degrees
#' \item Y. Latitude of gaging basin centroid, decimal degrees
#' \item ET0Har. Reference evapotranspiration calculated by means of the Hargreaves-Samani relation, in millimeters
#' \item preSub1. Value of `precip` from previous month, in millimeters
#' \item preSub2. Value of `precip` from two months prior to current month, in millimeters
#' \item tmaxSub1. Value of `tmax` from previous month, in degrees Celcius
#' \item tmaxSub2. Value of `tmax` from two months prior to current month, in degrees Celcius
#' \item tmeanSub1. Value of `tmean` from previous month, in degrees Celcius
#' \item tmeanSub2. Value of `tmean` from two months prior to current month, in degrees Celcius
#' \item tminSub1. Value of `tmin` from previous month, in degrees Celcius
#' \item tminSub2. Value of `tmin` from two months prior to current month, in degrees Celcius
#' \item ET0Sub1. Value of `ET0Har` from previous month, in millimeters
#' \item ET0Sub2. Value of `ET0Har` from two months prior to current month, in millimeters
#' \item preTot6. Value of `precip` summed over previous 6 months, in millimeters
#' \item MIN. Minimum elevation of the gaging basin, in meters
#' \item MAX. Maximum elevation of the gaging basin, in meters
#' \item RANGE. Range of elevation of the gaging basin, in meters
#' \item MEAN. Mean elevation of the gaging basin, in meters
#' \item STD. Standard deviation of the elevation of the gaging basin, in meters
#' \item H2o. Percentage of gaging basin classified as open water
#' \item Q. Percentage of gaging basin having geology classified as Quaternary
#' \item pgT. Percentage of gaging basin having geology classified as Paleogene
#' \item K. Percentage of gaging basin having geology classified as Cretaceous
#' \item lPz. Percentage of gaging basin having geology classified as Lower Paleozoic
#' \item nT. Percentage of gaging basin having geology classified as Neogene
#' \item Yv. Percentage of gaging basin having geology classified as Middle Proterozoic
#' \item mPz. Percentage of gaging basin having geology classified as Middle Paleozoic
#' \item uPz. Percentage of gaging basin having geology classified as Upper Paleozoic
#' \item Yg. Percentage of gaging basin having geology classified as Middle Proterozoic
#' \item ZPz. Percentage of gaging basin having geology classified as Late Proterozoic
#' \item drain_area_va. Drainage area of the drainage basin associated with the stream gage, in square miles
#' }
#'
#' @docType data
#' @keywords datasets
#' @name testFlowDat.rds
#' @usage readr::read_rds("data/testFlowDat.rds")
#' @format A file containing 158526 observations of 39 variables
NULL

#' @title [DATASET] Predictor variables for gaged basins
#' @description Original predictor variable dataset for the drainage basins associated with USGS gages.
#' @format A data frame with 493584 rows and 37 variables:
#' \itemize{
#' \item siteNo. USGS gage ID
#' \item Date. Date of discharge, precipitation, and temperature observation
#' \item Flow. Total daily average discharge, in cubic feet per second
#' \item baseFlow. Estimated daily average baseflow calculated by means of the HYSEP algorithm, cubic feet per second
#' \item precip. Daily average zonal precipitation calculated for the drainage basin associated with the gage, in millimeters
#' \item tmax. Daily average zonal maximum air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item tmean. Daily average zonal mean air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item tmin. Daily average zonal minimum air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item X. Longitude of gaging basin centroid, decimal degrees
#' \item Y. Latitude of gaging basin centroid, decimal degrees
#' \item ET0Har. Reference evapotranspiration calculated by means of the Hargreaves-Samani relation, in millimeters
#' \item preSub1. Value of `precip` from previous month, in millimeters
#' \item preSub2. Value of `precip` from two months prior to current month, in millimeters
#' \item tmaxSub1. Value of `tmax` from previous month, in degrees Celcius
#' \item tmaxSub2. Value of `tmax` from two months prior to current month, in degrees Celcius
#' \item tmeanSub1. Value of `tmean` from previous month, in degrees Celcius
#' \item tmeanSub2. Value of `tmean` from two months prior to current month, in degrees Celcius
#' \item tminSub1. Value of `tmin` from previous month, in degrees Celcius
#' \item tminSub2. Value of `tmin` from two months prior to current month, in degrees Celcius
#' \item ET0Sub1. Value of `ET0Har` from previous month, in millimeters
#' \item ET0Sub2. Value of `ET0Har` from two months prior to current month, in millimeters
#' \item preTot6. Value of `precip` summed over previous 6 months, in millimeters
#' \item MIN. Minimum elevation of the gaging basin, in meters
#' \item MAX. Maximum elevation of the gaging basin, in meters
#' \item RANGE. Range of elevation of the gaging basin, in meters
#' \item MEAN. Mean elevation of the gaging basin, in meters
#' \item STD. Standard deviation of the elevation of the gaging basin, in meters
#' \item H2o. Percentage of gaging basin classified as open water
#' \item Q. Percentage of gaging basin having geology classified as Quaternary
#' \item pgT. Percentage of gaging basin having geology classified as Paleogene
#' \item K. Percentage of gaging basin having geology classified as Cretaceous
#' \item lPz. Percentage of gaging basin having geology classified as Lower Paleozoic
#' \item nT. Percentage of gaging basin having geology classified as Neogene
#' \item Yv. Percentage of gaging basin having geology classified as Middle Proterozoic
#' \item mPz. Percentage of gaging basin having geology classified as Middle Paleozoic
#' \item uPz. Percentage of gaging basin having geology classified as Upper Paleozoic
#' \item Yg. Percentage of gaging basin having geology classified as Middle Proterozoic
#' \item ZPz. Percentage of gaging basin having geology classified as Late Proterozoic
#' \item drain_area_va. Drainage area of the drainage basin associated with the stream gage, in square miles
#' }
#' @docType data
#' @keywords datasets
#' @name testGagedDat.rds
#' @details Original predictor variable dataset used to generate discharge and baseflow
#' estimates for the drainage basins associated with USGS gages.
NULL


#' @title [DATASET] Predictor variables for *un*gaged basins
#' @description Original predictor variable dataset for the drainage basins that are *NOT* associated with USGS gages.
#' @format A data frame with 232960 rows and 37 variables:
#' \itemize{
#' \item siteNo. USGS gage ID; since these basins are not associated with a USGS gage, the labels are of the form 'ung3' (for 'ungaged 3'),
#' which is an arbitrary label associated with the ungaged basin drainage area
#' \item Date. Date of discharge, precipitation, and temperature observation
#' \item Flow. Total daily average discharge, in cubic feet per second
#' \item baseFlow. Estimated daily average baseflow calculated by means of the HYSEP algorithm, cubic feet per second
#' \item precip. Daily average zonal precipitation calculated for the drainage basin associated with the gage, in millimeters
#' \item tmax. Daily average zonal maximum air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item tmean. Daily average zonal mean air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item tmin. Daily average zonal minimum air temperature calculated for the drainage basin associated with the gage, in degrees Celcius
#' \item X. Longitude of gaging basin centroid, decimal degrees
#' \item Y. Latitude of gaging basin centroid, decimal degrees
#' \item ET0Har. Reference evapotranspiration calculated by means of the Hargreaves-Samani relation, in millimeters
#' \item preSub1. Value of `precip` from previous month, in millimeters
#' \item preSub2. Value of `precip` from two months prior to current month, in millimeters
#' \item tmaxSub1. Value of `tmax` from previous month, in degrees Celcius
#' \item tmaxSub2. Value of `tmax` from two months prior to current month, in degrees Celcius
#' \item tmeanSub1. Value of `tmean` from previous month, in degrees Celcius
#' \item tmeanSub2. Value of `tmean` from two months prior to current month, in degrees Celcius
#' \item tminSub1. Value of `tmin` from previous month, in degrees Celcius
#' \item tminSub2. Value of `tmin` from two months prior to current month, in degrees Celcius
#' \item ET0Sub1. Value of `ET0Har` from previous month, in millimeters
#' \item ET0Sub2. Value of `ET0Har` from two months prior to current month, in millimeters
#' \item preTot6. Value of `precip` summed over previous 6 months, in millimeters
#' \item MIN. Minimum elevation of the gaging basin, in meters
#' \item MAX. Maximum elevation of the gaging basin, in meters
#' \item RANGE. Range of elevation of the gaging basin, in meters
#' \item MEAN. Mean elevation of the gaging basin, in meters
#' \item STD. Standard deviation of the elevation of the gaging basin, in meters
#' \item H2o. Percentage of gaging basin classified as open water
#' \item Q. Percentage of gaging basin having geology classified as Quaternary
#' \item pgT. Percentage of gaging basin having geology classified as Paleogene
#' \item K. Percentage of gaging basin having geology classified as Cretaceous
#' \item lPz. Percentage of gaging basin having geology classified as Lower Paleozoic
#' \item nT. Percentage of gaging basin having geology classified as Neogene
#' \item Yv. Percentage of gaging basin having geology classified as Middle Proterozoic
#' \item mPz. Percentage of gaging basin having geology classified as Middle Paleozoic
#' \item uPz. Percentage of gaging basin having geology classified as Upper Paleozoic
#' \item Yg. Percentage of gaging basin having geology classified as Middle Proterozoic
#' \item ZPz. Percentage of gaging basin having geology classified as Late Proterozoic
#' \item drain_area_va. Drainage area of the drainage basin associated with the stream gage, in square miles
#' }
#' @docType data
#' @keywords datasets
#' @name testUngagedDat.rds
#' @details Original predictor variable dataset used to generate discharge and baseflow
#' estimates for the drainage basins *NOT* associated with USGS gages.
NULL

#' @title [DATASET] Baseflow used in initial MODFLOW/SFR application
#' @description Original random forest model output as supplied to the MODFLOW modelers.
#' @docType data
#' @keywords datasets
#' @name baseflow_data_used_in_first_round_of_SFR.rds
#' @usage readr::read_rds("data/baseflow_data_used_in_first_round_of_SFR.rds")
#' @format A data frame with 874453 rows and 4 variables:
#' \itemize{
#'   \item{\code{siteNo}}{character USGS gage ID.}
#'   \item{\code{Date}}{date Date associated with baseflow estimate. }
#'   \item{\code{baseFlow}}{double Estimated baseflow, in cubic feet per second.
#'   \item{\code{comment}}{character One of 'estimated gaged', 'estimated ungaged', or 'calculated'.}
#'}
#' @details This is the data file used in the initial application of the SFR MODFLOW package as applied to the
#' MERAS study area. The file contains a mix of random forest model forecasts and observed values for baseflow.
#' Values with a comment field value of 'estimated ungaged' or 'estimated gaged' represent outputs from the random
#' forest model. Values with the comment field listed as 'calculated' represent observed values of baseflow
#' as calculated from streamflow records using hydrograph separation methods.
NULL


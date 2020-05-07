#' Retrieve and summarize flow data by month for a site
#'
#' This function retrieves site data for a USGS gaging station, pulls down all daily mean values
#' that exist in the record, and summarizes the mean daily flow by month. In addition, the function
#' calls the 'part' routine coded as part of the USGS DVstats package to perform a baseflow separation
#' on the daily values. Months for which all or part of the daily values are 'NA' (missing) are deleted.
#'
#' If the user is running this function from within the USGS firewall, setAccess('internal') is
#' called in the initial dataRetrieval package setup. This *may* retrieve
#' more site results than are publically available outside the USGS firewall.
#'
#' @param site A character representation of a USGS gage ID
#' @param insideUSGSfirewall A logical indicating whether the user is inside
#' or outside of the USGS firewall

#'
#' @return List containing 1) 'df': a dataframe listing the mean streamflow and baseflow where calculable for each month-year in the record, and 2) 'siteinfo': site information returned from the USGS NWIS system
#' @export
#' @importFrom magrittr %>%
#' @examples
#' mylist <- getFlowDataForSite("07063000")
#' head(mylist$df)
#' print(mylist$siteinfo)
getFlowDataForSite <- function(site, insideUSGSfirewall=FALSE) {

  # apparently some sites are set to allow access from within USGS firewall ONLY
  # if we're inside the firewall, set this to TRUE in order to gain access to some
  # of these sites. note that these sites contain *APPROVED* data, so it is
  # unclear why public access is disabled.
  if (insideUSGSfirewall) {
    dataRetrieval::setAccess('internal')
  }

  # these NWIS peak value codes indicate:
  # 5	discharge affected to unknown degree by regulation or diversion**
  # 6	discharge affected by regulation or diversion**
  # C	all or part of the record affected by urbanization, mining, agricultural changes,
  #      channelization, or others
  regVals <- c("5", "6", "C", "5,C", "6,C")

  info <- dataRetrieval::readNWISsite(site)
  dvs <- dataRetrieval::readNWISdv(site, parameterCd = "00060") %>%
         dataRetrieval::renameNWISColumns()

  # This hack is designed to cover edge cases (such as for gage ID 02441390, 03592000) where
  # NWIS reports back *two* columns containing flow data. The additional column is
  # labelled "X_..2.._00060_00003", whereas normally only a single data column
  # labelled "X_00060_00003" would be returned
  if( !is.null(dvs$..2.._Flow) ) {
    dvs$Flow <- ifelse(is.na(dvs$..2.._Flow), dvs$Flow, dvs$..2.._Flow)
  }

  if (nrow(dvs) == 0) {

    monthlySummary <- NA

  } else {

    dvSeq <- data.table(Date = seq(from = min(dvs$Date), to=max(dvs$Date), by = "1 days"))
    dvBase <- dplyr::left_join(x = dvSeq, y = dvs, by = "Date", all.x = TRUE)
    dvBase$Flow_imputed <- ifelse(is.na(dvBase$Flow),1,dvBase$Flow)

    pkFile <- dataRetrieval::readNWISpeak(siteNumbers=site)

    # filter out everything except valid records that contain peak flow codes indicative of
    # flow diversion or regulation
    pkFile <- pkFile %>%
                dplyr::filter(!is.na(peak_va) & !is.na(peak_dt)) %>%
                dplyr::filter(peak_cd %in% regVals)

    monthlySummary <- dvBase %>%
      dplyr::filter( !is.na(Flow)) %>%
      dplyr::group_by(Date = format(Date, "%Y-%m")) %>%
      dplyr::summarize(Flow = signif(mean(Flow), 3), Count=n())

    #partBaseflow <- with(dvBase,
    #                     DVstats::part(Flow = Flow_imputed, Dates = Date, da = info$drain_area_va, STAID = site))



    partBaseflow <- with(dvBase, bf_part(date=Date, discharge=Flow_imputed, da=info$drain_area_va, STAID=site) )

    baseSummary <- partBaseflow %>%
      dplyr::group_by(Date = format(date, "%Y-%m")) %>%
      dplyr::summarize(Baseflow = signif(mean(baseflow, na.rm=TRUE), 3))

    monthlySummary <- monthlySummary %>%
      dplyr::left_join(baseSummary, by = "Date", all.x = TRUE) %>%
      dplyr::filter(!is.na(Flow))

    colnames(monthlySummary) <- c("date","discharge","count_non_na","baseflow")

    monthlySummary$YYYYMMDD <- lubridate::ymd( paste(monthlySummary$date,"15",sep="-") )

    monthlySummary$is_regulated <- FALSE

    # if there are any rows of data, presume that they pertain to diversions or
    # other flow regulation practices
    if ( nrow(pkFile) > 1) {

      startDate <- min(pkFile$peak_dt)
      endDate <- max(pkFile$peak_dt)

      # don't run this if we have nonsensical start and end dates
      if ( endDate >= startDate ) {

        # any record that falls with the probable date of flow regulation is marked as such
        monthlySummary$is_regulated<- ifelse(
            (lubridate::year(monthlySummary$YYYYMMDD) >= lubridate::year(startDate))
            & (lubridate::year(monthlySummary$YYYYMMDD) <= lubridate::year(endDate)),
            TRUE,
            FALSE )
      }

    }

    attr(monthlySummary,"Drainage_area") <- info$drain_area_va

  }
  return( list(df=monthlySummary, siteinfo=info ) )
}

#' Create a data frame of random forest model inputs for a site
#'
#' @param drainage_area_sq_mi Drainage area upstream of the point of interest, in square miles
#' @param lon Longitude of the centroid of the drainage area associated with the point of interest
#' @param lat Latitude of the centriod of the drainage area associated with the point of interest
#' @param start_date Starting date for analysis (POSIX_date)
#' @param end_date Ending date for analysis (POSIX_date)
#'
#' @return Data frame with all necessary data items needed to run using the trained random forest model
#' @export
#'
#' @examples
makeRandomForestInput <- function( drainage_area_sq_mi, lon, lat, start_date, end_date,
                                   site_info, path_to_PRISM_data="") {

  start_year  <- lubridate::year(start_date)
  start_month <- lubridate::month(start_date)
  end_year    <- lubridate::year(end_date)
  end_month   <- lubridate::month(end_date)

#  test_PRISM_file   <- paste(path_to_PRISM_data,"ppt","PRISM_ppt_stable_4kmM2_1896_bil.bil",sep="/")
#  test_PRISM_raster <- velox::velox(test_PRISM_file)

  max_clusters = 7

  cl <- parallel::makeCluster(max_clusters)

  # must register parallel backend with the 'foreach' package
  doParallel::registerDoParallel(cl)

    # create a list of dates for which we want to summarize PRISM data
  sim_dates <- format(seq(lubridate::ymd(start_date),lubridate::ymd(end_date),by="month"),format="%Y-%m")

  centroid_coord <- cbind(lon,lat)
  centroid_sp <- sp::SpatialPoints(coords=centroid_coord)

  output_df <- foreach::foreach(b=c('tmin','tmax','tmean','ppt'),
                               .combine=rbind)                                        %:%

    foreach::foreach(a=sim_dates,
                     .combine=rbind,
                     .packages=c("magrittr","dplyr","raster","stringr",
                                 "data.table","lubridate","velox","mapRandomForest") ) %dopar% {

                                   value <- getRasterValuesAtPoints_PRISM(shp_sf=centroid_sp,
                                                                          month=unlist(stringr::str_split(a,"-"))[2],
                                                                          year=unlist(stringr::str_split(a,"-"))[1],
                                                                          dataname=b,
                                                                          path_to_PRISM_data=path_to_PRISM_data)

                                   data.frame(date=a, value=value, dataname=b)

                                 }

# now move the values associated with 'tmin', 'tmean', 'tmax', 'ppt' to their own columns
new_df <- tidyr::spread(output_df, key=dataname, value=value)

new_df <- new_df %>% dplyr::left_join(site_info, by=c('date'='date'))

# add in some details pulled from the NWIS site file
new_df <- new_df %>% dplyr::mutate(lat=lat,
                                   lon=lon,
                                   site_no=site_info$site_no,
                                   area_sq_mi=site_info$drain_area_va,
                                   agency_cd=site_info$agency_cd,
                                   station_nm=site_info$station_nm,
                                   dec_lat_va=site_info$dec_lat_va,
                                   dec_lon_va=site_info$dec_long_va)

# create lagged values for precipitation data inputs
new_df <- new_df %>%
                     dplyr::rename(precip=ppt) %>%
                     dplyr::group_by(site_no) %>%
                     dplyr::mutate(block_id=mapRandomForest::identify_contiguous(date)) %>%
                     dplyr::mutate(group_id=paste(site_no,block_id,sep="_")) %>%
                     dplyr::group_by(group_id) %>%
                     dplyr::mutate(precip_lag_1m=dplyr::lag(precip,n=1)) %>%
                     dplyr::mutate(precip_lag_2m=dplyr::lag(precip,n=2)) %>%
                     dplyr::mutate(precip_lag_3m=dplyr::lag(precip,n=3)) %>%
                     dplyr::mutate(precip_lag_4m=dplyr::lag(precip,n=4)) %>%
                     dplyr::mutate(precip_lag_5m=dplyr::lag(precip,n=5)) %>%
                     dplyr::mutate(precip_lag_6m=dplyr::lag(precip,n=6)) %>%
                     dplyr::mutate(precip_sum_6m=precip_lag_1m +
                                                 precip_lag_2m +
                                                 precip_lag_3m +
                                                 precip_lag_4m +
                                                 precip_lag_5m +
                                                 precip_lag_6m )

# Create lagged values for minimum air temperature data
new_df <- new_df %>%
                     dplyr::group_by(group_id) %>%
                     dplyr::mutate(tmin_lag_1m=dplyr::lag(tmin,n=1,default=NA)) %>%
                     dplyr::mutate(tmin_lag_2m=dplyr::lag(tmin,n=2,default=NA))

# Create lagged values for maximum air temperature data
new_df <- new_df %>%
                     dplyr::group_by(group_id) %>%
                     dplyr::mutate(tmax_lag_1m=dplyr::lag(tmax,n=1,default=NA)) %>%
                     dplyr::mutate(tmax_lag_2m=dplyr::lag(tmax,n=2,default=NA))

# Create lagged values for mean air temperature data
new_df <- new_df %>%
                     dplyr::group_by(group_id) %>%
                     dplyr::mutate(tmean_lag_1m=dplyr::lag(tmean,n=1,default=NA)) %>%
                     dplyr::mutate(tmean_lag_2m=dplyr::lag(tmean,n=2,default=NA))

# add date/time variables for use calculating Hargreaves ET0
new_df <- new_df %>% dplyr::mutate(yyyymmdd=lubridate::ymd(paste(date,"15",sep="-"))) %>%
                     dplyr::mutate(year=lubridate::year(yyyymmdd)) %>%
                     dplyr::mutate(days_in_month=lubridate::days_in_month(yyyymmdd)) %>%
                     dplyr::mutate(day_of_year=as.integer(yyyymmdd - lubridate::ymd(paste(year-1,"-12-31"))))

# calculate Reference Evapotranspiration by means of the Hargreaves-Samani (1985) relation
new_df <- new_df %>% dplyr::mutate(Hargreaves_ET0=mapRandomForest::hargreavesET0(tmin, tmax, day_of_year, days_in_month, precip, lat))

# Create lagged values for Hargreaves ET0
new_df <- new_df %>%
        dplyr::group_by(group_id) %>%
        dplyr::mutate(ET0_lag_1m=dplyr::lag(Hargreaves_ET0,n=1,default=NA)) %>%
        dplyr::mutate(ET0_lag_2m=dplyr::lag(Hargreaves_ET0,n=2,default=NA)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(ET0_lag_1m)) %>%
        dplyr::filter(!is.na(ET0_lag_2m))

# here we eliminate the columns containing 'NA' owing to the fact that we can't calculate a 6-month lagged
# value until 6 months into the record!
new_df <- new_df %>%
             dplyr::filter(!is.na(precip_sum_6m))  %>%
             dplyr::filter(!is.na(tmean_lag_2m))   %>%
             dplyr::ungroup()                      %>%
             dplyr::select(-block_id, -group_id)

# Finally we create the data frame that will be fed into the Random Forest model
input_df <- new_df %>% dplyr::rename(X=lon, Y=lat) %>%
                       dplyr::rename(tmaxSub1=tmax_lag_1m, tmaxSub2=tmax_lag_2m) %>%
                       dplyr::rename(tminSub1=tmin_lag_1m, tminSub2=tmin_lag_2m) %>%
                       dplyr::rename(tmeanSub1=tmean_lag_1m, tmeanSub2=tmean_lag_2m) %>%
                       dplyr::rename(ET0Har=Hargreaves_ET0) %>%
                       dplyr::rename(ET0Sub1=ET0_lag_1m, ET0Sub2=ET0_lag_2m) %>%
                       dplyr::rename(preSub1=precip_lag_1m, preSub2=precip_lag_2m) %>%
                       dplyr::rename(preTot6=precip_sum_6m) %>%
                       dplyr::rename(drain_area_va=area_sq_mi)

return(input_df)

}

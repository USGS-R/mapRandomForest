#' Identify contiguous blocks of data
#'
#' Returns a vector identifying contigous blocks of data. Data is judged
#' to be part of a contiguous block if the date (expressed as year and month)
#' increases monotonically without gaps. The block ID is incremented by one
#' when a gap is identified.
#'
#' @param yyyymm A string representing the date expressed as "YYYY-MM"
#'
#' @return blockIDs A vector of numerical block IDs
#'
#' @export
#' @importFrom magrittr %>%
identify_contiguous <- function(date_strings) {

  df <- data.frame(posix_date=lubridate::ymd(paste(date_strings,"-01",sep="")))

  df <- df %>% dplyr::mutate(expected_date = lubridate::add_with_rollback(dplyr::lag(df$posix_date,1),months(1)))
  df <- df %>% dplyr::mutate(is_contiguous = expected_date==posix_date)

  block_starts <- which(!df$is_contiguous | is.na(df$is_contiguous))

  # it may be that the entire block of dates is contiguous; if so, the
  # logic used to ID block ends will fail. just set the block_id to 1
  if( length(block_starts) == 1) {

    df$block_id <- 1

  } else {

    block_ends <- block_starts[2:length(block_starts)] - 1

    df$block_id <- 0
    block_id <- 0

    for (n in 1:length(block_ends)) {
      block_id <- block_id + 1
      df$block_id[block_starts[n]:block_ends[n]] <- block_id
    }

    # take care of numbering for last contiguous block
    block_id <- block_id + 1
    df$block_id[block_starts[n+1]:nrow(df)] <- block_id

  }

  return(df$block_id)
}

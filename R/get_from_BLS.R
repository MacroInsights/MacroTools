#' Downloads specific series from BLS as either long format, wide format, or individual series.
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param seriesIDs Vector of codes from the BLS to get.
#' @param format Output format to get: Default is 'long', but it can return 'wide'
#' @param BLS_key A BLS Key
#'
#' @return An xts object
#' @export
#'
#' @examples
#' get_from_BLS(seriesIDs = "LNS14000000")
#' get_from_BLS(seriesIDs = c("LNS14000000","CES0000000001"))
#' get_from_BLS(end_year = 2019, start_year = 2012, seriesIDs = "LNS14000000")
#' get_from_BLS(seriesIDs = c("LNS14000000","BDU0000000000000000110001LQ5"), format = 'wide')
get_from_BLS <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    seriesIDs,
    format = 'long',
    BLS_key = blsKey)
{

  #######################################################################
  #                          LOGIC FOR DATES
  # Current year
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  # If end_year is NULL, set it to the current year
  if (is.null(end_year)) {
    end_year <- currentYear
  } else {
    end_year <- as.numeric(end_year) # Ensure end_year is numeric
  }

  # If start_year is NULL, set it to five years less than end_year
  if (is.null(start_year)) {
    start_year <- end_year - 5
  } else {
    start_year <- as.numeric(start_year) }


  # Ensure start_year is less than end_year
  if (start_year >= end_year) {
    stop("start_year must be less than end_year")
  }


  # start_year = lubridate::year(Sys.Date()) - years
  # end_year = lubridate::year(Sys.Date())

  code_vector <- as.character(seriesIDs)

  df_raw <- bls_post_chunked(
    seriesIDs  = code_vector,
    start_year = start_year,
    end_year   = end_year,
    BLS_key    = BLS_key
  )

  if(format == 'wide') {
    df_output <- df_raw %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date = lubridate::ym(paste(df_raw$year,
                             df_raw$period))) %>%
      dplyr::select(date, value, seriesID) %>%
      tidyr::pivot_wider(names_from = seriesID, values_from = value)
    return(df_output)
  }

  if(format == 'long') {
    return(df_raw)
  }



}, cache = memoise::cache_memory())

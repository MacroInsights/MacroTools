#' Downloads one or more series from FRED in long format.
#'
#' @param seriesIDs A character vector of FRED series IDs (e.g. "GDP", "UMCSENT").
#' @param start_year First year of data to get. Default is five years before end_year.
#' @param end_year Last year of data to get. Default is today's year.
#' @param separate If TRUE, returns a named list of tibbles, one per series at its natural frequency.
#' @param fred_key A FRED API key.
#'
#' @return A long-format tibble, or a named list of tibbles if separate = TRUE.
#' @export
#'
#' @examples
#' get_from_fred("GDP")
#' get_from_fred(c("GDP", "UMCSENT"))
#' get_from_fred(c("GDP", "UMCSENT"), separate = TRUE)
#' get_from_fred("GDPC1", start_year = 2010, end_year = 2023)
get_from_fred <- memoise::memoise(function(
    seriesIDs,
    start_year = NULL,
    end_year = NULL,
    separate = FALSE,
    fred_key = fredKey)
{

  years <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year   <- years$end_year

  fredr::fredr_set_key(fred_key)

  series_ids <- as.character(seriesIDs)

  series_list <- purrr::map(series_ids, function(sid) {
    fredr::fredr(
      series_id         = sid,
      observation_start = as.Date(paste0(start_year, "-01-01")),
      observation_end   = as.Date(paste0(end_year,   "-12-31"))
    ) |>
      dplyr::select(date, series_id, value)
  })

  if (separate) {
    names(series_list) <- series_ids
    return(series_list)
  }

  purrr::list_rbind(series_list)

}, cache = memoise::cache_memory())

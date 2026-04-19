#' Downloads unemployment claims data from FRED (weekly or monthly)
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param monthly If FALSE (default), returns weekly claims. If TRUE, returns monthly averages
#' @param latest Gets the latest complete observation
#' @param fred_key A FRED API key
#'
#' @return An xts object with unemployment claims data
#' @export
#'
#' @examples
#' claims_weekly <- get_claims()
#' claims_monthly <- get_claims(monthly = TRUE)
#' claims_latest <- get_claims(latest = TRUE)
get_claims <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    monthly = FALSE,
    latest = FALSE,
    fred_key = fredKey
) {

  years <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year <- years$end_year

  # Setting FRED API Key
  fredr::fredr_set_key(fred_key)

  # Claims series (weekly, SA)
  variables <- c(
    "ICSA",  # Initial Claims
    "CCSA"   # Continuing Claims
  )

  num_series <- length(variables)

  # ----------------------------
  # Download from FRED (weekly)
  # ----------------------------
  params <- list(
    series_id = variables,
    observation_start = rep(as.Date(paste0(start_year, "-01-01")), num_series),
    observation_end   = rep(as.Date(paste0(end_year, "-12-31")),  num_series)
  )

  raw_claims <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(
      series_id = ..1,
      observation_start = ..2,
      observation_end   = ..3
    )
  ) %>%
    dplyr::select(date, series_id, value)

  # ----------------------------
  # WEEKLY output (default)
  # ----------------------------
  if (!monthly) {

    claims_out <- raw_claims %>%
      tidyr::pivot_wider(names_from = series_id, values_from = value) %>%
      dplyr::rename(
        InitialClaims = ICSA,
        ContinuingClaims = CCSA
      )

  } else {

    # ----------------------------
    # MONTHLY output (average of weekly)
    # ----------------------------
    claims_out <- raw_claims %>%
      dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
      dplyr::group_by(date, series_id) %>%
      dplyr::summarise(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(names_from = series_id, values_from = value) %>%
      dplyr::rename(
        InitialClaims = ICSA,
        ContinuingClaims = CCSA
      )
  }

  # ----------------------------
  # Latest complete observation
  # ----------------------------
  if (latest) {
    claims_out <- claims_out %>%
      tidyr::drop_na() %>%
      utils::tail(1)
  }

  claims_out %>% xts::as.xts()

}, cache = memoise::cache_memory())

#' Downloads monthly JOLTS data from FRED
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param latest Gets the latest complete JOLTS figures
#' @param fred_key A FRED API key
#'
#' @return An xts object with JOLTS data
#' @export
#'
#' @examples
#' jolts <- get_jolts()
#' jolts_latest <- get_jolts(latest = TRUE)
#' jolts_2010_2020 <- get_jolts(start_year = 2010, end_year = 2020)
get_jolts <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    latest = FALSE,
    fred_key = fredKey
) {

  #######################################################################
  #                          LOGIC FOR DATES
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  if (is.null(end_year)) {
    end_year <- currentYear
  } else {
    end_year <- as.numeric(end_year)
  }

  if (is.null(start_year)) {
    start_year <- end_year - 5
  } else {
    start_year <- as.numeric(start_year)
  }

  if (start_year >= end_year) {
    stop("start_year must be less than end_year")
  }
  #######################################################################

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  # JOLTS + unemployment level series (FRED)
  variables <- c(
    "JTSJOL",    # Job Openings
    "JTSQUL",    # Quits
    "JTSLDL",    # Layoffs and Discharges
    "UNEMPLOY"   # Unemployment Level
  )

  num_series <- length(variables)

  # ----------------------------
  # Download from FRED
  # ----------------------------
  params <- list(
    series_id = variables,
    observation_start = rep(as.Date(paste0(start_year, "-01-01")), num_series),
    observation_end   = rep(as.Date(paste0(end_year, "-12-31")),  num_series)
  )

  raw_data_jolts <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(
      series_id = ..1,
      observation_start = ..2,
      observation_end   = ..3
    )
  ) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value)

  # ----------------------------
  # Rename for clarity
  # ----------------------------
  raw_data_jolts <- raw_data_jolts %>%
    dplyr::rename(
      JobOpenings = JTSJOL,     # thousands
      Quits = JTSQUL,           # thousands
      LayoffsDischarges = JTSLDL, # thousands
      UnemploymentLevel = UNEMPLOY # thousands of persons
    )

  # ----------------------------
  # Latest complete observation
  # ----------------------------
  if (latest) {
    raw_data_jolts <- raw_data_jolts %>%
      tidyr::drop_na() %>%
      utils::tail(1)
  }

  raw_data_jolts %>% xts::as.xts()

}, cache = memoise::cache_memory())

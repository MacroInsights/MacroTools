#' Downloads unemployment data from Fred
#'
#' @param years Number of years of data to get
#' @param geography Whether to include all states
#' @param demography Wheter to include race/ethnicity
#' @param latest Gets the latest complete unemployment figures
#' @param fred_key A FRED API
#' @param BLS_key A BLS KEy
#'
#' @return An xts object with unemployment data
#' @export
#'
#' @examples
#' unemployment <- get_unemployment()
get_unemployment <- memoise::memoise(function(
    years = 5,
    geography = "National",
    demography = FALSE,
    latest = FALSE,
    fred_key = fredKey,
    BLS_key = blsKey)
{


  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  states <- data.frame(state = c(state.abb,"DC"))

  if (length(geography) == 1 && geography == "National") {
    variables <- "UNRATE"
  } else if (length(geography) == 1 && geography == "State") {
    variables <- NULL
    for (state in states$state) {
      tmp <- paste0(state, "UR")
      variables <- c(variables, tmp)
    }
    variables <- c(variables, "UNRATE")
  } else if (is.vector(geography) && length(geography) >= 1) {
    states <- geography
    variables <- NULL
    for (state in states) {
      tmp <- paste0(state, "UR")
      variables <- c(variables, tmp)
    }
    variables <- c(variables, "UNRATE")
  } else {
    stop("Invalid input for 'geography'.")
  }

  if (demography) {
    variables <- c(variables,
                   "LNS14000003",           # White
                   "LNS14000006",           # Black
                   "LNS14000009",           # Hispanic
                   "LNU04032183",           # Asian (NSA)
                   "LNS14000028",           # White men
                   "LNS14000029",           # White women
                   "LNS14000031",           # Black men
                   "LNS14000032",           # Black women
                   "LNU04000034",           # Hispanic men
                   "LNU04000035"            # Hispanic women
                   )
  }
  num_series <- length(variables)


  # Connect to Fred and Download Data ####
  # Paparameters of the request
  params <- list(
    series_id = variables,
    observation_start = rep(as.Date(lubridate::floor_date(Sys.Date(),
                                               unit = "weeks") + lubridate::days(7))-lubridate::years(years),
                            num_series),
    observation_end = rep(as.Date(lubridate::floor_date(Sys.Date(),
                                             unit = "weeks") + lubridate::days(7)), num_series))

  # Actual request of data
  raw_data_fred <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                 observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value) |>
    dplyr::rename(USUR = UNRATE)

  if(demography) {
    # Downloading American Indians and Alaska Natives Unemployment
    start_year <- params$observation_start[[1]] %>% lubridate::year()
    end_year <- params$observation_end[[1]] %>% lubridate::year()
    api_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
    payload <- glue::glue('{
                    "seriesid":["LNU04035243"],
                    "startyear":"{{start_year}}",
                    "endyear":"{{end_year}}",
                    "registrationkey":"{{BLS_key}}"
                    }', .open="{{", .close="}}")

    response <- httr::POST(api_url,
                     body = payload,
                     httr::content_type("application/json"),
                     encode = "json")

    x <- httr::content(response, "text") %>%
      jsonlite::fromJSON()

    UR_AIAN_raw <- x$Results$series$data[[1]] %>%
      as_tibble()

    UR_AIAN <- UR_AIAN_raw %>%
      transmute(date = ym(paste(UR_AIAN_raw$year,
                                UR_AIAN_raw$periodName)),
                ur_AIAN = value %>% as.numeric())

    raw_data_fred %>% left_join(UR_AIAN,
                                by=c("date")) -> raw_data_fred

    raw_data_fred <- raw_data_fred %>%
      dplyr::rename(WhiteUR = LNS14000003,
                    BlackUR = LNS14000006,
                    HispanicUR = LNS14000009,
                    WhiteMenUR = LNS14000028,
                    WhiteWomenUR = LNS14000029,
                    BlackMenUR = LNS14000031,
                    BlackWomenUR = LNS14000032,
                    HispanicMenUR = LNU04000034,
                    HispanicWomenUR = LNU04000035,
                    AsianUR = LNU04032183,
                    AIANUR = ur_AIAN)

    }

  # Gets the latest complete unemployment figures
  if(latest) {
    raw_data_fred <- raw_data_fred |>
    tidyr::drop_na() |>
      tail(1)
  }

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

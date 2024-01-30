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
                   "LNS14000003",     # White UR
                   "LNS14000006",     # Black UR
                   "LNS14000009",     # Hispanic UR
                   "LNU04032183"      # Asian UR
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

    # Specifying the payload
    demo_variables <- tribble(
      ~seriesID, ~name,
      "LNS14000004", "WhiteMenUR_SA",
      "LNS14000005", "WhiteWomenUR_SA",
      "LNS14000007", "BlackMenUR_SA",
      "LNS14000008", "BlackWomenUR_SA",
      "LNU04000004", "WhiteMenUR_NS",
      "LNU04000005", "WhiteWomenUR_NS",
      "LNU04000007", "BlackMenUR_NS",
      "LNU04000008", "BlackWomenUR_NS",
      "LNU04000010", "HispanicMenUR_NS",
      "LNU04000011", "HispanicWomenUR_NS",
      "LNU04035243", "AIANUR_NS")
    code_vector <- as.character(demo_variables$seriesID)

    payload <- glue::glue('{
  "seriesid":{{jsonlite::toJSON(code_vector)}},
  "startyear":"{{start_year}}",
  "endyear":"{{end_year}}",
  "registrationkey":"{{blsKey}}"
}', .open="{{", .close="}}")

    response <- httr::POST(api_url,
                     body = payload,
                     httr::content_type("application/json"),
                     encode = "json")

    raw_data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()

    # Combine all the data in long form:
    df_raw <- raw_data$Results$series |>
      rowwise() |>
      mutate(data = list(
        data |>
          transmute(date = as.numeric(year),
                    value = as.numeric(value),
                    period = periodName) |>
          mutate(seriesID = first(seriesID)))) |>
      pull(data) %>%
      map_dfr(~ .x)

    df_tidy <- df_raw %>% as_tibble() %>%
      mutate(date = ym(paste(df_raw$date,
                             df_raw$period))) %>%
      select(date, value, seriesID) %>%
      left_join(demo_variables) %>%
      select(-seriesID) %>%
      pivot_wider(names_from = name, values_from = value)

    raw_data_fred %>% left_join(df_tidy,
                                by=c("date")) -> raw_data_fred

    raw_data_fred <- raw_data_fred %>%
      dplyr::rename(WhiteUR_SA = LNS14000003,
                    BlackUR_SA = LNS14000006,
                    HispanicUR_SA = LNS14000009,
                    AsianUR_NS = LNU04032183
      )


    }

  # Gets the latest complete unemployment figures
  if(latest) {
    raw_data_fred <- raw_data_fred |>
    tidyr::drop_na() |>
      tail(1)
  }

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

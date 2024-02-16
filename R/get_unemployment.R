#' Downloads monthly unemployment data from Fred and the BLS
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param geography 'National' (default), 'State', or list of states like c("DC","NM")
#' @param demography Include national levels of unemployment by gender/race/ethnicity
#' @param latest Gets the latest complete unemployment figures
#' @param fred_key A FRED API
#' @param BLS_key A BLS KEy
#'
#' @return An xts object with unemployment data
#' @export
#'
#' @examples
#' unemployment <- get_unemployment()
#' unemployment_state_demography <- get_unemployment(geography = c("DC","NC"), demography = TRUE)
#' unemployment_latest <- get_unemployment(latest = TRUE)
#' unemployment_2020_2021 <- get_unemployment(start_year = 2020, end_year = 2021)
get_unemployment <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    geography = "National",
    demography = FALSE,
    latest = FALSE,
    fred_key = fredKey,
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
  #######################################################################

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
    observation_start = rep(as.Date(paste0(start_year,"-01","-01")),
                            num_series),
    observation_end = rep(as.Date(paste0(end_year,"-12","-31")), num_series))

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

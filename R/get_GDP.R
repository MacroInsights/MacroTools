#' Downloads Real GDP from Fred for the US, all US States, or a selection of states.
#'
#' @param years Number of years of data to get
#' @param geography 'National' (default), 'State', or list of states like c("DC","NM")
#' @param latest Gets the latest complete GDP figures
#' @param fred_key A FRED API
#'
#' @return An xts object with GDP data
#' @export
#'
#' @examples
#' real_GDP <- get_GDP()
#' states_real_GDP <- get_GDP(geography = c("DC","NC"), latest = TRUE)
get_GDP <- memoise::memoise(function(
    years = 5,
    geography = "National",
    latest = FALSE,
    fred_key = fredKey)
{

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  states <- data.frame(state = c(state.abb,"DC"))

  if (length(geography) == 1 && geography == "National") {
    variables <- "GDPC1"
  } else if (length(geography) == 1 && geography == "State") {
    variables <- NULL
    for (state in states$state) {
      tmp <- paste0(state, "RQGSP")
      variables <- c(variables, tmp)
    }
    variables <- c(variables, "GDPC1")
  } else if (is.vector(geography) && length(geography) >= 1) {
    states <- geography
    variables <- NULL
    for (state in states) {
      tmp <- paste0(state, "RQGSP")
      variables <- c(variables, tmp)
    }
    variables <- c(variables, "GDPC1")
  } else {
    stop("Invalid input for 'geography'.")
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
    dplyr::rename(RQGDP = GDPC1)

  # Subroutine to get GDP per capita
  # It uses whatever the latest population figures are for the relevant geography
  perCapita <- FALSE

  if(perCapita) {

    first_year <- params[[2]][[1]] |> lubridate::year()
    last_year <- lubridate::year(lubridate::today())
    pop_data <- NULL

    for (year in first_year:last_year) {
      if (year == 2020 | year == 2023) {
        next  # skip the rest of the loop for this iteration
      }
      temp <- get_acs(
      geography = "state"
    , variables = 'B01001_001'
    , survey = 'acs1'
    , state = states$state
    , year = year
    , cache_table = TRUE) %>% suppressMessages() %>%
      dplyr::mutate(state_abb = fips(as.factor(NAME)),
             state_abb = fips(state_abb, to = 'Abbreviation')) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"USA"))) |>
      transmute(state_abb = state_abb, pop = estimate) |>
      dplyr::mutate(year = year)
      pop_data <- bind_rows(pop_data, temp)
    }

  if(geography == "State") {

    raw_data_fred <- left_join(pop_data,
                               raw_data_fred |>
                                 tidyr::pivot_longer(-date,
                                                     names_to = "state",
                                                     values_to = "RGDP") |>
                                 mutate(state_abb = if_else(state != "RQGDP",
                                                            str_sub(state, 1, 2), "USA"),
                                        year = year(date))) %>%
      mutate(RGDPPC = if_else(state_abb != "USA", RGDP/pop*1000000,RGDP/pop*1000000000)) %>%
      select(date, state,RGDPPC) %>%
      pivot_wider(names_from = state, values_from = RGDPPC, names_glue = "{state}_PC") }
    else {
      raw_data_fred <- left_join(pop_data %>% filter(state_abb == 'USA'),
                                 raw_data_fred |>
                                   tidyr::pivot_longer(-date,
                                                       names_to = "state",
                                                       values_to = "RGDP") %>%
                                   mutate(state_abb = "USA",
                                          year = year(date))) %>%
        mutate(RGDPPC = RGDP/pop*1000000000) %>%
        select(date, state,RGDPPC) %>%
        pivot_wider(names_from = state, values_from = RGDPPC, names_glue = "{state}_PC")
      }

}

  # Gets the latest complete unemployment figures
  if(latest) {
    raw_data_fred <- raw_data_fred |>
    tidyr::drop_na() |>
      tail(1)
  }

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

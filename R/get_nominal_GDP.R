#' Downloads Nominal GDP from Fred for the US, all US States, or a selection of states.
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param geography 'National' (default), 'State', or list of states like c("DC","NM")
#' @param latest Gets the latest complete GDP figures
#' @param fred_key A FRED API
#'
#' @return An xts object with GDP data
#' @export
#'
#' @examples
#' nominal_GDP <- get_nominal_GDP()
#' nominal_GDP_from_2020 <- get_nominal_GDP(start_year = 2020)
#' states_nominal_GDP <- get_nominal_GDP(geography = c("DC","NC"), latest = TRUE)
get_nominal_GDP <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    geography = "National",
    latest = FALSE,
    fred_key = fredKey)
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
    variables <- "GDP"
  } else if (length(geography) == 1 && geography == "State") {
    variables <- NULL
    for (state in states$state) {
      tmp <- paste0(state, "NQGSP")
      variables <- c(variables, tmp)
    }
    variables <- c(variables, "GDP")
  } else if (is.vector(geography) && length(geography) >= 1) {
    states <- geography
    variables <- NULL
    for (state in states) {
      tmp <- paste0(state, "NQGSP")
      variables <- c(variables, tmp)
    }
    variables <- c(variables, "GDP")
  } else {
    stop("Invalid input for 'geography'.")
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
    dplyr::rename(QGDP = GDP)

  # Subroutine to get GDP per capita
  # It uses whatever the latest population figures are for the relevant geography

  perCapita <- FALSE

  if(perCapita) {

    first_year <- params[[2]][[1]] |> year()
    last_year <- year(today())
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
      mutate(state_abb = fips(as.factor(NAME)),
             state_abb = fips(state_abb, to = 'Abbreviation')) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"USA"))) |>
      transmute(state_abb = state_abb, pop = estimate) |>
      mutate(year = year)
      pop_data <- bind_rows(pop_data, temp)
    }

  if(geography == "State") {

    raw_data_fred <- left_join(pop_data,
                               raw_data_fred |>
                                 tidyr::pivot_longer(-date,
                                                     names_to = "state",
                                                     values_to = "GDP") |>
                                 mutate(state_abb = if_else(state != "QGDP",
                                                            str_sub(state, 1, 2), "USA"),
                                        year = year(date))) %>%
      mutate(GDPPC = if_else(state_abb != "USA", GDP/pop*1000000,GDP/pop*1000000000)) %>%
      select(date, state,GDPPC) %>%
      pivot_wider(names_from = state, values_from = GDPPC, names_glue = "{state}_PC") }
    else {
      raw_data_fred <- left_join(pop_data %>% filter(state_abb == 'USA'),
                                 raw_data_fred |>
                                   tidyr::pivot_longer(-date,
                                                       names_to = "state",
                                                       values_to = "GDP") %>%
                                   mutate(state_abb = "USA",
                                          year = year(date))) %>%
        mutate(GDPPC = GDP/pop*1000000000) %>%
        select(date, state,GDPPC) %>%
        pivot_wider(names_from = state, values_from = GDPPC, names_glue = "{state}_PC")
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

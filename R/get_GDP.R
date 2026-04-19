#' Downloads GDP from Fred for the US, all US States, or a selection of states.
#'
#' All figures are in millions
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param geography 'National' (default), 'State', or list of states like c("DC","NM")
#' @param latest Gets the latest complete GDP figures
#' @param real If TRUE (default), returns real (inflation-adjusted) GDP. If FALSE, returns nominal GDP.
#' @param fred_key A FRED API
#'
#' @return An XTS object with GDP data
#' @export
#'
#' @examples
#' real_GDP <- get_GDP()
#' nominal_GDP <- get_GDP(real = FALSE)
#' real_GDP_from_2020 <- get_GDP(start_year = 2020)
#' states_real_GDP <- get_GDP(geography = c("DC","NC"), latest = TRUE)
get_GDP <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    geography = "National",
    latest = FALSE,
    real = TRUE,
    fred_key = fredKey)
{

  years <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year <- years$end_year

  fredr::fredr_set_key(fred_key)

  nat_series   <- if (real) "GDPC1" else "GDP"
  state_suffix <- if (real) "RQGSP" else "NQGSP"
  col_name     <- if (real) "RQGDP" else "QGDP"

  states <- data.frame(state = c(state.abb,"DC"))

  if (length(geography) == 1 && geography == "National") {
    variables <- nat_series
  } else if (length(geography) == 1 && geography == "State") {
    variables <- c(paste0(states$state, state_suffix), nat_series)
  } else if (is.vector(geography) && length(geography) >= 1) {
    variables <- c(paste0(geography, state_suffix), nat_series)
  } else {
    stop("Invalid input for 'geography'.")
  }

  num_series <- length(variables)

  params <- list(
    series_id = variables,
    observation_start = rep(as.Date(paste0(start_year,"-01","-01")),
                            num_series),
    observation_end = rep(as.Date(paste0(end_year,"-12","-31")), num_series))

  raw_data_fred <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                 observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value) |>
    dplyr::rename_with(~ col_name, .cols = dplyr::all_of(nat_series)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(col_name), ~ .x * 1000))

  if(latest) {
    raw_data_fred <- raw_data_fred |>
    tidyr::drop_na() |>
      tail(1)
  }

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

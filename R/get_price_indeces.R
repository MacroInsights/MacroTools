#' Downloads unemployment data from Fred
#'
#' @param years Number of years of data to get
#' @param geography Whether to include all states
#' @param demography Wheter to include race/ethnicity
#'
#' @return An xts object with unemployment data
#' @export
#'
#' @examples
#' unemployment <- get_unemployment()
get_price_indeces <- memoise::memoise(function(
    years = 5,
    fred_key = fredKey)
  {

  variables <- dplyr::tribble(~code, ~name,
                        "CPIAUCSL",	      'CPI',
                        "CPILFESL",	      'Core CPI',
                        "PCEPI",	        'PCE',
                        "PCEPILFE",	      'Core PCE',
                        "PPIFIS",	        'Producer Price Index',
                        "CPIUFDSL",	      'Food',
                        "CUSR0000SAF11",	'Food at home',
                        "CUSR0000SEFV",	  'Food away',
                        "CPIENGSL",	      'Energy',
                        "CUSR0000SACE",	  'Energy commodities',
                        "CUSR0000SETB01",	'Gasoline',
                        "CUUR0000SEHE",	  'Fuel oil',
                        "CUSR0000SEHF",	  'Energy services',
                        "CUSR0000SEHF01",	'Electricity',
                        "CUSR0000SEHF02",	'Utility gas service',
                        "CUSR0000SACL1E",	'Commodities less food and energy',
                        "CUSR0000SETA01",	'New vehicles',
                        "CUSR0000SETA02",	'Used cars and trucks',
                        "CPIAPPSL",	      'Apparel',
                        "CUSR0000SAM1",	  'Medical care commodities',
                        "CUSR0000SASLE",	'Services less energy',
                        "CUSR0000SAH1",	  'Shelter',
                        "CUSR0000SAS4",	  'Transportation',
                        "CUSR0000SAM2",	  'Medical care services')

  num_series <- length(variables$code)


  # Connect to Fred and Download Data ####
  # Paparameters of the request
  params <- list(
    series_id = variables$code,
    observation_start = rep(as.Date(lubridate::floor_date(Sys.Date(),
                                    unit = "weeks") +
                                    lubridate::days(7))-lubridate::years(years),
                                    num_series),
    observation_end = rep(as.Date(lubridate::floor_date(Sys.Date(),
                                  unit = "weeks") +
                                  lubridate::days(7)), num_series))

  # Actual request of data
  raw_data_fred <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                 observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value)

  colnames(raw_data_fred)[1:num_series+1] <- variables$name

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

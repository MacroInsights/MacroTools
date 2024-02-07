#' Downloads the following price indexes data from Fred and the BLS:
#'   "CPIAUCSL",	      'CPI',
#'   "CPILFESL",	      'Core CPI',
#'   "PCEPI",	          'PCE',
#'   "PCEPILFE",	      'Core PCE',
#'   "PPIFIS",	        'Producer Price Index',
#'   "CPIUFDSL",	      'Food',
#'   "CUSR0000SAF11",	  'Food at home',
#'   "CUSR0000SEFV",	  'Food away',
#'   "CPIENGSL",	      'Energy',
#'   "CUSR0000SACE",	  'Energy commodities',
#'   "CUSR0000SETB01",	'Gasoline',
#'   "CUSR0000SEHF",	  'Energy services',
#'   "CUSR0000SEHF01",	'Electricity',
#'   "CUSR0000SEHF02",	'Utility gas service',
#'   "CUSR0000SACL1E",	'Commodities less food and energy',
#'   "CUSR0000SETA01",	'New vehicles',
#'   "CUSR0000SETA02",	'Used cars and trucks',
#'   "CPIAPPSL",	      'Apparel',
#'   "CUSR0000SAM1",	  'Medical care commodities',
#'   "CUSR0000SASLE",	  'Services less energy',
#'   "CUSR0000SAH1",	  'Shelter',
#'   "CUSR0000SAS4",	  'Transportation',
#'   "CUSR0000SAM2",	  'Medical care services')
#'
#' @param fred_key A fred API key
#' @param BLS_key A bls API Key
#' @param years Number of years of data to get
#'
#' @return An xts object with unemployment data
#' @export
#'
#' @examples
#' price_indeces <- get_price_indeces()
get_price_indeces <- memoise::memoise(function(
    years = 5,
    fred_key = fredKey,
    BLS_key = blsKey)
  {

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

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


  raw_data_fred <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                 observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value)

  # Substituting Fuel Oil
  start_year <- params$observation_start[[1]] %>% lubridate::year()
  end_year <- params$observation_end[[1]] %>% lubridate::year()
  api_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
  payload <- glue::glue('{
                    "seriesid":["CUUR0000SEHE01"],
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

  Fuel_Oil_raw <- x$Results$series$data[[1]] %>%
    dplyr::as_tibble()

  # Tidying up the data
  Fuel_Oil <- Fuel_Oil_raw %>%
    dplyr::transmute(date = lubridate::ym(paste(Fuel_Oil_raw$year,
                              Fuel_Oil_raw$periodName)),
              `Fuel Oil` = value %>% as.numeric())

  #Joins BLS

  raw_data_fred %>% dplyr::left_join(Fuel_Oil,
                              by=c("date")) -> raw_data_fred

  colnames(raw_data_fred)[1:num_series+1] <- variables$name

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

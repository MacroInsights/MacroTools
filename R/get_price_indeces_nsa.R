#' Downloads the following not-seasonally adjusted price indexes data from Fred and the BLS:
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
get_price_indeces_nsa <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
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

  variables <- dplyr::tribble(~code, ~name,
                        "CPIAUCNS",	      'CPI',
                        "CPILFENS",	      'Core CPI',
                        "PCEPI",	        'PCE',
                        "PCEPILFE",	      'Core PCE',
                        "PPIFID",	        'Producer Price Index',
                        "CPIUFDNS",	      'Food',
                        "CUUR0000SAF11",	'Food at home',
                        "CUUR0000SEFV",	  'Food away',
                        "CPIENGNS",	      'Energy',
                        "CUUR0000SACE",	  'Energy commodities',
                        "CUUR0000SETB01",	'Gasoline',
                        "CUSR0000SEHF",	  'Energy services',
                        "CUUR0000SEHF01",	'Electricity',
                        "CUUR0000SEHF02",	'Utility gas service',
                        "CUUR0000SACL1E",	'Commodities less food and energy',
                        "CUUR0000SETA01",	'New vehicles',
                        "CUUR0000SETA02",	'Used cars and trucks',
                        "CPIAPPNS",	      'Apparel',
                        "CUUR0000SAM1",	  'Medical care commodities',
                        "CUUR0000SASLE",	'Services less energy',
                        "CUUR0000SAH1",	  'Shelter',
                        "CUUR0000SAS4",	  'Transportation',
                        "CUUR0000SAM2",	  'Medical care services')

  num_series <- length(variables$code)


  # Connect to Fred and Download Data ####
  # Paparameters of the request
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
    as_tibble()

  # Tidying up the data
  Fuel_Oil <- Fuel_Oil_raw %>%
    transmute(date = ym(paste(Fuel_Oil_raw$year,
                              Fuel_Oil_raw$periodName)),
              `Fuel Oil` = value %>% as.numeric())

  #Joins BLS

  raw_data_fred %>% left_join(Fuel_Oil,
                              by=c("date")) -> raw_data_fred

  colnames(raw_data_fred)[1:num_series+1] <- variables$name

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

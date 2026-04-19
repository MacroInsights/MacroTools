#' Downloads price index data from FRED and the BLS.
#'
#' Variables included:
#'   CPI, Core CPI, PCE, Core PCE, Producer Price Index,
#'   Food, Food at home, Food away, Energy, Energy commodities,
#'   Gasoline, Energy services, Electricity, Utility gas service,
#'   Commodities less food and energy, New vehicles, Used cars and trucks,
#'   Apparel, Medical care commodities, Services less energy,
#'   Shelter, Transportation, Medical care services, Fuel Oil
#'
#' @param fred_key A FRED API key
#' @param BLS_key A BLS API key
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param seasonally_adjusted If TRUE (default), returns seasonally adjusted series. If FALSE, returns not seasonally adjusted series.
#'
#' @return An xts object with price index data
#' @export
#'
#' @examples
#' price_indeces <- get_price_indeces()
#' price_indeces_nsa <- get_price_indeces(seasonally_adjusted = FALSE)
get_price_indeces <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    seasonally_adjusted = TRUE,
    fred_key = fredKey,
    BLS_key = blsKey)
  {

  years <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year <- years$end_year

  fredr::fredr_set_key(fred_key)

  if (seasonally_adjusted) {
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
  } else {
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
  }

  num_series <- length(variables$code)

  params <- list(
    series_id = variables$code,
    observation_start = rep(as.Date(paste0(start_year,"-01","-01")),
                            num_series),
    observation_end = rep(as.Date(paste0(end_year,"-12","-31")), num_series))

  raw_data_fred <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                 observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value)

  Fuel_Oil <- bls_post_chunked(
    seriesIDs  = "CUUR0000SEHE01",
    start_year = start_year,
    end_year   = end_year,
    BLS_key    = BLS_key
  ) |>
    dplyr::transmute(
      date       = lubridate::ym(paste(year, period)),
      `Fuel Oil` = value
    )

  raw_data_fred %>% dplyr::left_join(Fuel_Oil,
                              by=c("date")) -> raw_data_fred

  colnames(raw_data_fred)[2:(num_series+1)] <- variables$name

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory())

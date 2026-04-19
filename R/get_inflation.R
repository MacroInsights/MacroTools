#' Computes inflation data. It uses unseasonably adjusted data for yearly computed inflation and seasonally adjusted data for monthly inflation. The main inflation variables are:
#'   "CPIAUCSL",	      'CPI',
#'   "CPILFESL",	      'Core CPI',
#'   "PCEPI",	          'PCE',
#'   "PCEPILFE",	      'Core PCE',
#'   "PPIFIS",	        'Producer Price Index'
#' The additional inflation variables are:
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
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param monthly Whether to calculate MoM % Change. The default is FALSE
#' @param main_variables Whether to include only CPI, Core CPI, PCE, Core PCE, and Producer Price Index. The default is TRUE
#'
#' @return An xts object with inflation data
#' @export
#'
#' @examples
#' inflation <- get_inflation()
#' inflation_since_2020 <- get_inflation(start_year = 2020)
#' inflationMoM <- get_inflation(monthly = TRUE, main_variables = FALSE)
get_inflation <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    monthly = FALSE,
    main_variables = TRUE,
    fred_key = fredKey,
    BLS_key = blsKey)
  {

  years <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year <- years$end_year

  fredr::fredr_set_key(fred_key)

  pricesM <- MacroTools::get_price_indeces(start_year = start_year - 1, end_year = end_year)
  pricesY <- MacroTools::get_price_indeces(start_year = start_year - 1, end_year = end_year, seasonally_adjusted = FALSE)

  if(monthly) {
    inflation <- (pricesM - dplyr::lag(pricesM, 1)) / dplyr::lag(pricesM, 1) * 100
  } else {
    inflation <- (pricesY - dplyr::lag(pricesY, 12)) / dplyr::lag(pricesY, 12) * 100
  }

  if(main_variables) {
    inflation <- inflation[,1:5]}

  return(inflation[-1:-12,])


}, cache = memoise::cache_memory())

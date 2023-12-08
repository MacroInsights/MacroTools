#' Computes inflation data. The default yields five years of monthly YoY inflation of:
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
#' @param years Number of years of data to get
#' @param monthly Whether to calculate MoM % Change
#' @param main_variables Whether to include only CPI, Core CPI, PCE, Core PCE, andProducer Price Index
#'
#' @return An xts object with inflation data
#' @export
#'
#' @examples
#' inflation <- get_inflation()
#' inflationMoM <- get_inflation(monthly = TRUE, main_variables = TRUE)
get_inflation <- memoise::memoise(function(
    years = 5,
    monthly = FALSE,
    main_variables = FALSE,
    fred_key = fredKey,
    BLS_key = blsKey)
  {

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  prices <- get_price_indeces(years + 1)

  if(monthly) {
    inflation <- (prices - dplyr::lag(prices, 1)) / dplyr::lag(prices, 1) * 100
  } else {
    inflation <- (prices - dplyr::lag(prices, 12)) / dplyr::lag(prices, 12) * 100
  }

  if(main_variables) {
    inflation <- inflation[,1:5]}

  return(inflation[-1:-12,])


}, cache = memoise::cache_memory())

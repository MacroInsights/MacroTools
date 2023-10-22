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
get_inflation <- memoise::memoise(function(
    years = 5,
    monthly = FALSE,
    main_variables = FALSE,
    fred_key = fredKey)
  {

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr_set_key(fred_key)

  prices <- get_price_indeces(years)

  if(monthly) {
    inflation <- (prices - dplyr::lag(prices, 1)) / dplyr::lag(prices, 1) * 100
  } else {
    inflation <- (prices - dplyr::lag(prices, 12)) / dplyr::lag(prices, 12) * 100
  }

  if(main_variables) {
    inflation <- inflation[,1:5]}

  return(inflation)


}, cache = memoise::cache_memory())

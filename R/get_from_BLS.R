#' Downloads specific series from BLS as either long format, wide format, or individual series.
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param seriesIDs Vector of codes from the BLS to get.
#' @param format Output format to get: Default is 'long', but it can return 'wide' and 'individual' for individual series
#' @param BLS_key A BLS Key
#'
#' @return An xts object
#' @export
#'
#' @examples
#' get_from_BLS(seriesIDs = "LNS14000000")
#' get_from_BLS(seriesIDs = c("LNS14000000","CES0000000001"))
#' get_from_BLS(end_year = 2019, start_year = 2012, seriesIDs = "LNS14000000")
#' get_from_BLS(seriesIDs = c("LNS14000000","CES0000000001"), format = 'individual')
#' get_from_BLS(seriesIDs = c("LNS14000000","BDU0000000000000000110001LQ5"), format = 'wide')
get_from_BLS <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    seriesIDs,
    format = 'long',
    BLS_key = blsKey)
{

  api_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

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


  # start_year = lubridate::year(Sys.Date()) - years
  # end_year = lubridate::year(Sys.Date())

  # Specifying the payload
  code_vector <- as.character(seriesIDs)

  payload <- glue::glue('{
  "seriesid":{{jsonlite::toJSON(code_vector)}},
  "startyear":"{{start_year}}",
  "endyear":"{{end_year}}",
  "registrationkey":"{{blsKey}}"
}', .open="{{", .close="}}")

  response <- httr::POST(api_url,
                         body = payload,
                         httr::content_type("application/json"),
                         encode = "json")

  raw_data <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  # Combine all the data in long form:
  df_raw <- raw_data$Results$series |>
    rowwise() |>
    mutate(data = list(
      data |>
        transmute(date = as.numeric(year),
                  value = as.numeric(value),
                  period = periodName) |>
        mutate(seriesID = first(seriesID)))) |>
    pull(data) %>%
    map_dfr(~ .x)

  if(format == 'wide') {
    df_output <- df_raw %>%
      as_tibble() %>%
      mutate(date = ym(paste(df_raw$date,
                             df_raw$period))) %>%
      select(date, value, seriesID) %>%
      pivot_wider(names_from = seriesID, values_from = value)
    return(df_output)
  }

  if(format == "individual") {
    i = 1
    for (series in code_vector) {
      temp_df <- data.frame(raw_data$Results$series$data[[i]])
      assign(series, temp_df, envir = .GlobalEnv)
      i <- i + 1
    }
  }

  if(format == 'long') {
    return(df_raw)
  }



}, cache = memoise::cache_memory())

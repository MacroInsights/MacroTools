#' Downloads unemployment data from Fred
#'
#' @param years Number of years of data to get
#' @param geography Whether to include all states
#' @param demography Wheter to include race/ethnicity
#' @param latest Gets the latest complete unemployment figures
#' @param fred_key A FRED API
#' @param BLS_key A BLS KEy
#'
#' @return An xts object with unemployment data
#' @export
#'
#' @examples
#' unemployment <- get_unemployment()
get_from_BLS <- memoise::memoise(function(
    years = 5,
    seriesIDs,
    format = 'long',
    BLS_key = blsKey)
{

  api_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

  start_year = lubridate::year(Sys.Date()) - years
  end_year = lubridate::year(Sys.Date())

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
      left_join(demo_variables) %>%
      select(-seriesID) %>%
      pivot_wider(names_from = name, values_from = value)
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

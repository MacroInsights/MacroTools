# Internal helper — not exported.
# Calls the BLS API, chunking the year range into 20-year windows as required
# by the API limit. Returns a long-format tibble with columns:
# year (numeric), value (numeric), period (character), seriesID (character).
bls_post_chunked <- function(seriesIDs, start_year, end_year, BLS_key) {
  api_url     <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
  code_vector <- as.character(seriesIDs)

  chunk_starts <- seq(start_year, end_year, by = 20)
  chunk_ends   <- pmin(chunk_starts + 19, end_year)

  purrr::map2_dfr(chunk_starts, chunk_ends, function(sy, ey) {
    response <- httr::POST(
      url = api_url,
      body = list(
        seriesid        = I(code_vector),
        startyear       = as.character(sy),
        endyear         = as.character(ey),
        registrationkey = BLS_key
      ),
      encode = "json",
      httr::content_type("application/json")
    )

    raw_data <- httr::content(response, "text", encoding = "UTF-8") |>
      jsonlite::fromJSON()

    if (is.null(raw_data$Results$series)) {
      status_msg <- if (!is.null(raw_data$status)) raw_data$status else "unknown"
      api_msg    <- if (!is.null(raw_data$message)) paste(raw_data$message, collapse = " ") else ""
      warning(glue::glue("BLS API returned no data for {sy}-{ey}. Status: {status_msg}. {api_msg}"))
      return(tibble::tibble())
    }

    raw_data$Results$series |>
      dplyr::rowwise() |>
      dplyr::mutate(data = list(
        data |>
          dplyr::transmute(
            year     = as.numeric(year),
            value    = as.numeric(value),
            period   = periodName
          ) |>
          dplyr::mutate(seriesID = dplyr::first(seriesID))
      )) |>
      dplyr::pull(data) |>
      purrr::map_dfr(~.x)
  }) |>
    dplyr::distinct()
}

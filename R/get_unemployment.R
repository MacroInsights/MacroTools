#' Downloads monthly unemployment data from Fred and the BLS
#'
#' @param end_year Last year of data to get. Default is today's year
#' @param start_year First year of data to get. Default is five years from 'end_year'
#' @param geography 'National' (default), 'State', or list of states like c("DC","NM")
#' @param demography Include national levels of unemployment by gender/race/ethnicity
#' @param latest Gets the latest complete unemployment figures
#' @param underutilization If TRUE, downloads U-1 through U-6 measures (U1RATE, U2RATE, UNRATE as U-3, U4RATE, U5RATE, U6RATE)
#' @param fred_key A FRED API key
#' @param BLS_key A BLS API key
#'
#' @return An xts object with unemployment data
#' @export
get_unemployment <- memoise::memoise(function(
    start_year = NULL,
    end_year = NULL,
    geography = "National",
    demography = FALSE,
    latest = FALSE,
    underutilization = FALSE,
    fred_key = fredKey,
    BLS_key = blsKey
) {

  #######################################################################
  #                          LOGIC FOR DATES
  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  if (is.null(end_year)) {
    end_year <- currentYear
  } else {
    end_year <- as.numeric(end_year)
  }

  if (is.null(start_year)) {
    start_year <- end_year - 5
  } else {
    start_year <- as.numeric(start_year)
  }

  if (start_year >= end_year) {
    stop("start_year must be less than end_year")
  }
  #######################################################################

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  states <- data.frame(state = c(state.abb, "DC", "PR"))

  # ----------------------------
  # Geography logic
  # ----------------------------
  if (length(geography) == 1 && geography == "National") {
    variables <- "UNRATE"
  } else if (length(geography) == 1 && geography == "State") {
    variables <- NULL
    for (state in states$state) {
      variables <- c(variables, paste0(state, "UR"))
    }
    variables <- c(variables, "UNRATE")
  } else if (is.vector(geography) && length(geography) >= 1) {
    variables <- NULL
    for (state in geography) {
      variables <- c(variables, paste0(state, "UR"))
    }
    variables <- c(variables, "UNRATE")
  } else {
    stop("Invalid input for 'geography'.")
  }

  # ----------------------------
  # Add U-1 ... U-6 measures (FRED series IDs provided)
  # UNRATE is U-3 and is already included above.
  # ----------------------------
  if (isTRUE(underutilization)) {
    u_measures <- c("U1RATE", "U2RATE", "U4RATE", "U5RATE", "U6RATE")
    variables <- unique(c(variables, u_measures))
  }

  # ----------------------------
  # Add demography series from FRED (BLS series added later)
  # ----------------------------
  if (demography) {
    variables <- c(
      variables,
      "LNS14000003",  # White UR (SA)
      "LNS14000006",  # Black UR (SA)
      "LNS14000009",  # Hispanic UR (SA)
      "LNU04032183"   # Asian UR (NS)
    )
  }

  num_series <- length(variables)

  # ----------------------------
  # Download from FRED
  # ----------------------------
  params <- list(
    series_id = variables,
    observation_start = rep(as.Date(paste0(start_year, "-01-01")), num_series),
    observation_end   = rep(as.Date(paste0(end_year, "-12-31")),  num_series)
  )

  raw_data_fred <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(
      series_id = ..1,
      observation_start = ..2,
      observation_end   = ..3
    )
  ) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value)

  # ----------------------------
  # Naming + ordering
  # ----------------------------
  if (isTRUE(underutilization)) {

    raw_data_fred <- raw_data_fred %>%
      dplyr::rename(
        U1 = U1RATE,
        U2 = U2RATE,
        U3 = UNRATE,
        U4 = U4RATE,
        U5 = U5RATE,
        U6 = U6RATE
      )

    # Put U1..U6 in order (U3 between U2 and U4), keep other columns after
    u_order <- c("U1", "U2", "U3", "U4", "U5", "U6")
    raw_data_fred <- raw_data_fred %>%
      dplyr::relocate(dplyr::any_of(u_order), .before = dplyr::everything())

  } else {
    # Original behavior
    raw_data_fred <- raw_data_fred %>% dplyr::rename(USUR = UNRATE)
  }

  # ----------------------------
  # BLS block (only if demography == TRUE)
  # FIXED: build JSON via httr encode="json" (no glue parsing)
  # ----------------------------
  if (demography) {

    start_year_bls <- lubridate::year(params$observation_start[[1]])
    end_year_bls   <- lubridate::year(params$observation_end[[1]])
    api_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

    demo_variables <- tibble::tribble(
      ~seriesID, ~name,
      "LNS14000004", "WhiteMenUR_SA",
      "LNS14000005", "WhiteWomenUR_SA",
      "LNS14000007", "BlackMenUR_SA",
      "LNS14000008", "BlackWomenUR_SA",
      "LNU04000004", "WhiteMenUR_NS",
      "LNU04000005", "WhiteWomenUR_NS",
      "LNU04000007", "BlackMenUR_NS",
      "LNU04000008", "BlackWomenUR_NS",
      "LNU04000010", "HispanicMenUR_NS",
      "LNU04000011", "HispanicWomenUR_NS",
      "LNU04035243", "AIANUR_NS"
    )

    code_vector <- as.character(demo_variables$seriesID)

    # Build body as an R list; httr will encode JSON safely
    response <- httr::POST(
      url = api_url,
      body = list(
        seriesid = code_vector,
        startyear = as.character(start_year_bls),
        endyear = as.character(end_year_bls),
        registrationkey = BLS_key
      ),
      encode = "json",
      httr::content_type("application/json")
    )

    raw_data <- httr::content(response, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()

    # Combine all the data in long form
    df_raw <- raw_data$Results$series |>
      dplyr::rowwise() |>
      dplyr::mutate(data = list(
        data |>
          dplyr::transmute(
            date = as.numeric(year),
            value = as.numeric(value),
            period = periodName
          ) |>
          dplyr::mutate(seriesID = dplyr::first(seriesID))
      )) |>
      dplyr::pull(data) %>%
      purrr::map_dfr(~.x)

    df_tidy <- df_raw %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date = lubridate::ym(paste(date, period))) %>%
      dplyr::select(date, value, seriesID) %>%
      dplyr::left_join(demo_variables, by = c("seriesID")) %>%
      dplyr::select(-seriesID) %>%
      tidyr::pivot_wider(names_from = name, values_from = value)

    raw_data_fred <- raw_data_fred %>%
      dplyr::left_join(df_tidy, by = c("date"))

    # Rename the FRED demographic series already included
    raw_data_fred <- raw_data_fred %>%
      dplyr::rename(
        WhiteUR_SA = LNS14000003,
        BlackUR_SA = LNS14000006,
        HispanicUR_SA = LNS14000009,
        AsianUR_NS = LNU04032183
      )
  }

  # ----------------------------
  # Latest complete row (if requested)
  # ----------------------------
  if (latest) {
    raw_data_fred <- raw_data_fred %>%
      tidyr::drop_na() %>%
      utils::tail(1)
  }

  raw_data_fred %>% xts::as.xts()

}, cache = memoise::cache_memory()
)

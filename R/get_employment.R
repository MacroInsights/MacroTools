#' Downloads monthly employment by industry sector
#'
#' Returns seasonally adjusted all-employee counts (in thousands) from the BLS
#' Current Employment Statistics (CES) program. National data are fetched from
#' FRED; state data are fetched directly from the BLS API.
#'
#' @param start_year First year of data to retrieve. Defaults to 5 years before
#'   \code{end_year}.
#' @param end_year Last year of data to retrieve. Defaults to the current year.
#' @param national Logical. If \code{TRUE} (default), includes national totals
#'   from FRED.
#' @param state Character vector of state abbreviations (e.g. \code{c("NC","VA")}),
#'   \code{"All"} for all 50 states plus DC and PR, or \code{NULL} (default)
#'   to skip state-level data.
#' @param main_industries Logical. If \code{TRUE} (default), returns 14 major
#'   CES supersectors (the standard BLS breakdown). If \code{FALSE}, returns
#'   20 detailed NAICS sectors, splitting manufacturing, financial activities,
#'   professional services, education and health, and leisure and hospitality
#'   into their component subsectors.
#' @param fred_key A FRED API key. Defaults to \code{fredKey}.
#' @param BLS_key A BLS API key. Defaults to \code{blsKey}.
#'
#' @return
#' \itemize{
#'   \item If only \code{national = TRUE}: a wide \code{tibble} with a
#'     \code{date} column and one column per sector.
#'   \item If \code{state} is supplied (and \code{national = FALSE}): a
#'     \code{tibble} with \code{date}, \code{state}, and one column per sector.
#'   \item If both: a named \code{list} with elements \code{national} and
#'     \code{state}.
#' }
#' All employment values are in thousands of employees.
#'
#' @examples
#' \dontrun{
#' # National 14-sector employment, last 5 years
#' get_employment()
#'
#' # All 20 NAICS sectors, national, 2010-2024
#' get_employment(start_year = 2010, end_year = 2024, main_industries = FALSE)
#'
#' # State-level for NC and VA
#' get_employment(national = FALSE, state = c("NC", "VA"))
#'
#' # Both national and state
#' get_employment(state = "All")
#' }
#'
#' @export
get_employment <- memoise::memoise(function(
    start_year      = NULL,
    end_year        = NULL,
    national        = TRUE,
    state           = NULL,
    main_industries = TRUE,
    fred_key        = fredKey,
    BLS_key         = blsKey
) {

  years      <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year   <- years$end_year

  if (!national && is.null(state))
    stop("At least one of 'national' or 'state' must be specified.")

  fredr::fredr_set_key(fred_key)

  # ---------------------------------------------------------------------------
  # Series lookup tables
  #   fred_id      : confirmed FRED series ID (all seasonally adjusted)
  #   bls_industry : 8-digit industry code used in BLS SM state series
  #                  (supersector 2-digit + industry detail 6-digit)
  # ---------------------------------------------------------------------------

  main_series <- tibble::tribble(
    ~label,                           ~fred_id,          ~bls_industry,
    "Mining_Logging",                 "USMINE",          "10000000",
    "Construction",                   "USCONS",          "20000000",
    "Manufacturing",                  "MANEMP",          "30000000",
    "Wholesale_Trade",                "USWTRADE",        "41000000",
    "Retail_Trade",                   "USTRADE",         "42000000",
    "Transportation_Warehousing",     "CES4300000001",   "43000000",
    "Utilities",                      "CES4422000001",   "44220000",
    "Information",                    "USINFO",          "50000000",
    "Financial_Activities",           "USFIRE",          "55000000",
    "Professional_Business_Services", "USPBS",           "60000000",
    "Education_Health_Services",      "USEHS",           "65000000",
    "Leisure_Hospitality",            "USLAH",           "70000000",
    "Other_Services",                 "USSERV",          "80000000",
    "Government",                     "USGOVT",          "90000000"
  )

  detailed_series <- tibble::tribble(
    ~label,                            ~fred_id,          ~bls_industry,
    "Mining_Logging",                  "USMINE",          "10000000",
    "Construction",                    "USCONS",          "20000000",
    "Durable_Goods_Mfg",               "DMANEMP",         "31000000",
    "Nondurable_Goods_Mfg",            "NDMANEMP",        "32000000",
    "Wholesale_Trade",                 "USWTRADE",        "41000000",
    "Retail_Trade",                    "USTRADE",         "42000000",
    "Transportation_Warehousing",      "CES4300000001",   "43000000",
    "Utilities",                       "CES4422000001",   "44220000",
    "Information",                     "USINFO",          "50000000",
    "Finance_Insurance",               "CES5552000001",   "55520000",
    "Real_Estate",                     "CES5553000001",   "55530000",
    "Professional_Technical_Services", "CES6054000001",   "60540000",
    "Management_of_Companies",         "CES6055000001",   "60550000",
    "Administrative_Waste_Services",   "CES6056000001",   "60560000",
    "Educational_Services",            "CES6561000001",   "65610000",
    "Health_Care_Social_Assistance",   "CES6562000001",   "65620000",
    "Arts_Entertainment_Recreation",   "CES7071000001",   "70710000",
    "Accommodation_Food_Services",     "CES7072000001",   "70720000",
    "Other_Services",                  "USSERV",          "80000000",
    "Government",                      "USGOVT",          "90000000"
  )

  series <- if (main_industries) main_series else detailed_series

  # ---------------------------------------------------------------------------
  # National — FRED
  # ---------------------------------------------------------------------------
  national_out <- NULL

  if (national) {
    fred_ids    <- series$fred_id
    n_series    <- nrow(series)
    id_to_label <- setNames(series$label, fred_ids)

    params <- list(
      series_id         = fred_ids,
      observation_start = rep(as.Date(paste0(start_year, "-01-01")), n_series),
      observation_end   = rep(as.Date(paste0(end_year,   "-12-31")), n_series)
    )

    national_out <- purrr::pmap_dfr(
      .l = params,
      .f = ~ fredr::fredr(
        series_id         = ..1,
        observation_start = ..2,
        observation_end   = ..3
      )
    ) %>%
      dplyr::select(date, series_id, value) %>%
      dplyr::mutate(label = id_to_label[series_id]) %>%
      dplyr::select(-series_id) %>%
      tidyr::pivot_wider(names_from = label, values_from = value)
  }

  # ---------------------------------------------------------------------------
  # State — BLS
  # Series ID format: SMS{fips2}00000{industry8}01
  # ---------------------------------------------------------------------------
  state_out <- NULL

  if (!is.null(state)) {

    state_fips <- c(
      AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08",
      CT = "09", DE = "10", DC = "11", FL = "12", GA = "13", HI = "15",
      ID = "16", IL = "17", IN = "18", IA = "19", KS = "20", KY = "21",
      LA = "22", ME = "23", MD = "24", MA = "25", MI = "26", MN = "27",
      MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33",
      NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38", OH = "39",
      OK = "40", OR = "41", PA = "42", RI = "44", SC = "45", SD = "46",
      TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53",
      WV = "54", WI = "55", WY = "56", PR = "72"
    )

    if (length(state) == 1 && state == "All") {
      state_abbrevs <- names(state_fips)
    } else {
      bad_states <- setdiff(state, names(state_fips))
      if (length(bad_states) > 0)
        stop(paste("Unrecognized state abbreviations:", paste(bad_states, collapse = ", ")))
      state_abbrevs <- state
    }

    state_series_df <- tidyr::expand_grid(
      abbrev = state_abbrevs,
      label  = series$label
    ) %>%
      dplyr::left_join(
        dplyr::select(series, label, bls_industry),
        by = "label"
      ) %>%
      dplyr::mutate(
        fips      = state_fips[abbrev],
        series_id = paste0("SMS", fips, "00000", bls_industry, "01")
      )

    # Local BLS fetch: like bls_post_chunked but filters out series that the
    # API returns with empty data arrays (common for state-sector combinations
    # with suppressed or unavailable data).
    fetch_state_bls <- function(seriesIDs, sy, ey, key) {
      api_url      <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
      chunk_starts <- seq(sy, ey, by = 20)
      chunk_ends   <- pmin(chunk_starts + 19, ey)

      purrr::map2_dfr(chunk_starts, chunk_ends, function(cs, ce) {
        response <- httr::POST(
          url    = api_url,
          body   = list(
            seriesid        = I(as.character(seriesIDs)),
            startyear       = as.character(cs),
            endyear         = as.character(ce),
            registrationkey = key
          ),
          encode = "json",
          httr::content_type("application/json")
        )

        raw <- httr::content(response, "text", encoding = "UTF-8") |>
          jsonlite::fromJSON()

        if (is.null(raw$Results$series)) return(tibble::tibble())

        raw$Results$series |>
          dplyr::filter(
            purrr::map_lgl(data, ~ is.data.frame(.x) && nrow(.x) > 0)
          ) |>
          dplyr::rowwise() |>
          dplyr::mutate(data = list(
            data |>
              dplyr::transmute(
                year   = as.numeric(year),
                value  = suppressWarnings(as.numeric(value)),
                period = periodName
              ) |>
              dplyr::mutate(seriesID = seriesID)
          )) |>
          dplyr::pull(data) |>
          purrr::map_dfr(~.x)
      }) |>
        dplyr::distinct()
    }

    all_ids       <- state_series_df$series_id
    series_chunks <- split(all_ids, ceiling(seq_along(all_ids) / 50))

    df_raw <- purrr::map_dfr(series_chunks, function(chunk) {
      fetch_state_bls(chunk, start_year, end_year, BLS_key)
    })

    id_lookup <- dplyr::select(state_series_df, series_id, abbrev, label)

    state_out <- df_raw %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date = lubridate::ym(paste(year, period))) %>%
      dplyr::select(date, seriesID, value) %>%
      dplyr::left_join(id_lookup, by = c("seriesID" = "series_id")) %>%
      dplyr::select(-seriesID) %>%
      dplyr::rename(state = abbrev) %>%
      tidyr::pivot_wider(names_from = label, values_from = value) %>%
      dplyr::arrange(state, date)
  }

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------
  if (!is.null(national_out) && !is.null(state_out)) {
    list(national = national_out, state = state_out)
  } else if (!is.null(national_out)) {
    national_out
  } else {
    state_out
  }

}, cache = memoise::cache_memory())

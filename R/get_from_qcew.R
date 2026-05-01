#' Downloads employment and wage data from the BLS Quarterly Census of Employment and Wages (QCEW).
#'
#' Uses the BLS QCEW public flat-file API — no API key required. Queries are made at the
#' county level (the finest geography the API supports) and optionally aggregated to MSA level.
#'
#' @param area_fips Character vector of 5-digit county FIPS codes
#'   (e.g. `"37081"` for Guilford County, NC). Use this for any county in the US.
#' @param cbsa Character vector of 5-digit CBSA codes for Metropolitan Statistical Areas.
#'   The following Piedmont Triad MSAs are built in: `"24660"` (Greensboro–High Point),
#'   `"49180"` (Winston–Salem), `"15500"` (Burlington). CBSA codes are resolved to their
#'   constituent counties internally.
#' @param start_year First year of data to retrieve. Default is five years before `end_year`.
#' @param end_year Last year of data to retrieve. Default is the current year.
#' @param quarters Integer vector of quarters to retrieve. Default is `1:4` (all quarters).
#'   Pass `"a"` for annual aggregates.
#' @param industry_code NAICS industry code to filter on. Default is `"10"` (all industries total).
#'   Common codes: `"1011"` (all private), `"23"` (construction), `"31-33"` (manufacturing),
#'   `"42"` (wholesale trade), `"44-45"` (retail trade), `"62"` (health care),
#'   `"72"` (leisure & hospitality).
#' @param ownership Ownership code to filter on. Default is `0` (all ownership types).
#'   Options: `1` = federal government, `2` = state government, `3` = local government,
#'   `5` = private.
#' @param aggregate_to_msa Logical. If `TRUE` and `cbsa` is provided, county-level rows are
#'   summed to MSA level (employment and establishments summed; `avg_wkly_wage` is
#'   employment-weighted). Default is `FALSE`.
#'
#' @return A tibble with columns: `area_fips`, `year`, `qtr`, `own_code`, `industry_code`,
#'   `qtrly_estabs`, `month1_emplvl`, `month2_emplvl`, `month3_emplvl`, `avg_monthly_emplvl`,
#'   `total_qtrly_wages`, `avg_wkly_wage`. When `aggregate_to_msa = TRUE`, `area_fips`
#'   contains the CBSA code and an `area_title` column with the MSA name is added.
#' @export
#'
#' @examples
#' # All industries, Guilford County (Greensboro)
#' get_from_qcew(area_fips = "37081", start_year = 2020, end_year = 2023)
#'
#' # Manufacturing sector, Greensboro-HP MSA aggregated
#' get_from_qcew(
#'   cbsa          = "24660",
#'   start_year    = 2015,
#'   end_year      = 2023,
#'   industry_code = "31-33",
#'   aggregate_to_msa = TRUE
#' )
#'
#' # All three Triad MSAs, private sector, aggregated to MSA level
#' get_from_qcew(
#'   cbsa             = c("24660", "49180", "15500"),
#'   start_year       = 2018,
#'   end_year         = 2023,
#'   ownership        = 5,
#'   aggregate_to_msa = TRUE
#' )
get_from_qcew <- memoise::memoise(function(
    area_fips        = NULL,
    cbsa             = NULL,
    start_year       = NULL,
    end_year         = NULL,
    quarters         = 1:4,
    industry_code    = "10",
    ownership        = 0,
    aggregate_to_msa = FALSE)
{

  # ---- Built-in CBSA → county FIPS lookup (Piedmont Triad MSAs) ----------------
  cbsa_lookup <- list(
    "24660" = list(
      title    = "Greensboro-High Point, NC MSA",
      counties = c("37081", "37157", "37151")   # Guilford, Rockingham, Randolph
    ),
    "49180" = list(
      title    = "Winston-Salem, NC MSA",
      counties = c("37067", "37057", "37059", "37169", "37197")  # Forsyth, Davidson, Davie, Stokes, Yadkin
    ),
    "15500" = list(
      title    = "Burlington, NC MSA",
      counties = c("37001")   # Alamance
    )
  )

  # ---- Resolve CBSA codes to county FIPS ----------------------------------------
  cbsa_map <- NULL   # tracks which county FIPS belongs to which CBSA

  if (!is.null(cbsa)) {
    cbsa <- as.character(cbsa)
    unknown <- setdiff(cbsa, names(cbsa_lookup))
    if (length(unknown) > 0) {
      stop(glue::glue(
        "Unknown CBSA code(s): {paste(unknown, collapse = ', ')}. ",
        "Only Piedmont Triad MSAs are built in (24660, 49180, 15500). ",
        "For other geographies pass county FIPS codes via `area_fips`."
      ))
    }

    cbsa_counties <- purrr::map(cbsa, ~ cbsa_lookup[[.x]]$counties)
    cbsa_map <- purrr::map2_dfr(cbsa, cbsa_counties, function(code, counties) {
      tibble::tibble(
        county_fips = counties,
        cbsa_code   = code,
        cbsa_title  = cbsa_lookup[[code]]$title
      )
    })

    cbsa_fips <- cbsa_map$county_fips
    area_fips  <- unique(c(area_fips, cbsa_fips))
  }

  if (is.null(area_fips) || length(area_fips) == 0) {
    stop("Provide at least one county FIPS code via `area_fips` or a CBSA code via `cbsa`.")
  }

  area_fips <- as.character(area_fips)

  # ---- Year validation ----------------------------------------------------------
  years      <- validate_year_range(start_year, end_year)
  start_year <- years$start_year
  end_year   <- years$end_year

  ind_code     <- as.character(industry_code)
  own_code_val <- as.integer(ownership)

  annual_mode <- identical(quarters, "a")
  qtr_values  <- if (annual_mode) "a" else as.integer(quarters)

  base_url <- "https://data.bls.gov/cew/data/api"

  # ---- Fetch one county × year × quarter ----------------------------------------
  fetch_one <- function(fips, year, qtr) {
    url  <- glue::glue("{base_url}/{year}/{qtr}/area/{fips}.csv")
    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) != 200) return(NULL)

    raw <- httr::content(resp, as = "text", encoding = "UTF-8")
    df  <- utils::read.csv(text = raw, stringsAsFactors = FALSE)

    if (nrow(df) == 0) return(NULL)
    df
  }

  combos <- expand.grid(
    fips = area_fips,
    year = seq(start_year, end_year),
    qtr  = qtr_values,
    stringsAsFactors = FALSE
  )

  results <- purrr::pmap(combos, function(fips, year, qtr) {
    fetch_one(fips, year, qtr)
  })

  results <- purrr::compact(results)

  if (length(results) == 0) {
    stop("No QCEW data returned. Check your area_fips codes, year range, and internet connection.")
  }

  df_raw <- dplyr::bind_rows(results)
  names(df_raw) <- tolower(trimws(names(df_raw)))
  df_raw$area_fips <- as.character(df_raw$area_fips)

  # ---- Filter by industry and ownership -----------------------------------------
  df_filtered <- df_raw |>
    dplyr::filter(
      trimws(as.character(industry_code)) == ind_code,
      as.integer(own_code) == own_code_val
    )

  if (nrow(df_filtered) == 0) {
    stop(glue::glue(
      "No rows matched industry_code = '{ind_code}' and ownership = {own_code_val}. ",
      "Check that these codes are valid for the requested areas."
    ))
  }

  df_out <- df_filtered |>
    dplyr::mutate(dplyr::across(
      c(month1_emplvl, month2_emplvl, month3_emplvl,
        total_qtrly_wages, avg_wkly_wage, qtrly_estabs),
      as.numeric
    )) |>
    dplyr::mutate(
      avg_monthly_emplvl = round((month1_emplvl + month2_emplvl + month3_emplvl) / 3, 0)
    ) |>
    dplyr::select(
      area_fips, year, qtr,
      own_code, industry_code,
      qtrly_estabs, month1_emplvl, month2_emplvl, month3_emplvl,
      avg_monthly_emplvl, total_qtrly_wages, avg_wkly_wage
    ) |>
    dplyr::arrange(area_fips, year, qtr)

  # ---- Aggregate to MSA level ---------------------------------------------------
  if (aggregate_to_msa && !is.null(cbsa_map)) {
    df_out <- df_out |>
      dplyr::left_join(
        cbsa_map |> dplyr::rename(area_fips = county_fips),
        by = "area_fips"
      ) |>
      dplyr::filter(!is.na(cbsa_code)) |>
      dplyr::group_by(cbsa_code, cbsa_title, year, qtr, own_code, industry_code) |>
      dplyr::summarise(
        qtrly_estabs       = sum(qtrly_estabs,      na.rm = TRUE),
        month1_emplvl      = sum(month1_emplvl,     na.rm = TRUE),
        month2_emplvl      = sum(month2_emplvl,     na.rm = TRUE),
        month3_emplvl      = sum(month3_emplvl,     na.rm = TRUE),
        total_qtrly_wages  = sum(total_qtrly_wages, na.rm = TRUE),
        avg_wkly_wage      = stats::weighted.mean(avg_wkly_wage, avg_monthly_emplvl, na.rm = TRUE),
        avg_monthly_emplvl = sum(avg_monthly_emplvl, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(area_fips = cbsa_code, area_title = cbsa_title) |>
      dplyr::arrange(area_fips, year, qtr)
  }

  df_out

}, cache = memoise::cache_memory())

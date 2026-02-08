#' Creates a Heatmap of Unemployment in the United States in the style of a stock market board
#'
#' @param fred_key A FRED API Key
#' @param color_up A color for when unemployment increases (and when it is unchanged)
#' @param color_down A color for when unemployment decreases
#' @param nas Color for NA cells
#' @param background Table background color
#' @param borders Border color
#'
#' @return A PNG Table
#' @export
#'
#' @examples
#' create_unemp_heatmap()
create_unemp_heatmap <- function(
    fred_key = fredKey,
    color_up = "#d40509",
    color_down = "#046438",
    nas = "black",
    background = "black",
    borders = "black"
) {

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  # --- 1) Pull a small recent window to detect the latest year present in the data
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # Use a small buffer so we still detect the latest year even if current year has gaps
  probe <- get_unemployment(start_year = current_year - 2, end_year = current_year) |>
    transform_to_tsibble() |>
    dplyr::mutate(
      Month = lubridate::month(date, label = TRUE),
      Year  = lubridate::year(date)
    ) |>
    dplyr::as_tibble()

  latest_year <- probe |>
    dplyr::filter(!is.na(USUR)) |>
    dplyr::summarise(latest_year = max(Year, na.rm = TRUE)) |>
    dplyr::pull(latest_year)

  start_year <- latest_year - 9
  end_year   <- latest_year

  # --- 2) Pull the decade window (10 calendar years)
  data <- get_unemployment(start_year = start_year, end_year = end_year) |>
    transform_to_tsibble() |>
    dplyr::mutate(
      Month = lubridate::month(date, label = TRUE),
      Year  = lubridate::year(date),
      Month = factor(Month, levels = month.abb, ordered = TRUE)
    ) |>
    dplyr::as_tibble() |>
    dplyr::filter(Year >= start_year, Year <= end_year)

  # --- 3) Compute direction signal (unchanged counts as "up")
  data <- data |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      delta  = USUR - dplyr::lag(USUR),
      change = dplyr::case_when(
        is.na(delta) ~ NA_real_,
        delta > 0    ~ 1,     # up only if strictly higher
        delta <= 0   ~ -1     # down OR unchanged
      )
    )

  # --- 4) Build level table and change table
  table_data <- data |>
    dplyr::select(Month, Year, USUR) |>
    dplyr::arrange(Month) |>
    tidyr::pivot_wider(names_from = Year, values_from = USUR)

  table_data_chg <- data |>
    dplyr::select(Month, Year, change) |>
    dplyr::arrange(Month) |>
    tidyr::pivot_wider(names_from = Year, values_from = change, names_prefix = "Chg_") |>
    dplyr::select(-Month)

  out <- dplyr::bind_cols(table_data, table_data_chg)

  # Rename ONLY the year columns for display: 2016 -> '16, etc.
  year_cols_raw <- names(out)[grepl("^\\d{4}$", names(out))]
  out <- out |>
    dplyr::rename_with(
      ~ stringr::str_replace(.x, "^20", "'"),
      dplyr::all_of(year_cols_raw)
    )

  # Dynamically define columns after renaming
  year_cols <- names(out)[grepl("^'\\d{2}$", names(out))]
  chg_cols  <- names(out)[grepl("^Chg_\\d{4}$", names(out))]

  # --- 5) Render GT table
  out |>
    gt::gt(rowname_col = "Month") |>
    gt::cols_hide(columns = dplyr::all_of(chg_cols)) |>
    gt::sub_missing(missing_text = "") |>
    gt::data_color(
      columns = dplyr::all_of(chg_cols),          # signal columns
      target_columns = dplyr::all_of(year_cols),  # colored columns
      palette = c(color_down, color_up),
      na_color = nas
    ) |>
    gt::tab_options(
      table.background.color = background,
      table.border.top.width = 0,
      column_labels.padding  = gt::px(35),
      heading.padding        = gt::px(35)
    ) |>
    gt::cols_label(Month = "") |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right", "left"),
        color = borders,
        weight = gt::px(5),
        style = "solid"
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("bottom", "top"),
        color = "transparent",
        weight = gt::px(0)
      ),
      locations = gt::cells_body()
    ) |>
    gt::opt_horizontal_padding(scale = 3) |>
    gt::opt_vertical_padding(scale = 1.3) |>
    gt::grand_summary_rows(
      columns = dplyr::all_of(year_cols),
      fns = list(Yearly ~ mean(., na.rm = TRUE)),
      fmt = ~ gt::fmt_number(., use_seps = FALSE)
    ) |>
    gt::tab_header(title = "Unemployment Rate") |>
    gt::gtsave("unemp_heatmap.png")
}

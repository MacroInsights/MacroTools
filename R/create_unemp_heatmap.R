#' Creates a Heatmap of Unemployment in the United States
#'
#' @param fred_key A FRED API KEy
#' @param color_up A color for when unemployment increases
#' @param color_down A color for when unemployment decreases
#'
#' @return A PNG Table
#' @export
#'
#' @examples
#' create_unemp_heatmap()
create_unemp_heatmap <- function(
    fred_key = fredKey,
    color_up = "#d40509",
    color_down = "#046438"
    ) {


  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  data <- get_unemployment(10) |>
    transform_to_tsibble() |>
    dplyr::mutate(Month = lubridate::month(date, label = TRUE),
           Year = lubridate::year(date)) |>
    dplyr::as_tibble()

  first_month <- data |> dplyr::select(Month) |> dplyr::slice(1) |> dplyr::pull()
  first_year <- data |> dplyr::select(Year) |> dplyr::slice(1) |> dplyr::pull()
  if (first_month != "Jan") {
    data <- data |> dplyr::filter(Year > first_year)
  }

  data <- data |> dplyr::mutate(change = USUR - lag(USUR)) %>%
    dplyr::mutate(change = dplyr::if_else(change > 0, 1, -1))

  table_data <- data |>
    dplyr::select(-date, -change) |>
    dplyr::arrange(Month) |>
    tidyr::pivot_wider(names_from = Year, values_from = USUR)


  table_data_chg <- data |>
    dplyr::select(-date, -USUR) |>
    dplyr::arrange(Month) %>%
    tidyr::pivot_wider(names_from = Year, values_from = change, names_prefix = "Chg_") %>%
    dplyr::select(-1)

  dplyr::bind_cols(table_data, table_data_chg) %>%
    dplyr::rename_with(~stringr::str_replace(.,"^20","'")) |>
    gt::gt(rowname_col = "Month") |>
    gt::cols_hide(c(Chg_2014:Chg_2023)) %>%
    gt::sub_missing(missing_text = "") |>
    gt::data_color(columns =  c(Chg_2014:Chg_2023), target_columns = c(2:11),
               palette = c(color_down,color_up),
               na_color = "black") %>%
    gt::tab_options(table.background.color = "black",
                table.border.top.width=0,
                column_labels.padding = gt::px(35),
                heading.padding = gt::px(35)) |>
    gt::cols_label(
      Month = ""  ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right", "left"),
        color = "black",
        weight = gt::px(5),
        style = "solid"
      ),
      locations = gt::cells_body()) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("bottom","top"),
        color = "transparent",
        weight = gt::px(0)
      ),
      locations = gt::cells_body()
    ) |>
    gt::opt_horizontal_padding(scale = 3) |>
    gt::opt_vertical_padding(scale = 1.3)  %>%
    gt::grand_summary_rows(
      columns = c(2:11),
      fns = list(
        Yearly ~ mean(., na.rm = TRUE)
      ),
      fmt = ~ gt::fmt_number(., use_seps = FALSE)
    ) %>%
    gt::tab_header(
      title = "Unemployment Rate") %>%
    gt::gtsave("unemp_heatmap.png")

}

#' Creates a Heatmap of Unemployment in the United States
#'
#' @param fred_key A FRED API KEy
#'
#' @return A PNG Table
#' @export
#'
#' @examples
#' create_unemp_heatmap()
create_unemp_heatmap <- function(
    fred_key = readline(prompt = "Enter FRED API Key: ")) {

  # Loading pacman, libraries from CRAN, libraries from Github
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load('tidyverse','fredr','gt')

  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr_set_key(fred_key)

  data <- get_unemployment(10) |>
    transform_to_tsibble() |>
    mutate(Month = month(date, label = TRUE),
           Year = year(date)) |>
    as_tibble()

  first_month <- data |> select(Month) |> slice(1) |> pull()
  first_year <- data |> select(Year) |> slice(1) |> pull()
  if (first_month != "Jan") {
    data <- data |> filter(Year > first_year)
  }

  data <- data |> mutate(change = USUR - lag(USUR)) %>%
    mutate(change = if_else(change > 0, 1, -1))

  table_data <- data |>
    select(-date, -change) |>
    arrange(Month) |>
    pivot_wider(names_from = Year, values_from = USUR)


  table_data_chg <- data |>
    select(-date, -USUR) |>
    arrange(Month) %>%
    pivot_wider(names_from = Year, values_from = change, names_prefix = "Chg_") %>%
    select(-1)

  bind_cols(table_data, table_data_chg) %>%
    rename_with(~str_replace(.,"^20","'")) |>
    gt(rowname_col = "Month") |>
    cols_hide(c(Chg_2014:Chg_2023)) %>%
    sub_missing(missing_text = "") |>
    # data_color(
    #   columns = -Month,
    #   direction = "column",
    #   palette = c("green","red"),
    #   na_color = "black"
    #   ) |>
    data_color(columns =  c(Chg_2014:Chg_2023), target_columns = c(2:11),
               palette = c("#046438","#d40509"),
               na_color = "black") %>%
    tab_options(table.background.color = "black",
                table.border.top.width=0,
                column_labels.padding = px(35),
                heading.padding = px(35)) |>
    cols_label(
      Month = ""  ) |>
    tab_style(
      style = cell_borders(
        sides = c("right", "left"),
        color = "black",
        weight = px(5),
        style = "solid"
      ),
      locations = cells_body()) |>
    tab_style(
      style = cell_borders(
        sides = c("bottom","top"),
        color = "transparent",
        weight = px(0)
      ),
      locations = cells_body()
    ) |>
    opt_horizontal_padding(scale = 3) |>
    opt_vertical_padding(scale = 1.3)  %>%
    grand_summary_rows(
      columns = c(2:11),
      fns = list(
        Yearly ~ mean(., na.rm = TRUE)
      ),
      fmt = ~ fmt_number(., use_seps = FALSE)
    ) %>%
    tab_header(
      title = "Unemployment Rate") %>%
    gtsave("unemp_heatmap.png")

}

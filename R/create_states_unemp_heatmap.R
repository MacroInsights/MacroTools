create_states_unemp_heatmap <- function(
    fred_key = fredKey
) {

  # ------------------------------------------------------------------
  # Geometry handling (critical for tmap + census shapes)
  # ------------------------------------------------------------------
  sf::sf_use_s2(FALSE)
  tmap::tmap_mode("plot")

  # ------------------------------------------------------------------
  # FRED API
  # ------------------------------------------------------------------
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  # ------------------------------------------------------------------
  # State unemployment (latest available per state)
  # ------------------------------------------------------------------
  states_ur <- get_unemployment(geography = "State", latest = TRUE) |>
    transform_to_tsibble() |>
    dplyr::select(-USUR) |>
    tidyr::pivot_longer(-date, names_to = "series_id") |>
    dplyr::transmute(
      unemployment = value,
      abb = substr(series_id, 1, 2),
      name = cdlTools::fips(abb, to = "Name")
    )

  # National unemployment rate
  UR <- get_unemployment(geography = "State", latest = TRUE) |>
    transform_to_tsibble() |>
    dplyr::pull(USUR)

  states_data <- states_ur |>
    dplyr::mutate(estimate = unemployment - UR)

  # ------------------------------------------------------------------
  # State geometry (ACS + shifted AK/HI)
  # ------------------------------------------------------------------
  states_geo <- suppressMessages(
    tidycensus::get_acs(
      geography = "state",
      variables = c(households2021 = "B11012_001"),
      survey = "acs1",
      year = 2021,
      state = c(state.abb, "DC", "PR"),
      geometry = TRUE,
      cache_table = TRUE,
      output = "wide"
    )
  )

  states_geo <- tigris::shift_geometry(states_geo) |>
    dplyr::mutate(name = NAME)

  states_map <- states_geo |>
    dplyr::inner_join(states_data, by = "name")

  # ------------------------------------------------------------------
  # Tighter bounding box (ZOOM IN a bit to reduce whitespace)
  # ------------------------------------------------------------------
  bb <- sf::st_bbox(states_map)
  xr <- as.numeric(bb["xmax"] - bb["xmin"])
  yr <- as.numeric(bb["ymax"] - bb["ymin"])

  # shrink bbox ~3% on each side (zoom in)
  bb_tight <- bb
  bb_tight["xmin"] <- bb["xmin"] + 0.03 * xr
  bb_tight["xmax"] <- bb["xmax"] - 0.03 * xr
  bb_tight["ymin"] <- bb["ymin"] + 0.03 * yr
  bb_tight["ymax"] <- bb["ymax"] - 0.03 * yr

  # ------------------------------------------------------------------
  # Color scale (fixed bins) + HUMAN-READABLE LEGEND LABELS
  # ------------------------------------------------------------------
  fill_scale <- tmap::tm_scale_intervals(
    style  = "fixed",
    breaks = c(-Inf, -1, -0.5, 0, 0.5, 1, 1.5, Inf),
    values = c(
      "#C77417", "#FDAF54", "#E6C8A5",
      "#6ECCF9", "#2A9BCE", "#136598", "#0F3562"
    ),
    # labels correspond to each interval between breaks (length = breaks - 1)
    labels = c(
      "Less than -1.0",
      "-1.0 to -0.5",
      "-0.5 to 0.0",
      "0.0 to 0.5",
      "0.5 to 1.0",
      "1.0 to 1.5",
      "1.5 or more"
    ),
    midpoint = NA
  )
  # tm_scale_intervals supports explicit interval labels via `labels=`. [1](https://github.com/r-tmap/tmap/issues/47)[2](https://rdrr.io/cran/tmap/man/tm_text.html)

  fill_legend <- tmap::tm_legend(
    title = paste0(
      "Percentage Points from the National ",
      UR, "% Unemployment Rate"
    ),
    orientation = "landscape",
    position = tmap::tm_pos_out("center", "bottom"),
    frame = FALSE,
    title.fontface = "italic",
    text.size = 0.55,
    title.size = 1
  )

  # ------------------------------------------------------------------
  # MAP
  # ------------------------------------------------------------------
  statesMap <-
    tmap::tm_shape(states_map, bbox = bb_tight) +  # bbox supported in tm_shape [3](https://www.rdocumentation.org/packages/tmap/versions/4.1/topics/tm_check_fix)[4](https://rdrr.io/cran/tmap/f/NEWS.md)
    tmap::tm_fill(
      fill = "estimate",
      fill.scale = fill_scale,
      fill.legend = fill_legend
    ) +
    tmap::tm_borders(col = "white", lwd = 1) +
    tmap::tm_text(
      "abb",
      size = 0.45,
      options = tmap::opt_tm_text(point.label = TRUE)
    ) +
    tmap::tm_title(
      "Unemployment Rate Differential",
      position = tmap::tm_pos_out("center", "top"),
      fontface = "bold",
      fontfamily = "serif",
      size = 1.4
    ) +
    tmap::tm_layout(
      frame = FALSE,
      bg.color = "white",

      # tighter outer margins => bigger map panel
      # (bottom,left,top,right)
      outer.margins = c(0.04, 0.01, 0.06, 0.01),

      # small inner margins
      inner.margins = c(0.005, 0.005, 0.005, 0.005),

      text.fontfamily = "serif"
    )

  # ------------------------------------------------------------------
  # SAVE
  # ------------------------------------------------------------------
  tmap::tmap_save(
    statesMap,
    "states_map.png",
    width = 6,
    height = 4,
    dpi = 600
  )

  invisible(statesMap)
}

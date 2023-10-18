#' Creates a Clickable Map of the United States
#'
#' @param states_urls A CSV File containing the URLs for state fact sheets
#' @param fred_key A FRED API Key
#'
#' @return an HTML Map
#' @export
#'
#' @examples
#' create_states_clickable_map()
create_states_clickable_map <- function(
    states_urls = choose.files(caption = "Select CSV file containing State URLs",
                               multi = FALSE),
    fred_key = readline(prompt = "Enter FRED API Key: ")) {


  # Setting FRED API Key
  fred_key <- gsub("\"", "", fred_key)
  fredr::fredr_set_key(fred_key)

  # JavaScript Code that Makes the Map Clickable
  jsCode <- paste0('
   function(el, x, data) {
    var marker = document.getElementsByClassName("leaflet-interactive");
    for(var i=0; i < marker.length; i++){
      (function(){
        var v = data.win_url[i];
        marker[i].addEventListener("click", function() { window.open(v);}, false);
    }());
    }
   }
  ')

  # Get latest state unemployment figures from FRED and JECTools
  unemployment <- get_unemployment(years = 1, geography = 'State') %>%
    transform_to_tsibble() |>
    as_tibble() |>
    select(-USUR) |>
    drop_na() |>
    tail(1)
  print(paste("Data is updated as of",unemployment %>% select(date) %>% pull() %>% zoo::as.yearmon()))

  unemployment %>%
    pivot_longer(-date, names_to = 'State', values_to = 'UR') |>
    mutate(FIP = substr(State, start = 1, stop = 2),
           names = cdlTools::fips(FIP, to = "Name")) |>
    transmute(names = names,
              unemployment = UR) -> states_ur

  # Read URLs for states factsheets
  states_urls <- read_csv(states_urls, show_col_types = FALSE)
  # Merges the unemploymend and urls data
  states_data <- states_urls %>% inner_join(states_ur)
  # Merges the states data with geography
  states_map <- albersusa::usa_sf("lcc") %>% inner_join(states_data)

  # Colors
  # Sets the heatmap to Blue by quantiles
  state_palette <- leaflet::colorQuantile("Blues",
                                 states_map$unemployment)
  # Sets the background color of the to Transparent
  backg <- htmltools::tags$style(".leaflet-container { background: none; }")

  # Creates the map and saves it as HTML
  states_map %>%
    leaflet::leaflet(options = leaflet::leafletOptions(crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple"),
                                     minZoom = -100), width = "100%") %>%
    leaflet::addPolygons(data = states_map,
                weight = 2,
                label = ~paste0(name,": ",unemployment,"%"),
                color = "black",
                fillColor = ~state_palette(unemployment)) %>%
    htmlwidgets::prependContent(backg) %>%
    htmlwidgets::onRender(jsCode, data=states_map) %>%
    htmlwidgets::saveWidget(paste0(gsub(" ", "_", data_date),"_map.html"))

}

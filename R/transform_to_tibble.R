#' It transforms an xts object to a tibble
#'
#' @param xts_obj The xts object to transform
#'
#' @return A tibble
#' @export
#'
#' @examples
#' data_tibble <- get_unemployment() |> transform_to_tibble()
transform_to_tibble <- function(data = xts_obj) {
  # Convert xts to tibble
  tibble_obj <- tidyr::tibble(date = zoo::index(data),
                              as.data.frame(zoo::coredata(data)))

  return(tibble_obj)
}

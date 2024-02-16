#' It transforms an xts object to a tsibble
#'
#' @param xts_obj The xts object to transform
#'
#' @return A tsibble
#' @export
#'
#' @examples
#' data_tsibble <- get_unemployment() |> transform_to_tsibble()
transform_to_tsibble <- function(data = xts_obj) {
  # Convert xts to tibble
  tibble_obj <- tidyr::tibble(date = zoo::index(data),
                              as.data.frame(zoo::coredata(data)))

  # Convert tibble to tsibble
  tsibble_obj <- tsibble::as_tsibble(tibble_obj, index = date)

  return(tsibble_obj)
}

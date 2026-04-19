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
  tsibble_obj <- tsibble::as_tsibble(xts_to_tibble_base(data), index = date)

  return(tsibble_obj)
}

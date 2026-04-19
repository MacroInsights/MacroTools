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
  return(xts_to_tibble_base(data))
}

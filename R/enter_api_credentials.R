#' A script to enter API keys for the JECTools package
#'
#' @param fred_key An API key from FRED
#' @param bls_key  An API key from BLS
#'
#' @return It creates the API keys in the global environment. Maybe not the best practice
#' @export
#'
#' @examples
#' enter_api_credentials()
enter_api_credentials <- function(
    fred_key = readline(prompt = "Enter FRED API key:"),
    bls_key  = readline(prompt = "Enter BLS API key:")) {
  fredKey <<- fred_key
  blsKey <<- bls_key
}

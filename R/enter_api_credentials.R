enter_api_credentials <- function(
    fred_key = readline(prompt = "Enter FRED API key:"),
    bls_key  = readline(prompt = "Enter BLS API key:")) {
  fredKey <<- fred_key
  blsKey <<- bls_key
}

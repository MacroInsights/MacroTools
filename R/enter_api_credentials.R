#' A script to load or save API keys for the JECTools package. If the keys are already saved, it loads them into the environment. If they are not loaded, it produces a message to insert them.
#' #############################################################
#' NOTE: After saving an API Key, YOU MUST RESTART THE R SESSION
#' #############################################################
#'
#' @param fred_key An API key from FRED
#' @param bls_key  An API key from BLS
#'
#' @return It creates the API keys in the global environment. Maybe not the best practice
#' @export
#'
#' @examples
#' enter_api_credentials()  # Loads already saved API Keys into the environment
#' enter_api_credentials(fred_api_key = "FRED_API_KEY_GOES_HERE")  # Saves or updates the FRED KEY
#' enter_api_credentials(bls_api_key = "BLS_API_KEY_GOES_HERE")  # Saves or updates the BLS KEY
enter_api_credentials <- function(fred_api_key = NULL, bls_api_key = NULL) {

  update_renviron_contents <- function(contents, key, value) {
    key_pattern <- paste("^", key, "=", sep="")
    matched = FALSE

    for (i in seq_along(contents)) {
      if (grepl(key_pattern, contents[i])) {
        contents[i] <- paste(key, "=", value, sep="")
        matched = TRUE
        break
      }
    }

    if (!matched) {
      contents <- c(contents, paste(key, "=", value, sep=""))
    }

    return(contents)
  }


  api_keys <- list(FRED_API_KEY = fred_api_key, BLS_API_KEY = bls_api_key)
  renviron_path <- file.path(normalizePath("~"), ".Renviron")

  # Attempt to read the existing .Renviron file, if it exists
  if (file.exists(renviron_path)) {
    renviron_contents <- readLines(renviron_path, warn = FALSE)
  } else {
    renviron_contents <- character()
  }

  for (api_key_name in names(api_keys)) {
    new_value <- api_keys[[api_key_name]]
    existing_value <- Sys.getenv(api_key_name)

    if (!is.null(new_value)) {
      # Correctly use Sys.setenv to dynamically set an environment variable
      Sys.setenv(api_key_name = new_value)
      message(paste(api_key_name, "will be updated."))
      renviron_contents <- update_renviron_contents(renviron_contents, api_key_name, new_value)
    } else if (existing_value == "") {
      new_value <- readline(prompt = paste("Enter your", api_key_name, ": "))
      Sys.setenv(api_key_name = new_value)
      renviron_contents <- update_renviron_contents(renviron_contents, api_key_name, new_value)
      message(paste(api_key_name, "saved to .Renviron. Please restart your R session."))
    } else {
      message(paste(api_key_name, "already set in the environment. No action needed."))
    }
  }

  # Write the updated contents back to the .Renviron file
  writeLines(c(renviron_contents, ""), renviron_path)

  fredKey <<- Sys.getenv("FRED_API_KEY")
  blsKey <<- Sys.getenv("BLS_API_KEY")

  message("API keys are now available in the global environment as 'fredKey' and 'blsKey'.")
}

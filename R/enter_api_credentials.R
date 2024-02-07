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
enter_api_credentials <- function(fredKey = Sys.getenv("FRED_API_KEY"),
                                  blsKey = Sys.getenv("BLS_API_KEY"),
                                  overwrite = FALSE, install = FALSE)
{
  bls_api_key <- function(key = blsKey, overwrite, install) {

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("BLS_API_KEY", oldenv),]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("BLS_API_KEY",tv))){
          stop("A BLS_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("BLS_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("BLS_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(key)
  } else {
    message("To install your BLS API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(BLS_API_KEY = key)
  }
  }

  fred_api_key <- function(key = blsKey, overwrite, install) {

    if (install) {
      home <- Sys.getenv("HOME")
      renv <- file.path(home, ".Renviron")
      if(file.exists(renv)){
        # Backup original .Renviron before doing anything else here.
        file.copy(renv, file.path(home, ".Renviron_backup"))
      }
      if(!file.exists(renv)){
        file.create(renv)
      }
      else{
        if(isTRUE(overwrite)){
          message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
          oldenv=read.table(renv, stringsAsFactors = FALSE)
          newenv <- oldenv[-grep("FRED_API_KEY", oldenv),]
          write.table(newenv, renv, quote = FALSE, sep = "\n",
                      col.names = FALSE, row.names = FALSE)
        }
        else{
          tv <- readLines(renv)
          if(any(grepl("FRED_API_KEY",tv))){
            stop("A FRED_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
          }
        }
      }

      keyconcat <- paste0("FRED_API_KEY='", key, "'")
      # Append API key to .Renviron file
      write(keyconcat, renv, sep = "\n", append = TRUE)
      message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("FRED_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
      return(key)
    } else {
      message("To install your FRED API key for use in future sessions, run this function with `install = TRUE`.")
      Sys.setenv(FRED_API_KEY = key)
    }
  }

  bls_api_key(key = blsKey, overwrite, install)
  fred_api_key(key = fredKey, overwrite, install)

  fredKey <<- Sys.getenv("FRED_API_KEY")
  blsKey <<- Sys.getenv("BLS_API_KEY")



}

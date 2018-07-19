gemrtables <- function(region = "SDG", export = FALSE, format = "wide", key, password) {

  pkg.env <- new.env(parent = emptyenv())

  if (missing(key)) {
    stop("error: key for UIS API not specified")
  } else {
  pkg.env$key <- as.character(key)
  }

  if (missing(password)) {
    stop("error: password for cedar sql database not specified")
  } else {
    pkg.env$password <- as.character(password)
  }

  region = as.name(region)

  source()


}





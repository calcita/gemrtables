gemrtables <- function(region = "SDG", ref_year, export = FALSE, format = "wide", key, password) {

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

  ref_year <- ifelse(missing(ref_year), year(Sys.Date())-2, as.numeric(ref_year))

  region = as.name(region)

  source()


}





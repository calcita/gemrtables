#' gemrtables: A package for generating the Global Education Monitoring (GEM)
#' Report statistical tables.
#'
#' \code{\link{gemrtables}} is the primary function for this package, generating
#' the statistical tables in a 'wide' data format in excel, or a 'long' format
#' data frame.
#'
#' In addition, the gemrtables package provides three categories of functions:
#' import, import/clean, clean and summarise.
#'
#' @section import functions:
#' The import functions read data from SDMX url queries or the GEM cedar SQL
#' database.
#'
#' @section clean functions:
#' Clean functions sort data into a standard 'long' format with six columns:
#'
#' \enumerate{
#'   \item iso2c: ISO 3166-1 alpha-2 country code
#'   \item year: year of observation
#'   \item ind: standardised indicator name, matching with that in the inds()
#'   data frame (for indicators to be included in the stat tables)
#'   \item value: observation value
#'   \item val_status: observation status
#'   \item source of the observation (matching to the inds() data frame)
#' }
#' In addition several clean function compute new observations.
#'
#' @section summarise functions:
#' summarise functions compute new observations -aggregates and parity indices
#'
#' @section import/clean functions:
#' These functions largely serve as a wrapper for import and clean functions.
#'
#' @import utils
#'
#' @docType package
#' @name gemrtables
NULL

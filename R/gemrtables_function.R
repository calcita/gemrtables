#if(isTRUE(exists(".gemrtables.pkg.env", mode="environment"))) {
#  .gemrtables.pkg.env.SAVED <- .gemrtables.pkg.env
#  print("Existing environment '.gemrtables.pkg.env' saved as '.gemrtables.pkg.env.SAVED'!")
#}


#' gemrtables
#'
#' \code{gemrtables} is the main function to generate the UNESCO Global
#' Education Monitoring (GEM) Report Statistical tables, returning a dataframe
#' or an xlsx workbook
#'
#' @param region regional grouping on which to compute aggregate values. Options
#'   are `GEMR.region`, `UIS.region` or `SDG.region`. Defaults to `SDG.region`.
#' @param ref_year A four digit numeric of the reference year. Default is two
#'   years prior to the current year.
#' @param export If `TRUE` returns an xlsx workbook in 'wide' format, with
#'   separate worksheets per table.
#' If `FALSE` (the default) returns a data frame in 'long' format.
#' @param path File path to write xlsx workbook (character). Overwrites existing
#'   file.
#' @param key UIS api subscription key.
#' @param level_country If it is TRUE, parity indices are calculated at the country level
#' @param drake If it is TRUE, the drake plan is run instead load or generate data
#' @param removeCache Character vector of unprocessed dataframes. Options are c("uis_up", "wb_up", "eurostat_up", "oecd_up")
#' @param pc_flag_cut threshold
#' @param pc_comp_cut2 threshold
#' @importFrom dplyr select filter %>%
#' @importFrom writexl write_xlsx
#' @importFrom R.cache setCacheRootPath findCache
#' @return A data frame or an xlsx workbook.
#' @export
#' @examples
#' gemrtables(ref_year = 2016, export = TRUE, path = x, key = y)
#'
#' df <- gemrtables(region = "UIS.region", ref_year = 2016, export = FALSE, key = y)

gemrtables <- function(
  region = "SDG.region",
  ref_year,
  export = TRUE,
  path,
  key,
  pc_flag_cut = 66,
  pc_comp_cut2 = 33,
  level_country = TRUE,
  drake = TRUE,
  removeCache
  ) {

  #define package environment and package wide parameters

  .gemrtables.pkg.env <<- new.env(parent = emptyenv())
  # rm(list = ls(envir = .gemrtables.pkg.env), envir = .gemrtables.pkg.env)

  .gemrtables.pkg.env$ref_year <- ifelse(missing(ref_year), lubridate::year(Sys.Date())-2, as.numeric(ref_year))

  .gemrtables.pkg.env$region = as.name(region)

  .gemrtables.pkg.env$pc_flag_cut <- pc_flag_cut

  .gemrtables.pkg.env$pc_comp_cut2 <- pc_comp_cut2

  .gemrtables.pkg.env$level_country <- level_country

  if(region == "SDG.region") {
    .gemrtables.pkg.env$subregion <- as.name("SDG.subregion")
  }else if(region == "UIS.region") {
    .gemrtables.pkg.env$subregion <- as.name("UIS.subregion")
  }else if(region == "GEMR.region") {
    .gemrtables.pkg.env$subregion <- as.name("GEMR.subregion")
  }

  #import / generate other merge files
  .gemrtables.pkg.env$indicators <- inds()
  .gemrtables.pkg.env$regions <- region_groups()
  .gemrtables.pkg.env$indicators_unique <- .gemrtables.pkg.env$indicators %>%
    dplyr::select(-source, -var_concat, -priority, -ind_lab) %>%
    unique()
  .gemrtables.pkg.env$regions2 <- region_groups2() %>%
    dplyr::filter(grouping == as.character(.gemrtables.pkg.env$region))

  #Set directory for cache (users current working directory)

  dir.create(path="./.Rcache", showWarnings=FALSE)
  R.cache::setCacheRootPath(path="./.Rcache")

  if(isFALSE(drake)){

  if(!missing(removeCache)) {
    for(i in 1:length(removeCache)) {
      file.remove(R.cache::findCache(key=list(removeCache[i])))
    }
  }

  #define api and db keys.

  if (missing(key)) {
    stop("Key for UIS API is missing")
  } else {
    .gemrtables.pkg.env$key <- as.character(key)
  }

  if (isTRUE(export)) {
    if (missing(path)) {
      stop("file path not specified")
    } else {
      path <- as.character(path)
    }
  }

  long_data <- format_long()

  wide_data <- long_data %>%
    format_wide()

  if(nrow(.gemrtables.pkg.env$unmatched > 0)) {
    message(glue::glue("The following variables are missing:"))
    cat(paste(capture.output(print(.gemrtables.pkg.env$unmatched)), collapse = "\n"))
  }

  if(isTRUE(export)) {
    writexl::write_xlsx(wide_data, path = path)
  }else {
    return(long_data)
  }
 }

}

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
#'   seperate worksheets per table.
#' If `FALSE` (the default) returns an data frame in 'long' format.
#' @param path File path to write xlsx workbook (character). Overwrites existing
#'   file.
#' @param key UIS api subcription key.
#' @param password password to cedar database.
#' @return A data frame or an xlsx workbook.
#' @export
#' @examples
#' gemrtables(ref_year = 2016, export = TRUE, path = x, key = y, password = z)
#'
#' df <- gemrtables(region = "UIS.region", ref_year = 2016, export = FALSE, key = y, password = z)

gemrtables <- function(region = "SDG.region", ref_year, export = FALSE, path, key, password) {

  ref_year <- ifelse(missing(ref_year), lubridate::year(Sys.Date())-2, as.numeric(ref_year))
  region_type = as.name(region)

    #define new environment for api and db keys. Set directory for cache (users current working directory)

  pkg.env <<- new.env(parent = emptyenv())
  dir.create(path="~/.Rcache", showWarnings=FALSE)
  R.cache::setCacheRootPath(path="./.Rcache")

  if (missing(key)) {
    stop("Key for UIS API is missing")
  } else {
    pkg.env$key <- as.character(key)
  }

  if (missing(password)) {
    stop("Password for cedar sql database is missing")
  } else {
    pkg.env$password <- as.character(password)
  }

  if (missing(path) & isTRUE(export)) {
    stop("file path not specified")
  } else {
    path <- as.character(path)
  }

  #import / generate other merge files
  indicators <- inds()
  regions <- region_groups()
  indicators_unique <- indicators %>%
    dplyr::select(-source, -var_concat, - priority, -ind_lab) %>%
    unique()

  #load/ cache for imported/cleaned country data and weights

  # function to generate country_data
  c_data <- function() {

    uis_data <- uis()
    cedar_data <- cedar()
    other_data <- other()
    dplyr::bind_rows(uis_data, cedar_data, other_data)
  }

  load_cache_data <- function(df) {

    # 1. Try to load cached data, if already generated
    key <- list(df)
    data <- R.cache::loadCache(key)
    if (!is.null(data)) {
      cat(paste("Loaded cached", df, "\n", sep = " " ))
      return(data);
    }
    # 2. If not available, generate it.
    cat(paste("Generating", df, "from scratch...\n", sep = " "))
    if(df == "country_data") {
      data <- c_data()
    }
    if(df == "weights_data") {
      data <- weights()
    }
    R.cache::saveCache(data, key=key, comment= df)
    data;
  }

  country_data <- load_cache_data("country_data")
  weights_data <- load_cache_data("weights_data")

  #clean country data and export statistical tables

  country_data1 <- country_data %>%
    dplyr::right_join(regions, by = "iso2c") %>%
    dplyr::left_join(indicators, by = c("ind", "source")) %>%
    dplyr::filter(year >= ref_year - year_cut)

  unmatched <- dplyr::anti_join(indicators, country_data1, by = c("ind", "source")) %>%
    dplyr::select(ind, source, sheet, position)

  country_data2 <- country_data1 %>%
    dplyr::select(iso2c, annex_name, SDG.region, income_group, year, ind, value, val_status, source) %>%
    dplyr::right_join(indicators, by = c("ind", "source")) %>%
    dplyr::group_by(iso2c, ind, source) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(priority == min(priority)) %>%
    dplyr::left_join(weights_data[, -2], by = c("iso2c", "wt_var")) %>%
    dplyr::mutate(wt_value = ifelse(aggregation != "w_mean", 0, wt_value),
           entity = "country") %>%
    ungroup()

  regional_aggregates <- country_data2 %>%
    aggregates() %>%
    dplyr::filter(!is.na(annex_name)) %>%
    dplyr::inner_join(indicators_unique, by = c("ind", "aggregation"))

  long_data <- dplyr::bind_rows(country_data2, regional_aggregates) %>%
    dplyr::mutate(entity = factor(entity, levels = c("country", "world", "region", "income_group"))) %>%
    dplyr::arrange(sheet, position, SDG.region, entity, annex_name)

  wide_data <- long_data %>%
    format_wide()

  if(nrow(unmatched > 0)) {
    cat(paste("The following variables are missing:\n"))
    cat(paste(capture.output(print(unmatched)), collapse = "\n"))
  }

  if(isTRUE(export)) {
    writexl::write_xlsx(wide_data, path = path)
  }else {
    return(long_data)
  }

}

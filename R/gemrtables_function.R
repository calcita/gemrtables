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
#' If `FALSE` (the default) returns a data frame in 'long' format.
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

  if(!isTRUE(exists("pkg.env", mode="environment"))) {
  pkg.env <<- new.env(parent = emptyenv())
  }
  #define regions and ref_year

  pkg.env$ref_year <- ifelse(missing(ref_year), lubridate::year(Sys.Date())-2, as.numeric(ref_year))

  pkg.env$region = as.name(region)

  if(region == "SDG.region") {
    pkg.env$subregion <- as.name("SDG.subregion")
  }else if(region == "UIS.region") {
    pkg.env$subregion <- as.name("UIS.subregion")
  }else if(region == "GEMR.region") {
    pkg.env$subregion <- as.name("GEMR.subregion")
  }


  #define api and db keys. Set directory for cache (users current working directory)

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
  pkg.env$indicators <- inds()
  pkg.env$regions <- region_groups()
  indicators_unique <- pkg.env$indicators %>%
    dplyr::select(-source, -var_concat, -priority, -ind_lab) %>%
    unique()
  pkg.env$regions2 <- region_groups2() %>%
    dplyr::filter(grouping == as.character(pkg.env$region))

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
  weights_data <-
    load_cache_data("weights_data") %>%
    dplyr::group_by(wt_var) %>%
    dplyr::mutate(wt_total = sum(wt_value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  #clean country data and export statistical tables

  country_data1 <- country_data %>%
    dplyr::right_join(pkg.env$regions, by = "iso2c") %>%
    dplyr::left_join(pkg.env$indicators, by = c("ind", "source")) %>%
    dplyr::filter(year >= pkg.env$ref_year - year_cut)

  unmatched <- dplyr::anti_join(pkg.env$indicators, country_data1, by = c("ind", "source")) %>%
    dplyr::select(ind, source, sheet, position)

  country_data2 <- country_data1 %>%
    dplyr::select(iso2c, annex_name, World, !!pkg.env$region, !!pkg.env$subregion, income_group, income_subgroup, year, ind, value, val_status, source) %>%
    dplyr::right_join(pkg.env$indicators, by = c("ind", "source")) %>%
    dplyr::group_by(iso2c, ind, source) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(priority == min(priority)) %>%
    dplyr::left_join(weights_data[, -3], by = c("iso2c", "wt_var")) %>%
    dplyr::mutate(wt_value = ifelse(aggregation != "w_mean", 1, wt_value), entity = "country") %>%
    ungroup()

  uis_aggregates <- pkg.env$uis_comp %>%
    dplyr::inner_join(pkg.env$indicators, by = "var_concat") %>%
    dplyr::filter(aggregation %in% c("w_mean", "sum") & year >= (ref_year - 4)) %>%
    dplyr::inner_join(pkg.env$regions2[, 1:3], by = c("iso2c" = "code")) %>%
    dplyr::select(-iso2c)

  computed_aggregates <- country_data2 %>%
    aggregates() %>%
    dplyr::filter(!is.na(annex_name)) %>%
    dplyr::inner_join(indicators_unique, by = c("ind", "aggregation", "pc_comp_cut")) %>%
    dplyr::anti_join(uis_aggregates, by = c("annex_name", "ind")) %>%
    dplyr::mutate(value = dplyr::case_when(annex_name == "World" & ind == "odaflow.volumescholarship" ~ value + pkg.env$schol_unspec[1,2],
                                           annex_name == "World" & ind == "odaflow.imputecost" ~ value + pkg.env$schol_unspec[2,2],
                                           TRUE ~ value))

  long_data <<- dplyr::bind_rows(country_data2, computed_aggregates, uis_aggregates)  %>%
    tidyr:: complete(tidyr::nesting(ind, sheet, position), tidyr::nesting(annex_name, !!pkg.env$region, !!pkg.env$subregion, entity),
    fill = list(value = NA, val_status = "", year_diff = 0)) %>%
    dplyr::mutate(value = ifelse(stringr::str_detect(ind, stringr::regex("admi", ignore_case = TRUE)) & entity == "country" & is.na(value), 0, value)) %>%
    dplyr::arrange(sheet, position, !!pkg.env$region, entity, annex_name)

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

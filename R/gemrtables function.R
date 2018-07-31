
gemrtables <- function(region = "SDG", ref_year, export = FALSE, path, format = "wide", key, password) {

  ref_year <- ifelse(missing(ref_year), year(Sys.Date())-2, as.numeric(ref_year))
  region_type = as.name(region)

    #define new environment for api and db keys

  pkg.env <<- new.env(parent = emptyenv())
  dir.create(path="~/.Rcache", showWarnings=FALSE)
  setCacheRootPath(path="./.Rcache")

  if (missing(key)) {
    stop("Key for UIS API is missing")
  } else {
    pkg.env$key <- "43ef903b32e74ebabe916007e9cf89e9"
  }

  if (missing(password)) {
    stop("Password for cedar sql database is missing")
  } else {
    pkg.env$password <- as.character(password)
  }

  #import / generate other merge files
  indicators <- inds()
  regions <- region_groups()
  indicators_unique <- indicators %>%
    select(-source, -var_concat, - priority, -ind_lab) %>%
    unique()

  #load/ cache for imported/cleaned country data and weights

  # function to generate country_data
  c_data <- function() {

    uis_data <- uis()
    #cedar_data <- cedar()
    other_data <- other()
    bind_rows(uis_data,  other_data)
  }

  load_data <- function(df) {

    # 1. Try to load cached data, if already generated
    key <- list(df)
    data <- loadCache(key)
    if (!is.null(data)) {
      cat("Loaded cached data\n")
      return(data);
    }
    # 2. If not available, generate it.
    cat(paste("Generating", df, "from scratch...", sep = " "))
    if(df == "country_data") {
      data <- c_data()
    }
    if(df == "weights_data") {
      data <- weights()
    }
    saveCache(data, key=key, comment= df)
    data;
  }

  country_data <- load_data("country_data")
  weights_data <- load_data("weights_data")

  #clean country data and export statistical tables

  country_data1 <- country_data %>%
    right_join(regions, by = "iso2c") %>%
    left_join(indicators, by = c("ind", "source")) %>%
    filter(year >= ref_year - year_cut)

  unmatched <- anti_join(indicators, country_data1, by = c("ind", "source")) %>%
    select(ind, source, sheet, position)

  country_data2 <- country_data1 %>%
    select(iso2c, annex_name, SDG.region, income_group, year, ind, value, val_status, source) %>%
    right_join(indicators, by = c("ind", "source")) %>%
    group_by(iso2c, ind, source) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    group_by(iso2c, ind) %>%
    filter(priority == min(priority)) %>%
    left_join(weights_data[, -2], by = c("iso2c", "wt_var")) %>%
    mutate(wt_value = ifelse(aggregation != "w_mean", 0, wt_value),
           entity = "country") %>%
    ungroup()

  regional_aggregates <- country_data2 %>%
    aggregates() %>%
    filter(!is.na(annex_name)) %>%
    inner_join(indicators_unique, by = c("ind", "aggregation"))

  long_data <- bind_rows(country_data2, regional_aggregates) %>%
    mutate(entity = factor(entity, levels = c("country", "region", "income_group"))) %>%
    arrange(sheet, position, SDG.region, entity, annex_name)

  wide_data <- long_data %>%
    format_wide()

  if(nrow(unmatched > 0)) {
    cat(paste("the following variables are missing:"))
    cat(paste(capture.output(print(unmatched)), collapse = "\n"))
  }

  if(isTRUE(export)) {
    write_xlsx(wide_data, path = path)
  }else {
    return(full_data)
  }

}

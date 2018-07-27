

pkg.env <- new.env(parent = emptyenv())

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

ref_year <- ifelse(missing(ref_year), year(Sys.Date())-2, as.numeric(ref_year))
region_type = as.name(region_type)

indicators <- inds()
regions <- region_groups()
uis_data <- uis()
cedar_data <- cedar()
other_data <- other()
weights_data <- weights()

indicators_unique <- indicators %>%
  select(-source, -var_concat, - priority, -ind_lab) %>%
  unique()

country_data1 <- bind_rows(uis_data, other_data) %>%
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
  warning( paste("the following variables are missing:", capture.output(print(unmatched)), collapse = "\n"))
}

if(isTRUE(export)) {
  write_xlsx(wide_data, path = "C:/Users/a_mc-william/Downloads/stat_tables.xlsx")
}else {
  return(full_data)
}

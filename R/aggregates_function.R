#' agg_preprocess
#'
#' \code{agg_preprocess} converts several country data variables to boolean in
#' order to caclulate the percentage of countries meeting a condition in the
#' aggregates tables
#'
#' @param df a data frame with a key / value columns.
#' @return A data frame.
#' @family clean


agg_preprocess <- function(df) {
  df %>%
    mutate(value = case_when(
       ind == "Comp.02" | ind == "Free.02"  ~ as.numeric(value >= 1),
       ind == "Comp.2t3" ~ as.numeric(value >= 9),
       ind == "Free.2t3" ~ as.numeric(value >= 12),
       ind ==  "stu.bully" ~ as.numeric(value == 3),
       stringr::str_detect(ind, "esd") ~ as.numeric(value >= 2),
       TRUE ~ value))
}

#' compute_aggregate
#'
#' \code{compute_aggregate} computes various summarise functions on country_data
#' for a specified aggregate grouping.
#'
#' @param df a data frame with a key / value columns.
#' @param region aggregate grouping on which to compute (corresponds to the
#'   relevent column in `.gemrtables.pkg.env$indicators`.
#' @param entity type of aggregate (character)
#' @return A data frame.
#' @family summarise

compute_aggregate <- function(df, region, entity) {

  wt <- .gemrtables.pkg.env$weights_data %>%
  filter(wt_region == region) %>%
  select(-wt_region) %>%
  identity

  region <- as.name(region)
  entity <- as.character(entity)

  computed <- df %>%
    agg_preprocess() %>%
    dplyr::left_join(wt, by = c('iso2c', 'wt_var')) %>%
    dplyr::group_by(!!region) %>%
    dplyr::mutate(count1 = n_distinct(iso2c)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!region, ind, count1, pc_comp_cut) %>%
    dplyr::summarise(sum = sum(value,  na.rm = TRUE),
                     median = median(value, na.rm = TRUE),
                     w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE), #weighted.mean(value, wt_value, na.rm = TRUE),
                     pc_true = (sum(value)/dplyr::n())*100,
                     #pc_true2 = (sum(value/count1*100)),
                     wt_share = 100 * sum(wt_value[!is.na(value) & val_status == 'A'], na.rm = TRUE)/wt_total[1],
                     pop_share = 100 * sum(pop[!is.na(value) & val_status == 'A'], na.rm = TRUE)/pop_total[1],
                     count2 = dplyr::n()) %>%
    dplyr::mutate(pc_comp = 100 * round(count2/count1, digits = 2),
                  entity = entity) %>%
    tidyr::gather(key = "aggregation", value = "value", -ind, -!!region, -pc_comp, -pc_comp_cut, -entity, -wt_share, -pop_share) %>%
    dplyr::select(annex_name = !!region, ind, aggregation, value, pc_comp, wt_share, pop_share, pc_comp_cut, entity)
}

#' aggregates
#'
#' \code{aggregates} Is a wrapper function for \code{compute_aggregate}.
#' Aggregates are computed for each type of grouping and binded. Redundant
#' aggregations are dropped through a join.
#'
#' @param df a data frame with a key / value columns.
#' @return A data frame.
#' @family summarise

aggregates <- function(df) {

  region <- as.character(.gemrtables.pkg.env$region)
  subregion <- as.character(.gemrtables.pkg.env$subregion)

  world <- compute_aggregate(df, region = "World", entity = "world")
  regional <- compute_aggregate(df, region = region, entity = "region")
  subregional <- compute_aggregate(df, region = subregion, entity = "subregion")
  income <- compute_aggregate(df, region = "income_group", entity = "income_group")
  subincome <- compute_aggregate(df, region = "income_subgroup", entity = "income_subgroup")

  aggregates <- dplyr::bind_rows(world, regional, subregional, income, subincome) %>%
    dplyr::semi_join(.gemrtables.pkg.env$indicators, by = c("ind", "aggregation")) %>%
    dplyr::mutate(year = .gemrtables.pkg.env$ref_year) %>%
    dplyr::filter(!(aggregation != 'w_mean' & pc_comp < .gemrtables.pkg.env$pc_comp_cut2) &
                  !(aggregation == 'w_mean' & wt_share < .gemrtables.pkg.env$pc_comp_cut2)) %>%
    dplyr::mutate(val_status = case_when(
      aggregation == 'sum' & (pc_comp < .gemrtables.pkg.env$pc_flag_cut | pop_share < 95) ~ 'E',
      aggregation != 'w_mean' & pc_comp < .gemrtables.pkg.env$pc_flag_cut ~ 'E',
      aggregation == 'w_mean' & pmin(wt_share, pop_share, na.rm = TRUE) < .gemrtables.pkg.env$pc_flag_cut ~ 'E',
      TRUE ~ "A"
    )) %>%
    dplyr::filter(!is.na(annex_name) | annex_name != "")
}

#' compute_aggregate_values
#'
#' \code{compute_aggregate_values} computes medians on country_data
#' for a specified aggregate grouping.
#'
#' @param df a data frame with a key / value columns.
#' @param region aggregate grouping on which to compute (corresponds to the
#'   relevent column in `.gemrtables.pkg.env$indicators`.
#' @param entity type of aggregate (character)
#' @return A data frame.
#' @family summarise


compute_aggregate_values <- function(df, region, entity){
  region <- as.name(region)
  entity <- as.character(entity)
  computed <- df %>%
    dplyr::group_by(var_concat,!!region) %>%
    summarise(value = median(value, na.rm = TRUE))
}

#' aggregates_values
#'
#' \code{aggregates_values} Is a wrapper function for \code{compute_aggregate_values}.
#' Aggregates values by medians at the region level.
#'
#' @param df a data frame with a key / value columns.
#' @return A data frame.
#' @family summarise

aggregates_values <- function(df){
  world <- compute_aggregate_values(df, region = "World", entity = "world")
  regional <- compute_aggregate_values(df, region = region, entity = "region")
  subregional <- compute_aggregate_values(df, region = .gemrtables.pkg.env$subregion, entity = "subregion")
  income <- compute_aggregate_values(df, region = "income_group", entity = "income_group")
  subincome <- compute_aggregate_values(df, region = "income_subgroup", entity = "income_subgroup")

  aggregates <- dplyr::bind_rows(world, regional, subregional, income, subincome) %>%
    dplyr::filter(!is.na(var_concat))

}

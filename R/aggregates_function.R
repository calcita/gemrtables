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

compute_aggregate <- function(df, region, entity) {

  region <- as.name(region)
  entity <- as.character(entity)

  computed <- df %>%
    agg_preprocess() %>%
    dplyr::group_by(!!region) %>%
    dplyr::mutate(count1 = n_distinct(iso2c)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!region, ind, count1, pc_comp_cut) %>%
    dplyr::summarise(sum = sum(value,  na.rm = TRUE),
                     median = median(value, na.rm = TRUE),
                     w_mean = weighted.mean(value, wt_value, na.rm = TRUE),#sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
                     pc_true = (sum(value)/dplyr::n())*100,
                     pc_true2 = (sum(value/count1*100)),
                     wt_share = sum(wt_value[!is.na(value) & val_status == 'A'], na.rm = TRUE)/wt_total[1],
                     count2 = dplyr::n()) %>%
    dplyr::mutate(pc_comp = 100 * round(count2/count1, digits = 2),
                  entity = entity) %>%
    tidyr::gather(key = "aggregation", value = "value", -ind, -!!region, -pc_comp, -pc_comp_cut, -entity, -wt_share) %>%
    dplyr::select(annex_name = !!region, ind, aggregation, value, pc_comp, wt_share, pc_comp_cut, entity)
}

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
    dplyr::filter(!(aggregation != 'w_mean' & pc_comp < pc_comp_cut) &
                  !(aggregation == 'w_mean' & wt_share < pc_comp_cut)) %>%
    dplyr::mutate(val_status = case_when(
      aggregation == 'sum' & pc_comp < 90 ~ 'E',
      aggregation != 'w_mean' & pc_comp < .gemrtables.pkg.env$pc_flag_cut ~ 'E',
      aggregation == 'w_mean' & wt_share < .gemrtables.pkg.env$pc_flag_cut ~ 'E',
      TRUE ~ "A"
    )) %>%
    dplyr::filter(!is.na(annex_name) | annex_name != "")
}



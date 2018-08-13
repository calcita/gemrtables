compute_aggregate <- function(df, region, entity) {

  region <- as.name(region)
  entity <- as.character(entity)

  computed <- df %>%
    dplyr::group_by(!!region) %>%
    dplyr::mutate(count1 = n_distinct(iso2c)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!region, ind, count1, pc_comp_cut) %>%
    dplyr::summarise(sum = sum(value,  na.rm = TRUE),
                     median = median(value, na.rm = TRUE),
                     w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
                     pc_true = (sum(value)/dplyr::n())*100,
                     pc_1 = (sum(ifelse(value >= 1, 1, 0))/dplyr::n())*100,
                     pc_2 = (sum(ifelse(value >= 2, 1, 0))/dplyr::n())*100,
                     pc_9 = (sum(ifelse(value >= 9, 1, 0))/dplyr::n())*100,
                     pc_12 = (sum(ifelse(value >= 12, 1, 0))/dplyr::n())*100,
                     count2 = dplyr::n()) %>%
    dplyr::mutate(pc_comp = 100 * round(count2/count1, digits = 2),
                  entity = entity) %>%
    tidyr::gather(key = "aggregation", value = "value", -ind, -!!region, -pc_comp, -pc_comp_cut, -as.name(entity)) %>%
    dplyr::select(annex_name = !!region, ind, aggregation, value, pc_comp, pc_comp_cut, entity)

}


aggregates <- function(df, region) {

  region <- as.name(region)

 if(region == "SDG.region") {
   subregion == as.name("SDG.subregion")
 }else if(region == "UIS.region") {
   subregion == as.name("UIS.subregion")
 }else if(region == "GEMR.region") {
   subregion == as.name("GEMR.subregion")
 }


  world <- compute_aggregate(df, region = "World", entity = "world")
  regional <- compute_aggregate(df, region = region)
  subregional <- compute_aggregate(df, region = subregion)
  income <- compute_aggregate(df, region = "income_group")

  aggregates <- dplyr::bind_rows(world, regional, subregional, income) %>%
    dplyr::semi_join(indicators, by = c("ind", "aggregation")) %>%
    dplyr::mutate(year = ref_year) %>%
    dplyr::filter(pc_comp >= pc_comp_cut)

}

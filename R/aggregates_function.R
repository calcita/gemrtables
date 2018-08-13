#' aggregates
#'
#' \code{aggregates} is a function to calculate income group and regional aggregates.
#'
#' Calculates, medians, weighted means, sums, and 'percentage true' aggregations (as
#' specified in the indicators dataframe). Takes \code{country_data2} (produced as part
#' of the \code{gemrtables} function) as an argument.
#' @param df data frame in the same format as \code{country_data2}
#' @family summarise
#' @seealso \code{\link{gemrtables}}

aggregates <- function(df_raw) {

<<<<<<< HEAD
  world <- df %>%
    dplyr::mutate(count1 = dplyr::n_distinct(iso2c)) %>%
    dplyr::group_by(ind, count1, pc_comp_cut) %>%
    dplyr::summarise(sum = sum(value),
                     median = median(value, na.rm = TRUE),
                     w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
                     pc_true = (sum(value)/dplyr::n())*100,
                     count2 = dplyr::n()) %>%
    dplyr::mutate(annex_name = "World", pc_comp = 100 * round(count2/count1, digits = 2),
                  entity = "world") %>%
    tidyr::gather(key = "aggregation", value = "value", -ind, - annex_name, -pc_comp, -pc_comp_cut, -entity)

  regional <- df %>%
    dplyr::group_by(SDG.region) %>%
    dplyr::mutate(count1 = n_distinct(iso2c)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SDG.region, ind, count1, pc_comp_cut) %>%
    dplyr::summarise(sum = sum(value,  na.rm = TRUE),
                     median = median(value, na.rm = TRUE),
                     w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
                     pc_true = (sum(value)/dplyr::n())*100,
                     count2 = dplyr::n()) %>%
    dplyr::mutate(pc_comp = 100 * round(count2/count1, digits = 2),
                  entity = "region") %>%
    tidyr::gather(key = "aggregation", value = "value", -ind, -SDG.region, -pc_comp, -pc_comp_cut, -entity) %>%
    dplyr::select(annex_name = SDG.region, ind, aggregation, value, pc_comp, pc_comp_cut, entity)

  income <- df %>%
    dplyr::group_by(income_group) %>%
    dplyr::mutate(count1 = dplyr::n_distinct(iso2c)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(income_group, ind, count1, pc_comp_cut) %>%
    dplyr::summarise(sum = sum(value),
                     median = median(value, na.rm = TRUE),
                     w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
                     pc_true = (sum(value)/dplyr::n())*100,
                     count2 = dplyr::n()) %>%
    dplyr::mutate(pc_comp = 100 * round(count2/count1, digits = 2),
                  entity = "income_group") %>%
    tidyr::gather(key = "aggregation", value = "value", -ind, -income_group, -pc_comp, -pc_comp_cut, -entity) %>%
    dplyr::select(annex_name = income_group, ind, aggregation, value, pc_comp, pc_comp_cut, entity)

  aggregates <- dplyr::bind_rows(world, regional, income) %>%
    dplyr::semi_join(indicators, by = c("ind", "aggregation")) %>%
    dplyr::mutate(year = ref_year) %>%
    dplyr::filter(pc_comp >= pc_comp_cut)
=======
  # This has been added, see function definition below
  df <- agg_preprocess(df)

  # I think all the aggregates can be replaced as followed
  generic_agg <- function(df) {
    df %>%
    mutate(count1 = n_distinct(iso2c)) %>%
    ungroup() %>%
    group_by(ind, count1, pc_comp_cut, add = TRUE) %>%
    summarise(sum = sum(value,  na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
              pc_true = (sum(value)/n())*100,
              count2 = n()) %>%
    mutate(pc_comp = 100 * round(count2/count1, digits = 2)) %>%
    gather(key = "aggregation", value = "value", -ind, -SDG.region, -pc_comp) %>%
    # The relevant variable (SDG.region, income_group) will continue to be implicitly
    # selected below because it is still a grouping variable
    select(ind, aggregation, value, pc_comp)
    # Do you rely on the current grouping further downstream?
    # If not (and it wouldn't be good practice), then the following 'cleanup' should be uncommented:
    # ungroup
  }
  # To be used so:
  world <- df %>%
    generic_agg %>%
    mutate(entity = "world") %>%
    mutate(annex_name = 'World')

  regional <- df %>%
    group_by(SDG.region) %>%
    generic_agg %>%
    mutate(entity = "region") %>%
    rename(annex_name = SDG.region)

  # Since subregions are interspersed with regions in the final table, I'm assuming they share an 'entity'?
  # If necessary, change the entity to 'subregion' if that's not the case.
  # N.B. 'SDG.subregion' still needs to be added to the indicators spreadsheet!
  subregional <- df %>%
    group_by(SDG.subregion) %>%
    generic_agg %>%
    mutate(entity = "region") %>%
    rename(annex_name = SDG.subregion)

  income <- df %>%
    group_by(income_group) %>%
    generic_agg %>%
    mutate(entity = "income_group") %>%
    rename(annex_name = income_group)

  # Defining a wrapper that works as, e.g., regional <- wrap_agg(SDG.region, 'region') doesn't seem
  # worth the trouble at this stage. Please do not spend any time trying to do so.

  # world <- df %>%
  #   mutate(count1 = n_distinct(iso2c)) %>%
  #   group_by(ind, count1, pc_comp_cut) %>%
  #   summarise(sum = sum(value),
  #             median = median(value, na.rm = TRUE),
  #             w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
  #             pc_true = (sum(value)/n())*100,
  #             count2 = n()) %>%
  #   mutate(annex_name = "World", pc_comp = 100 * round(count2/count1, digits = 2),
  #          entity = "world") %>%
  #   gather(key = "aggregation", value = "value", -ind, - annex_name, -pc_comp, -entity)
  #
  # regional <- df %>%
  #   group_by(SDG.region) %>%
  #   mutate(count1 = n_distinct(iso2c)) %>%
  #   ungroup() %>%
  #   group_by(SDG.region, ind, count1, pc_comp_cut) %>%
  #   summarise(sum = sum(value,  na.rm = TRUE),
  #             median = median(value, na.rm = TRUE),
  #             w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
  #             pc_true = (sum(value)/n())*100,
  #             count2 = n()) %>%
  #   mutate(pc_comp = 100 * round(count2/count1, digits = 2),
  #          entity = "region") %>%
  #   gather(key = "aggregation", value = "value", -ind, -SDG.region, -pc_comp, -entity) %>%
  #   select(annex_name = SDG.region, ind, aggregation, value, pc_comp, entity)
  #
  # income <- df %>%
  #   group_by(income_group) %>%
  #   mutate(count1 = n_distinct(iso2c)) %>%
  #   ungroup() %>%
  #   group_by(income_group, ind, count1, pc_comp_cut) %>%
  #   summarise(sum = sum(value),
  #             median = median(value, na.rm = TRUE),
  #             w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
  #             pc_true = (sum(value)/n())*100,
  #             count2 = n()) %>%
  #   mutate(pc_comp = 100 * round(count2/count1, digits = 2),
  #          entity = "income_group") %>%
  #   gather(key = "aggregation", value = "value", -ind, -income_group, -pc_comp, -entity) %>%
  #   select(annex_name = income_group, ind, aggregation, value, pc_comp, entity)

  aggregates <-
    # bind_rows(world, regional, income) %>%
    bind_rows(world, regional, subregional, income) %>%
    semi_join(indicators, by = c("ind", "aggregation")) %>%
    mutate(year = ref_year) %>%
    # N.B. I changed the following, to avoid dropping aggregates:
    # filter(pc_comp >= pc_comp_cut)
    mutate(value = ifelse(pc_comp >= pc_comp_cut, value, NA))

}
>>>>>>> 1b42925f035a1c31164dec72e0402713f094ed88

agg_preprocess <- function(df) {
  df %>%
  #
  # e.g. apply threshold to 'HIGH'/'MEDIUM'/'LOW' for conversion into boolean 1s/0s, or,
  # as in the example sketched below, to years of compulsory/free education.
  #
    # mutate(value = case_when(
    #   ind == <INSERT YRS OF COMPULSORY PRIM-SEC INDICATOR> ~ as.numeric(value >= 9),
    #   ind == <INSERT YRS OF FREE PRIM-SEC INDICATOR>       ~ as.numeric(value >= 12),
    #   ind == <INSERT BULLYING INDICATOR>                   ~ as.numeric(value == 3), # is bullying risk high? yes/no
    #   # etc.
    #   TRUE ~ value
    # )) %>%
  identity
}

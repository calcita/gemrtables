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

aggregates <- function(df) {

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

}

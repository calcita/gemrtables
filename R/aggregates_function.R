# function to calculate regional and incomegroup averages

aggregates <- function(df, cut = .5) {

  world <- df %>%
    mutate(count1 = n_distinct(iso2c)) %>%
    group_by(ind, count1) %>%
    summarise(sum = sum(value),
              median = median(value, na.rm = TRUE),
              w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
              pc_true = (sum(value)/n())*100,
              count2 = n()) %>%
    mutate(annex_name = "World", pc_comp = round(count2/count1, digits = 2),
           entity = "region") %>%
    gather(key = "aggregation", value = "value", -ind, - annex_name, -pc_comp, -entity)

  regional <- df %>%
    group_by(SDG.region) %>%
    mutate(count1 = n_distinct(iso2c)) %>%
    ungroup() %>%
    group_by(SDG.region, ind, count1) %>%
    summarise(sum = sum(value,  na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
              pc_true = (sum(value)/n())*100,
              count2 = n()) %>%
    mutate(pc_comp = round(count2/count1, digits = 2),
           entity = "region") %>%
    gather(key = "aggregation", value = "value", -ind, -SDG.region, -pc_comp, -entity) %>%
    select(annex_name = SDG.region, ind, aggregation, value, pc_comp, entity)

  income <- df %>%
    group_by(income_group) %>%
    mutate(count1 = n_distinct(iso2c)) %>%
    ungroup() %>%
    group_by(income_group, ind, count1) %>%
    summarise(sum = sum(value),
              median = median(value, na.rm = TRUE),
              w_mean = sum(value*wt_value, na.rm = TRUE)/sum(wt_value, na.rm = TRUE),
              pc_true = (sum(value)/n())*100,
              count2 = n()) %>%
    mutate(pc_comp = round(count2/count1, digits = 2),
           entity = "income_group") %>%
    gather(key = "aggregation", value = "value", -ind, -income_group, -pc_comp, -entity) %>%
    select(annex_name = income_group, ind, aggregation, value, pc_comp, entity)

  aggregates <- bind_rows(world, regional, income) %>%
    semi_join(indicators, by = c("ind", "aggregation")) %>%
    mutate(year = ref_year) %>%
    filter(pc_comp >= cut)

}

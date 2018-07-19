#function to read restful SDMX queries. Key corresponds to the uis user key.


read_urls <- function(urls, key = NULL, bind = TRUE) {

  key = as.character(key)

  if(length(urls) == 1) {
    {if(str_detect(urls, "key=")) paste(urls, key, sep = "") else urls } %>%
      readSDMX() %>%
      as_data_frame()
  } else {
    modify_if(.x = urls, .p = str_detect(urls, "key="), .f = paste, key, sep = "") %>%
      map(readSDMX) %>%
      map(as_data_frame) %>%
      {if(bind == TRUE) reduce(., bind_rows) else . }
  }
}


#function to read GEMR cedar SQL database


read_cedar <- function(sc, table, ind, level = 1) {

  cedar_con <- dbConnect(RMariaDB::MariaDB(), host = "77.104.134.109",
                         dbname = "cedardat_cedar",
                         port = "3306",
                         user = "cedardat_user", password = pkg.env$password)

  sc <- tbl(cedar_con, sc)
  dims <- colnames(sc)

  table <- tbl(cedar_con, table) %>%
    select(-id)

  df <- sc %>%
    purrr::when(
      sc$ops$x == "wide_dimension" ~ filter(., category_id %in% level),
      sc$ops$x == "other_dimension" ~ .
    ) %>%
    inner_join(table, by = c("id" = "dim_id")) %>%
    select(dims, !!!ind) %>%
    collect() %>%
    gather(indicator, value, !!!ind) %>%
    purrr::when(
      sc$ops$x == "wide_dimension" ~  group_by(., country_code, indicator, level_id, grade_id),
      sc$ops$x == "other_dimension" ~ group_by(., country_code, indicator, sex_id, ISCED_id, competence_id)
    ) %>%
    filter(!is.na(value)) %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year == max(year), value != 0) %>%
    {if(is.character(.$value)) mutate(., value = case_when(value == "0" ~ NA_real_, value == "LOW" ~ 1, value == "MEDIUM" ~ 2, value == "HIGH" ~ 3)) else . } %>%
    unique()

  dbDisconnect(cedar_con)

  return(df)

}


#' read_urls
#'
#' \code{read_urls} parses SDMX rest url(s) to data frames
#'
#' Wrapper for \code{\link[rsdmx]{readSDMX}}
#'
#' @param urls SDMX url query or list of SDMX url queries (character).
#' @param key UIS subcription key. Required if `urls` contains UIS queries.
#'   Default is NULL.
#' @param bind If `TRUE` returns a list a data frames for each query. If `FALSE` binds
#'   queries into a single data frame.
#' @return A data frame.
#' @export
#' @family import
#' @seealso \code{\link[rsdmx]{readSDMX}}
#' @examples
#' read_urls("https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/COMP_EDU.YR.L1..................?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
#' key = x)
#'
#' read_urls("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/trng_aes_100/.T.FE_NFE.PC../?startperiod=2010&endPeriod=2050")
#'
#' list(
#' "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20005..11420.100.100.D.112.E02+E01/all?startTime=2016&endTime=2050",
#' "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_TS_ACT/..L0+L1+L2_C4+L3_C4.Y25T64.T.RATIO_ACTL_TER/all?",
#' "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EDU_PERS_INST/.T.INST_T.T.L2+L3.T.TEACH.PER/all?startTime=2010&endTime=2050") %>%
#'  read_urls(bind = FALSE))
#'
#' list(
#' "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/COMP_EDU.YR.L1..................?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
#' "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_TS_ACT/..L0+L1+L2_C4+L3_C4.Y25T64.T.RATIO_ACTL_TER/all?",
#' "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EDU_PERS_INST/.T.INST_T.T.L2+L3.T.TEACH.PER/all?startTime=2010&endTime=2050") %>%
#'  read_urls(key = x))
read_urls <- function(urls, key = NULL, bind = TRUE) {

  key = as.character(key)

  if(length(urls) == 1) {
    {if(str_detect(urls, "key=")) paste(urls, key, sep = "") else urls } %>%
      rsdmx::readSDMX() %>%
      dplyr::as_data_frame()
  } else {
    purrr::modify_if(.x = urls, .p = stringr::str_detect(urls, "key="), .f = paste, key, sep = "") %>%
    purrr::map(readSDMX) %>%
    purrr::map(as_data_frame) %>%
    {if(bind == TRUE) purrr::reduce(., dplyr::bind_rows) else . }
  }
}


#' read_cedar
#'
#' \code{read_cedar} is a function to join, filter and import data from GEMR cedar SQL database
#'
#' @param sc SQL table on which to filter observations. Current choices are
#'   `wide_dimension` for wide data and `other_dimension` for GEM 2030 data.
#' @param level Disaggregations on which to filter `wide_dimension` table
#'   (connect to sql database for further details). Defaults to 1 (total
#'   country).
#' @param table Table of variables on which to join (character).
#' @param ind Character vector of variables to select.
#' @param password Password to connect to cedar SQL database.
#' @return A data frame.
#' @family import
#' @seealso codebook for WIDE indicators:
#'   \url{https://drive.google.com/file/d/0B5qc8r9eSwe4LUhWajNwTS10TDA/view}.
#'   Codebook for GEM2030 indicators:
#'   \url{https://sites.google.com/view/codebooks/home/gem-2030}
#' @examples
#' read_cedar(sc = "wide_dimension", level = 1, table = "wide_1", ind =
#' c("trans_prim_m", "comp_prim_v2_m"), password = x)
#'
#' list(sc = list("wide_dimension",
#' "wide_dimension",
#' "other_dimension",
#' "other_dimension",
#' "other_dimension",
#' "other_dimension"),
#' level = list(1,
#'             c(13:15, 31),
#'             NA,
#'             NA,
#'             NA,
#'             NA),
#'table = list("wide_1",
#'             "wide_1",
#'             "other_school_readiness",
#'             "other_facilities",
#'             "other_social_norms",
#'             "other_curriculum"),
#'ind = list(c("trans_prim_m", "comp_prim_v2_m", "comp_lowsec_v2_m", "comp_upsec_v2_m"),
#'           c("comp_prim_v2_m", "comp_lowsec_v2_m", "comp_upsec_v2_m"),
#'           c("u5_posit_home_learn", "u5_child_book", "school_child_track"),
#'           c("stu_exper_bully_13_17", "stu_exper_violence_13_17"),
#'           "child_chores_more_28_12_14",
#'           c("esd_gced_curr_ge", "esd_gced_curr_hr", "esd_gced_glo_cit", "esd_gced_sus_dev"))) %>%
#'  pmap(read_cedar, password = x)
read_cedar <- function(sc, level = 1, table, ind, password) {

  if(missing(password)) {
    password <- pkg.env$key
  }

  cedar_con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), host = "77.104.134.109",
                                   dbname = "cedardat_cedar",
                                   port = "3306",
                                   user = "cedardat_user", password = password)

  sc <- dplyr::tbl(cedar_con, sc)
  dims <- colnames(sc)

  table <- dplyr::tbl(cedar_con, table) %>%
    dplyr::select(-id)

  df <- sc %>%
    purrr::when(
      sc$ops$x == "wide_dimension" ~ dplyr::filter(., category_id %in% level),
      sc$ops$x == "other_dimension" ~ .
    ) %>%
    dplyr::inner_join(table, by = c("id" = "dim_id")) %>%
    dplyr::select(dims, !!!ind) %>%
    dplyr::collect() %>%
    tidyr::gather(indicator, value, !!!ind) %>%
    purrr::when(
      sc$ops$x == "wide_dimension" ~  dplyr::group_by(., country_code, indicator, level_id, grade_id),
      sc$ops$x == "other_dimension" ~ dplyr::group_by(., country_code, indicator, sex_id, ISCED_id, competence_id)
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(year == max(year), value != 0) %>%
    {if(is.character(.$value)) dplyr::mutate(., value = dplyr::case_when(value == "0" ~ 0, value == "LOW" ~ 1, value == "MEDIUM" ~ 2, value == "HIGH" ~ 3)) else . } %>%
    unique()

  dbDisconnect(cedar_con)

  return(df)

}


#' cedar
#'
#' \code{uis} is a function to import and clean cedar data
#'

#' Defines relvent tables and filters for cedar sql tables and applies the `cedar_clean` function
#'@family import/clean
#'@seealso \code{\link{cedar_clean}}

cedar <- function() {

  # key <- list("cedar_up")
  # cedar_up <- R.cache::loadCache(key)
  #
  # if(is.null(cedar_up)) {
  #   #cat(paste("   generating", key[[1]], "from scratch...\n", sep = " "))
  #   message(glue::glue("generating {key} from scratch..."))
  #
  #   cedar_up <- list(sc = list("wide_dimension",
  #                              "wide_dimension",
  #                              "other_dimension",
  #                              "other_dimension",
  #                              "other_dimension",
  #                              "other_dimension"),
  #                    level = list(1,
  #                                 c(13:15, 31),
  #                                 NA,
  #                                 NA,
  #                                 NA,
  #                                 NA),
  #                    table = list("wide_1",
  #                                 "wide_1",
  #                                 "other_school_readiness",
  #                                 "other_facilities",
  #                                 "other_social_norms",
  #                                 "other_curriculum"),
  #                    ind = list(c("trans_prim_m", "comp_prim_v2_m", "comp_lowsec_v2_m", "comp_upsec_v2_m"),
  #                               c("comp_prim_v2_m", "comp_lowsec_v2_m", "comp_upsec_v2_m"),
  #                               c("u5_posit_home_learn", "u5_child_book", "school_child_track"),
  #                               c("stu_exper_bully_13_17", "stu_exper_violence_13_17"),
  #                               "child_chores_more_28_12_14",
  #                               c("esd_gced_curr_ge", "esd_gced_curr_hr", "esd_gced_glo_cit", "esd_gced_sus_dev"))) %>%
  #   purrr::pmap(read_cedar) %>%
  #   dplyr::bind_rows()
  #
  # R.cache::saveCache(cedar_up, key=key, comment="cedar_up")
  #
  # }

  cedar_local <-  read.csv(system.file("local_data", "WIDE_2018_completion.csv", package = "gemrtables"), stringsAsFactors = FALSE)
  cedar_local <- cedar_local[-c(4648:4649),] %>%
    dplyr::filter(category != 'camps',
                  !stringr::str_detect(category, "2|3|4"),
                  survey != "MICS" & iso_code3 != "SRB") %>%
    tidyr::spread(key = group, value = category) %>%
    dplyr::select(country_code = iso_code3,
                  year,
                  comp_prim_v2_m = comp_prim_v2,
                  comp_lowsec_v2_m = comp_lowsec_v2,
                  comp_upsec_v2_m = comp_upsec_v2,
                  location_id = location,
                  wealth_id = wealth,
                  sex_id = sex,
                  wsex = "poorest by sex" ) %>%
    dplyr::mutate(location_id = case_when(location_id == 'intermediate or densely populated area' | location_id == 'urban' ~ 6,
                                          location_id == 'thinly populated area' | location_id == 'rural' ~ 5,
                                          TRUE ~ NA_real_),
                  sex_id = case_when(sex_id == 'female'| wsex == "poorest female"  ~ 1,
                                     sex_id == 'male' | wsex == "poorest male"  ~ 2,
                                     TRUE ~ NA_real_),
                  wealth_id = case_when(wealth_id == 'quintile 1'| !is.na(wsex)  ~ 6,
                                        wealth_id == 'quintile 5'  ~ 10,
                                        TRUE ~ NA_real_)) %>%
    dplyr::select(-wsex)%>%
    tidyr::gather(key = indicator, value = value, - country_code, - year, -location_id, -wealth_id, - sex_id) %>%
    dplyr::mutate(value = value/100) %>%
    cedar_clean()

  cedar_up %>%
    cedar_clean() %>%
    filter(!stringr::str_detect(ind, "CR")) %>%
    bind_rows(cedar_local)

}

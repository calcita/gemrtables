#' cedar
#'
#' \code{uis} is a function to import and clean cedar data
#'

#' Defines relvent tables and filters for cedar sql tables and applies the `cedar_clean` function
#'@family import/clean
#'@seealso \code{\link{cedar_clean}}

cedar <- function() {

  cedar_data <- list(sc = list("wide_dimension",
                               "wide_dimension",
                               "other_dimension",
                               "other_dimension",
                               "other_dimension",
                               "other_dimension"),
                     level = list(1,
                                  c(13:15, 31),
                                  NA,
                                  NA,
                                  NA,
                                  NA),
                     table = list("wide_1",
                                  "wide_1",
                                  "other_school_readiness",
                                  "other_facilities",
                                  "other_social_norms",
                                  "other_curriculum"),
                     ind = list(c("trans_prim_m", "comp_prim_v2_m", "comp_lowsec_v2_m", "comp_upsec_v2_m"),
                                c("comp_prim_v2_m", "comp_lowsec_v2_m", "comp_upsec_v2_m"),
                                c("u5_posit_home_learn", "u5_child_book", "school_child_track"),
                                c("stu_exper_bully_13_17", "stu_exper_violence_13_17"),
                                "child_chores_more_28_12_14",
                                c("esd_gced_curr_ge", "esd_gced_curr_hr", "esd_gced_glo_cit", "esd_gced_sus_dev"))) %>%
    purrr::pmap(read_cedar) %>%
    dplyr::bind_rows() %>%
    cedar_clean()

}

#' parity_adj
#'
#' \code{parity_adj} calculates adjusted parity ratios.
#'
#' @param df a data frame with a key / value columns.
#' @param col indicator key column.
#' @param a indicator for 'disadvantaged' group (numerator).
#' @param b indicator for 'advantaged' group  (denominator).
#' @param varname name for calculated indice (character).
#' @param val_status For use with data with flags for estimated observations. If
#'   `TRUE` will calculate flag for indice (requires flag column to be named
#'   `val_status` and estimates lablled as `E`.
#' @return A data frame.
#' @export
#' @family summarise
#' @examples
#'
#' parity_adj(df, indicator, ger.female, ger.male, "ger.gpia", val_status = FALSE)
#'
#' parity_indices <- list(df = list("df","df"),
#' col = list("ind",  "ind"),
#' a = list("adult.profiliteracy.f","adult.profinumeracy.f"),
#' b = list("adult.profiliteracy.m", "adult.profinumeracy.m"),
#' varname = list("adult.profiliteracy.gpia", "adult.profinumeracy.gpia")) %>%
#' pmap(parity_adj) %>%


parity_adj <- function(df, col, a, b, varname, val_status = FALSE) {

  df <- dynGet(df)
  col <- as.name(col)

    if(isTRUE(.gemrtables.pkg.env$level_country)){
      indice <- df %>%
        dplyr::filter(!!col %in% c(a, b))
      indice <- dplyr::group_by(indice, iso2c, year) %>%
    dplyr::filter(n()==2 )
  } else {
    indice <- df %>%
      dplyr::filter(!!col %in% c(a, b))
    indice <- dplyr::group_by(indice, region, year) %>%
      dplyr::filter(n()==2)
  }

  indice %>%
    {if(val_status == FALSE) dplyr::summarise(., value = ifelse(value[!!col == !!a] > value[!!col == !!b], 2 - (1/(value[!!col == !!a]/value[!!col == !!b])), value[!!col == !!a]/value[!!col == !!b]))
      else
        dplyr::summarise(., value = ifelse(value[!!col == !!a] > value[!!col == !!b], 2 - (1/(value[!!col == !!a]/value[!!col == !!b])), value[!!col == !!a]/value[!!col == !!b]),
                  val_status = ifelse(val_status[!!col == !!a] == "E" | val_status[!!col == !!b] == "E", "E", "A")) } %>%
    dplyr::mutate(ind = varname) %>%
    dplyr::filter(!is.na(value))
}


#' uis_clean
#'
#' \code{uis_clean} is a function to clean UIS data.
#'
#' Cleans data from UIS api queries and computes several variables
#' (admi.grade2or3prim, admi.endofprim, admi.endoflowersec, Comp.02, Free.02,
#' Comp.2t3, Free.2t3, Read.Primary.GPIA, Math.Primary.GPIA, Read.LowerSec.GPIA,
#' Math.LowerSec.GPIA, LR.Ag15t24.GPIA, LR.Ag15t99.GPIA, Read.Primary.WPIA,
#' Math.Primary.WPIA, Read.LowerSec.WPIA, Math.LowerSec.WPIA)
#'@family clean

uis_clean <- function(df) {

  clean1 <- df %>%
    tidyr::unite(col = var_concat, STAT_UNIT, UNIT_MEASURE, EDU_LEVEL, EDU_CAT, SEX, AGE, GRADE, SECTOR_EDU, EDU_ATTAIN, WEALTH_QUINTILE, LOCATION,
          EDU_TYPE, EDU_FIELD, SUBJECT, INFRASTR, SE_BKGRD, TEACH_EXPERIENCE, CONTRACT_TYPE, COUNTRY_ORIGIN, REGION_DEST,
          EXPENDITURE_TYPE, SOURCE_FUND, FUND_FLOW) %>%
    dplyr::select(iso2c = REF_AREA, var_concat, year = TIME_PERIOD, value = OBS_VALUE, val_status = OBS_STATUS) %>%
    dplyr::mutate(value = as.numeric(value),
           value = ifelse(val_status == "Z", NA, value),
           year = as.numeric(year)) %>%
    dplyr::filter(!is.na(value)) %>%
    unique()

  clean1 %>%
    dplyr::group_by(iso2c, var_concat) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    R.cache::saveCache(key=list("uis_comp"), comment="uis_comp")


  clean2 <- clean1 %>%
    dplyr::inner_join(.gemrtables.pkg.env$indicators[, 1:2], by = "var_concat")

  admin_assessment <- clean1 %>%
    dplyr::filter(stringr::str_detect(var_concat, "ADMIN_NB")) %>%
    dplyr::group_by(iso2c, year) %>%
    tidyr::spread(key = var_concat, value = value) %>%
    dplyr::mutate(admi.grade2or3prim = ifelse(ADMIN_NB_L1__T__T__T_G2_3_INST_T__Z__Z__T__T__T_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA == 1 | ADMIN_NB_L1__T__T__T_G2_3_INST_T__Z__Z__T__T__T_READING__Z__T__Z__Z_W00_W00_NA_NA_NA == 1, 1, 0),
           admi.endofprim = ifelse(ADMIN_NB_L1__T__T__T_GLAST_INST_T__Z__Z__T__T__T_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA == 1 | ADMIN_NB_L1__T__T__T_GLAST_INST_T__Z__Z__T__T__T_READING__Z__T__Z__Z_W00_W00_NA_NA_NA == 1, 1, 0),
           admi.endoflowersec = ifelse(ADMIN_NB_L2__T__T__T_GLAST_INST_T__Z__Z__T__T__T_READING__Z__T__Z__Z_W00_W00_NA_NA_NA == 1 | ADMIN_NB_L2__T__T__T_GLAST_INST_T__Z__Z__T__T__T_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA == 1, 1, 0),
           val_status = "A") %>%
    dplyr::select(iso2c, year, contains("admi."), val_status) %>%
    tidyr::gather(key = "ind", value = "value", -iso2c, -year, -val_status) %>%
    dplyr::mutate(value = ifelse(is.na(value), 0, value))

  # More accurate calculation based on published net flow of int'l mobile students,
  # but currently inbound rate is more available.
  # inbound_stu_add <- clean1 %>%
  #   dplyr::filter(var_concat %in% c(
  #     "MENF_PER_L5T8__T__T__T__T_INST_T__Z__Z__T__T__T__T__Z__Z__Z__Z_W00_W00_NA_NA_NA",
  #     "OE_PER_L5T8__T__T__T__T_INST_T__Z__Z__T__T__T__T__Z__Z__Z__Z_W00_W00_NA_NA_NA"
  #   )) %>%
  #   dplyr::group_by(iso2c, year) %>%
  #   dplyr::filter(n()==2) %>%
  #   dplyr::summarise(
  #     value = sum(value),
  #     val_status = ifelse(any(val_status == "E"), "E", "A")) %>%
  #   dplyr::mutate(ind = "IE.5t8.40510") %>%
  #   dplyr::select(iso2c, year, ind, value, val_status) %>%
  #   group_by(iso2c, ind) %>%
  #   filter(year == max(year)) %>%
  #   ungroup()

  inbound_stu <- clean1 %>%
    dplyr::filter(var_concat %in% c(
        "STU_PER_L5T8__T__T__T__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
        "MSEP_PT_L5T8__T__T__T__T_INST_T__Z__Z__T__T__T__T__Z__Z__Z__Z_W00_W00_NA_NA_NA"
                             )) %>%
    dplyr::group_by(iso2c, year) %>%
    dplyr::filter(n()==2) %>%
    dplyr::summarise(
      value = value[1] * (value[2]/100),
      val_status = ifelse(any(val_status == "E"), "E", "A")) %>%
    dplyr::mutate(ind = "IE.5t8.40510") %>%
    dplyr::select(iso2c, year, ind, value, val_status) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year)) %>%
    ungroup()

  # auxiliary
  vars_f <- list("STU_PT_L1__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L1__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "LR_PT__Z__Z_F_Y15T24__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "LR_PT__Z__Z_F_Y_GE15__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_READING__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_MATH__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_READING__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_MATH__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L02__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L1__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L2_3__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L5T8__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA")

  vars_m <- list("STU_PT_L1__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L1__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
              "LR_PT__Z__Z_M_Y15T24__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "LR_PT__Z__Z_M_Y_GE15__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_READING__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_MATH__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_READING__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
              "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_MATH__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L02__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L1__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L2_3__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
              "GER_PT_L5T8__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA")

  if(isTRUE(.gemrtables.pkg.env$level_country)){
    parity_indices <- list(df = rep(list("clean1"), 14),
                         col = rep(list("var_concat"), 14),
                         a = list(vars_f),
                         b = list(vars_m),
                         varname = list("Read.Primary.GPIA", "Math.Primary.GPIA", "Read.LowerSec.GPIA", "Math.LowerSec.GPIA",
                                        "LR.Ag15t24.GPIA", "LR.Ag15t99.GPIA", "Read.Primary.WPIA", "Math.Primary.WPIA",
                                        "Read.LowerSec.WPIA", "Math.LowerSec.WPIA", "GER.02.GPIA", "GER.1.GPIA", "GER.2t3.GPIA",
                                        "GER.5t8.GPIA"),
                         val_status = rep(list(TRUE), 14)) %>%
    purrr::pmap(parity_adj) %>%
    purrr::reduce(dplyr::bind_rows)

  cleaned <- dplyr::bind_rows(clean2, inbound_stu, parity_indices) %>%
    dplyr::mutate(source = "UIS", year = as.numeric(year)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source) %>%
    dplyr::filter(!is.na(value))
  } else {

    clean1 <- clean1 %>%
      dplyr::filter(var_concat %in% c(vars_f, vars_m)) %>%
      R.cache::saveCache(key=list("uis_cleaned"), comment="uis_cleaned")

    cleaned <- dplyr::bind_rows(clean2, inbound_stu) %>%
      dplyr::mutate(source = "UIS", year = as.numeric(year)) %>%
      dplyr::select(iso2c, year, ind, value, val_status, source) %>%
      dplyr::filter(!is.na(value))
  }
}

#' cedar_clean
#'
#' \code{cedar_clean} is a function to clean data imported by \code{read_cedar}.
#'
#' Cleans data imported from cedar database and computes several variables
#' (CR.1.GPIA, CR.1.LPIA, CR.1.WPIA, CR.2.GPIA, CR.2.LPIA, CR.2.WPIA, CR.3.GPIA,
#' CR.3.LPIA, CR.3.WPIA, chores.28plus.12t14.GPIA)
#'@family clean
#'@seealso \code{\link{read_cedar}}

cedar_clean <- function(df) {

  clean1 <- df %>%
    dplyr::mutate(ind = dplyr::case_when(indicator == "trans_prim_m" & is.na(sex_id) ~ "TranRA.2.GPV.cp",
                           indicator == "comp_prim_v2_m" & is.na(sex_id)  & is.na(location_id) & is.na(wealth_id) ~ "CR.1",
                           indicator == "comp_prim_v2_m" & sex_id == 1 & is.na(wealth_id) ~ "CR.1.f",
                           indicator == "comp_prim_v2_m" & sex_id == 2 & is.na(wealth_id) ~ "CR.1.m",
                           indicator == "comp_prim_v2_m" & location_id == 5 ~ "CR.1.rural",
                           indicator == "comp_prim_v2_m" & location_id == 6 ~ "CR.1.urban",
                           indicator == "comp_prim_v2_m" & wealth_id == 6 & is.na(sex_id) ~ "CR.1.q1",
                           indicator == "comp_prim_v2_m" & wealth_id == 10 & is.na(sex_id) ~ "CR.1.q5",
                           indicator == "comp_prim_v2_m" & wealth_id == 6 & sex_id == 1 ~ "CR.1.q1.f",
                           indicator == "comp_prim_v2_m" & wealth_id == 6 & sex_id == 2 ~ "CR.1.q1.m",
                           indicator == "comp_lowsec_v2_m"  & is.na(sex_id)  & is.na(location_id) & is.na(wealth_id) ~ "CR.2",
                           indicator == "comp_lowsec_v2_m" & sex_id == 1 & is.na(location_id) & is.na(wealth_id) ~ "CR.2.f",
                           indicator == "comp_lowsec_v2_m" & sex_id == 2 & is.na(location_id) & is.na(wealth_id) ~ "CR.2.m",
                           indicator == "comp_lowsec_v2_m" & location_id == 5 ~ "CR.2.rural",
                           indicator == "comp_lowsec_v2_m" & location_id == 6 ~ "CR.2.urban",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 6 & is.na(sex_id) ~ "CR.2.q1",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 10 & is.na(sex_id) ~ "CR.2.q5",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 6 & sex_id == 1 ~ "CR.2.q1.f",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 6 & sex_id == 2 ~ "CR.2.q1.m",
                           indicator == "comp_upsec_v2_m" & is.na(sex_id)  & is.na(location_id) & is.na(wealth_id) ~ "CR.3",
                           indicator == "comp_upsec_v2_m" & sex_id == 1 & is.na(wealth_id) ~ "CR.3.f",
                           indicator == "comp_upsec_v2_m" & sex_id == 2 & is.na(wealth_id) ~ "CR.3.m",
                           indicator == "comp_upsec_v2_m" & location_id == 5 ~ "CR.3.rural",
                           indicator == "comp_upsec_v2_m" & location_id == 6 ~ "CR.3.urban",
                           indicator == "comp_upsec_v2_m" & wealth_id == 6 & is.na(sex_id) ~ "CR.3.q1",
                           indicator == "comp_upsec_v2_m" & wealth_id == 10 & is.na(sex_id) ~ "CR.3.q5",
                           indicator == "comp_upsec_v2_m" & wealth_id == 6 & sex_id == 1 ~ "CR.3.q1.f",
                           indicator == "comp_upsec_v2_m" & wealth_id == 6 & sex_id == 2 ~ "CR.3.q1.m",
                           indicator == "u5_posit_home_learn"  ~ "home.lrn.env.3t7",
                           indicator == "u5_child_book"  ~ "home.book.u5",
                           indicator == "school_child_track"  ~ "OnTrack.three.domains",
                           indicator == "stu_exper_violence_13_17" & sex_id ==4  ~ "stu.viol.13t17",
                           indicator == "stu_exper_bully_13_17"  & sex_id == 4 ~ "stu.bully.13t17",
                           indicator == "esd_gced_curr_ge"  ~ "esd.curr.ge",
                           indicator == "esd_gced_curr_hr"  ~ "esd.curr.hr",
                           indicator == "esd_gced_glo_cit"  ~ "esd.curr.cit",
                           indicator == "esd_gced_sus_dev"  ~ "esd.sus.dev",
                           indicator == "child_chores_more_28_12_14" & sex_id == 4 ~ "chores.28plus.12t14",
                           indicator == "child_chores_more_28_12_14" & sex_id == 1 ~ "chores.28plus.12t14.f",
                           indicator == "child_chores_more_28_12_14" & sex_id == 2 ~ "chores.28plus.12t14.m"),
                  value = ifelse(stringr::str_detect(ind, stringr::regex("CR\\.|TranRA")), value*100, value),
                  val_status = ifelse(stringr::str_detect(ind, stringr::regex("CR\\.|TranRA")), "A", NA)) %>%

    dplyr::inner_join(.gemrtables.pkg.env$regions, by = c("country_code" = "iso3c")) %>%
    dplyr::filter(!is.na(ind))

  vars_f = list("CR.1.f", "CR.1.rural", "CR.1.q1", "CR.2.f", "CR.2.rural", "CR.2.q1", "CR.3.f", "CR.3.rural", "CR.3.q1", "chores.28plus.12t14.f")
  vars_m = list("CR.1.m", "CR.1.urban", "CR.1.q5","CR.2.m", "CR.2.urban", "CR.2.q5", "CR.3.m", "CR.3.urban", "CR.3.q5", "chores.28plus.12t14.m")

  if(isTRUE(.gemrtables.pkg.env$level_country)){

  parity_indices <- list(df = rep(list("clean1"), 10),
                         col = rep(list("ind"), 10),
                         a = list(vars_f),
                         b = list(vars_m),
                         varname = list("CR.1.GPIA", "CR.1.LPIA", "CR.1.WPIA", "CR.2.GPIA", "CR.2.LPIA", "CR.2.WPIA",
                                        "CR.3.GPIA", "CR.3.LPIA", "CR.3.WPIA", "chores.28plus.12t14.GPIA")) %>%
    purrr::pmap(parity_adj) %>%
    purrr::reduce(dplyr::bind_rows)

  cleaned <- dplyr::bind_rows(clean1, parity_indices) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso2c, year, ind, value, val_status) %>%
    dplyr::mutate(source = "cedar") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    unique()
  } else {
    cedar_clean <- clean1 %>%
      dplyr::filter(var_concat %in% c(vars_f, vars_m)) %>%
      R.cache::saveCache(key=list("cedar_cleaned"), comment="cedar_cleaned")

    cleaned <- clean1 %>%
      dplyr::ungroup() %>%
      dplyr::filter(!var_concat %in% c(vars_f, vars_m)) %>%
      dplyr::select(iso2c, year, ind, value, val_status) %>%
      dplyr::mutate(source = "cedar") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::group_by(iso2c, ind) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::ungroup() %>%
      unique()
  }
}

#' wb_clean
#'
#' \code{wb_clean} is a function to clean data imported by \code{[wbstats]{wb}} within \code{\link{other}} .
#'
#' Cleans data imported by \code{[wbstats]{wb}} and computes two variables
#' (adult.profiliteracy.gpia, adult.profinumeracy.gpia)
#'@family clean
#'@seealso \code{\link[wbstats]{wb}}, \code{\link{other}}

wb_clean <- function(df) {

  clean1 <- df %>%
    dplyr::mutate(ind = dplyr::case_when(indicatorID == "SH.STA.STNT.ZS" ~ "stunt.u5",
                                         indicatorID == "LO.TIMSS.SCI8.LOW" | stringr::str_detect(indicatorID, "PISA")  ~ "sci.lowerSec",
                                         stringr::str_detect(indicatorID, "PIAAC.LIT.YOU")  ~ "youth.profiliteracy",
                                         stringr::str_detect(indicatorID, "PIAAC.NUM.YOU")  ~ "youth.profinumeracy",
                                         stringr::str_detect(indicatorID, "LIT.BE|LIT.1")  ~ "adult.profiliteracy",
                                         stringr::str_detect(indicatorID, "NUM.BE|NUM.1")  ~ "adult.profinumeracy",
                                         stringr::str_detect(indicatorID, "LIT.MA.BE|LIT.MA.1")  ~ "adult.profiliteracy.m",
                                         stringr::str_detect(indicatorID, "LIT.FE.BE|LIT.FE.1")  ~ "adult.profiliteracy.f",
                                         stringr::str_detect(indicatorID, "NUM.MA.BE|NUM.MA.1")  ~ "adult.profinumeracy.m",
                                         stringr::str_detect(indicatorID, "NUM.FE.BE|NUM.FE.1")  ~ "adult.profinumeracy.f"),
                  source = dplyr::case_when(indicatorID == "SH.STA.STNT.ZS" ~ "World Bank",
                                            indicatorID == "LO.TIMSS.SCI8.LOW" ~ "TIMSS",
                                            stringr::str_detect(indicatorID, "PISA")  ~ "PISA",
                                            stringr::str_detect(indicatorID, "PIAAC")  ~ "PIAAC"))

  clean2 <- clean1 %>%
    dplyr::filter(stringr::str_detect(indicatorID, "PISA|PIAAC")) %>%
    dplyr::group_by(source, iso2c, ind, date) %>%
    dplyr::summarise(value = 100 - (sum(value)))  %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(clean1) %>%
    dplyr::filter(indicatorID %in% c("SH.STA.STNT.ZS", "LO.TIMSS.SCI8.LOW", NA)) %>%
    dplyr::select(iso2c, ind, year = date, value, source)

  if(isTRUE(.gemrtables.pkg.env$level_country)){
  parity_indices <- list(df = list("clean2","clean2"),
                         col = list("ind",  "ind"),
                         a = list("adult.profiliteracy.f","adult.profinumeracy.f"),
                         b = list("adult.profiliteracy.m", "adult.profinumeracy.m"),
                         varname = list("adult.profiliteracy.gpia", "adult.profinumeracy.gpia")) %>%
    purrr::pmap(parity_adj) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(source = "PIAAC")

  cleaned <- dplyr::bind_rows(clean2, parity_indices) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year), !is.na(value)) %>%
    dplyr::mutate(val_status = "A",
           year = as.numeric(year)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source)
  } else {
    cleaned <- clean2
  }
}

#' eurostat_clean
#'
#' \code{eurostat_clean} is a function to clean data from eurostat API queries
#' defined in \code{other}.
#'
#'@family clean function
#'@seealso \code{\link{other}}

eurostat_clean <- function(df) {

  cleaned <- df %>%
    dplyr::mutate(ind = dplyr::case_when(TRAINING == "FE_NFE" ~ "prya.25t64",
                                         INDIC_IS == "I_CCPY"  ~ "yadult.porcentICTskill.copi"),
                  source = "eurostat",
                  val_status = "A",
                  obsTime = as.numeric(obsTime)) %>%
    dplyr::select(iso2c = GEO, year = obsTime, ind, source, value = obsValue, val_status ) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year))

}

#' oecd_clean
#'
#' \code{oecd_clean} is a function to clean data from OECD API queries defined
#' in \code{other}.
#'
#' Cleans data from OECD api queries and computes one variable.
#' (sal.rel.2t3,)
#'
#'@family clean
#'@seealso \code{\link{other}}

oecd_clean <- function(df) {

  schol_unspec <- df[[1]] %>%
    dplyr::mutate(ind = dplyr::case_when(AIDTYPE == "E02" ~ "odaflow.imputecost",
                                         AIDTYPE == "E01"  ~ "odaflow.volumescholarship"),
                  RECIPIENT = as.numeric(RECIPIENT)) %>%
    dplyr::filter(RECIPIENT %in% c(89, 57, 189, 289, 298, 380, 389, 489, 498, 589, 619, 679, 689, 789, 798, 889, 9998)) %>%
    dplyr::group_by(ind) %>%
    dplyr::summarise(value = (sum(obsValue))*1000000)

  R.cache::saveCache(schol_unspec, key=list("schol_unspec"), comment="schol_unspec")

  schol <- df[[1]] %>%
    dplyr::mutate(ind = dplyr::case_when(AIDTYPE == "E02" ~ "odaflow.imputecost",
                                         AIDTYPE == "E01"  ~ "odaflow.volumescholarship"),
                  RECIPIENT = as.numeric(RECIPIENT)) %>%
    dplyr::left_join(.gemrtables.pkg.env$regions, by = c("RECIPIENT" = "oecd.crs.recipientcode")) %>%
    dplyr::select(iso2c, year = REFERENCEPERIOD, ind, value = obsValue) %>%
    dplyr::mutate(value = value*1000000)

  sal1 <- df[[2]] %>%
    dplyr::mutate(ind = dplyr::case_when(ISC11 == "L0" ~ "sal.rel.02",
                                         ISC11 == "L1" ~ "sal.rel.1",
                                         ISC11 == "L2_C4" ~ "sal.rel.2",
                                         ISC11 == "L3_C4" ~ "sal.rel.3")) %>%
    dplyr::inner_join(.gemrtables.pkg.env$regions, by = c("COUNTRY" = "iso3c")) %>%
    dplyr::select(iso2c, year = YEAR, ind, value = obsValue)

  sal2 <- df[[3]] %>%
    dplyr::mutate(ind = dplyr::case_when(ISC11_LEVEL == "L2" ~ "teachers.2",
                                         ISC11_LEVEL == "L3" ~ "teachers.3")) %>%
    dplyr::inner_join(.gemrtables.pkg.env$regions, by = c("COUNTRY" = "iso3c")) %>%
    dplyr::select(iso2c, year = obsTime, ind, value = obsValue) %>%
    dplyr::bind_rows(sal1)  %>%
    tidyr::spread(key = ind, value = value) %>%
    dplyr::mutate(value = ((sal.rel.2 * teachers.2) + (sal.rel.3 * teachers.3))/(teachers.2 + teachers.3), ind = "sal.rel.2t3") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(iso2c, year, ind, value)

  cleaned <- dplyr::bind_rows(schol, sal1, sal2) %>%
    dplyr::mutate(source = "OECD",
                  val_status = ifelse(ind == "sal.rel.2t3", "E", "A"),
                  year = as.numeric(year)) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::filter(!is.na(iso2c), !is.na(value))

}

#' un_aids_clean
#'
#' \code{un_aids_clean} is a function to clean data from the UN AIDS spreadsheet
#' on google drive.
#'
#' Information on survey used per observation is removed.
#'
#'@family clean
#'@seealso \code{\link{other}}

un_aids_clean <- function(df) {

  cleaned <- df %>%
    dplyr::mutate(year = paste0(stringi::stri_extract_first(source, regex ="[0-9]{2}"), stringi::stri_extract_last(source, regex ="[0-9]{2}"), sep =""),
                  year = suppressWarnings(ifelse(year == "NANA", NA, as.numeric(year))),
                  survey = stringr::str_replace_all(source, "Source: |[^A-Za-z ]+", ""),
                  source = "UNAIDS",
                  value = as.numeric(value),
                  ind = "hiv.prev.15t24",
                  iso2c = countrycode::countrycode(sourcevar = df$Country, origin = "country.name", destination = "iso2c"),
                  val_status = "A") %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year))

}

#' gcpea_clean
#'
#' \code{gcpea_clean} is a function to clean data from Global Campaign to
#' protect education from attack (GCPEA) on google drive.
#'
#' 'm' val_status indicates observation comes from a multi-year period and does
#' not reflect the number of attacks on education in a single year period).
#'
#'@family clean
#'@seealso \code{\link{other}}

gcpea_clean <- function(df) {

  cleaned <- df %>%
    tidyr::gather(key = "year", value = "value", - Country) %>%
    dplyr::mutate(year =  stringr::str_replace(year, pattern = "X", replacement = ""),
                  year = as.numeric(year),
                  ind = "edattacks",
                  val_status = ifelse(stringr::str_detect(value, "\\*"), "m", "A"),
                  value = stringr::str_replace(value, pattern = "\\*|-", replacement = ""),
                  value = as.numeric(value),
                  source = "GCPEA",
                  iso2c = countrycode::countrycode(sourcevar = .$Country, origin = "country.name", destination = "iso2c", warn = FALSE)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source) %>%
    dplyr::filter(!is.na(iso2c), !is.na(value)) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year))

}

#' unicef_ecce_clean
#'
#' \code{unicef_ecce_clean} is a function to clean UNICEF ECCE survey data on
#' google drive.
#'
#' Information on survey used per observation is removed.
#'@param df a datadrame in the same format as
#'  \url{https://drive.google.com/uc?export=download&id=1JzOo7rt8O3ZE9eSAL3v1jKexLwbICtM3}
#'
#'@param ind name of the indicator of the `value` column in the csv file
#'  (character).
#'@param source name of the source of the data (i.e. UNICEF)
#'@family clean
#'@seealso \code{\link{other}}


unicef_ecce_clean <- function(df, ind, source) {

  cleaned <- df %>%
    dplyr::mutate(year = paste0(stringi::stri_extract_first(survey, regex ="[0-9]{2}"), stringi::stri_extract_last(survey, regex ="[0-9]{2}"), sep =""),
                  year = suppressWarnings(ifelse(year == "NANA", NA, as.numeric(year))),
                  value = stringr::str_replace(value, "-|\\?", ""), value = suppressWarnings(as.numeric(value)),
                  survey = stringr::str_replace_all(survey, "Source: |[^A-Za-z ]+", ""),
                  source = source,
                  ind = ind,
                  iso2c = suppressWarnings(countrycode::countrycode(sourcevar = df$Country, origin = "country.name", destination = "iso2c")),
                  val_status = "A") %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source) %>%
    dplyr::group_by(iso2c, ind) %>%
    dplyr::filter(year == max(year))

}

#' unicef_wash_clean
#'
#' \code{unicef_wash_clean} is a function to clean data from the UNICEF WASH
#' file on google drive.
#'
#'@family clean
#'@seealso \code{\link{other}}


unicef_wash_clean <- function(df) {

  cleaned <- df %>%
    tidyr::gather(key = "ind", value = "value", - Country, - year) %>%
    dplyr::mutate(value = stringr::str_replace(value, "-", ""),
                  value = as.numeric(value),
                  iso2c = countrycode::countrycode(sourcevar = .$Country, origin = "country.name", destination = "iso2c", warn = FALSE),
                  year = as.numeric(year),
                  ind = dplyr::case_when(stringr::str_detect(ind, "drinking") ~ "SchBSP.WPoWat",
                                         stringr::str_detect(ind, "sanitation") ~ "SchBSP.WToilssx",
                                         stringr::str_detect(ind, "handwashing")~ "SchBSP.WHwash"),
                  val_status = "E",
                  source = "UNICEF") %>%
    dplyr::filter(!is.na(iso2c), !is.na(value)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source)

}

#' bullying_clean
#'
#' \code{bullying_clean} is a function to clean the Innocenti bullying data on
#' google drive.
#'
#' Information on survey used per observation is removed.
#'
#'@family clean
#'@seealso \code{\link{other}}

bullying_clean <- function(df) {

  cleaned <- df %>%
    dplyr::mutate(iso2c = countrycode::countrycode(sourcevar = .$Country, origin = "country.name", destination = "iso2c", warn = FALSE),
                  ind = "stu.bully",
                  value = stu.bully,
                  val_status = "A",
                  source = "Innocenti",
                  iso2c =  dplyr::case_when(Country == "Dominicana" ~ "DO",
                                            Country == "United Arab" ~ "AE",
                                            TRUE ~ iso2c),
                  value =  dplyr::case_when(value == "Low" ~ 1,
                                    value == "Medium" ~ 2,
                                    value == "High" ~ 3)) %>%
    dplyr::filter(!is.na(iso2c), !is.na(value)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source)
}

#' ict_skills_clean
#'
#' \code{ict_skills_clean} is a function to clean the ITU ICT data on
#' google drive.
#'
#'@family clean
#'@seealso \code{\link{other}}

ict_skills_clean <- function(df) {

  cleaned <- df %>%
    tidyr::gather(key = "ind", value = "value", -Country, -year) %>%
    dplyr::mutate(iso2c = countrycode::countrycode(sourcevar = .$Country, origin = "country.name", destination = "iso2c"),
                  val_status = "A",
                  source = "ITU") %>%
    dplyr::filter(!is.na(iso2c), !is.na(value)) %>%
    dplyr::select(iso2c, year, ind, value, val_status, source)
}

#' chores_clean
#'
#' \code{chores_clean} is a function to clean the child chores data on
#' google drive.
#'
#'@family clean
#'@seealso \code{\link{other}}

  chores_clean <- function(df) {
    cleaned <- df %>%
      dplyr::mutate(chores.28plus.12t14 = total,
                    chores.28plus.12t14.GPIA = ifelse(GPI > 1, 1+(1-(1/GPI)), GPI),
                    iso2c = countrycode::countrycode(sourcevar = .$country, origin = "country.name", destination = "iso2c", warn = T),
                    year = 2016,
                    val_status = "A",
                    source = "UNICEF") %>%
      tidyr::gather(key = "ind", value = "value", -total, - source, - GPI, -iso2c, -val_status, -country, -year, -variable) %>%
      dplyr::select(-country, -total, -GPI, -variable)
  }

#' weights_clean
#'
#' \code{weights_clean} is a function to clean weights data imported from SDMX
#' queries in `weights` function.
#'
#' Several variables are calculated (Y25T65, Y_GE25, Y15T64, L1_GLAST_Q1_F,
#' L1_GLAST_Q1_M, L2_GLAST_Q1_F, L2_GLAST_Q1_M, L3_Q1_F = L3_F, L3_Q1_M = L3_M).
#'@family clean
#'@seealso \code{\link{weights}}

weights_clean <- function(df) {

  uis <- df[[1]] %>%
    unique() %>%
    dplyr::mutate(wt_var = dplyr::case_when(AGE =="SCH_AGE_GROUP" & SEX == "_T" ~ EDU_LEVEL,
                                            AGE =="SCH_AGE_GROUP" & SEX != "_T" ~ paste(EDU_LEVEL, SEX, sep = "_"),
                                            AGE == "TH_ENTRY_GLAST" & SEX == "_T" ~ paste(EDU_LEVEL, "GLAST", sep = "_"),
                                            AGE == "TH_ENTRY_GLAST" & SEX != "_T" ~ paste(EDU_LEVEL, "GLAST", SEX, sep = "_"),
                                            is.na(EDU_LEVEL) | EDU_LEVEL == "_T"  ~ AGE,
                                            STAT_UNIT == "TEACH" ~ paste(STAT_UNIT, EDU_LEVEL, sep = "_"),
                                            GRADE == "GLAST" ~ paste(EDU_LEVEL, STAT_UNIT, GRADE, sep = "_"),
                                            STAT_UNIT == "ILLPOP" ~ paste(AGE, STAT_UNIT, sep = "_"),
                                            STAT_UNIT == "STU" & GRADE == "_T" ~ paste(EDU_LEVEL, STAT_UNIT, sep ="_")),
                 wt_value = dplyr::case_when(STAT_UNIT == "POP" ~ as.numeric(OBS_VALUE) *1000,
                                             OBS_STATUS == "Z" ~ NA_real_,
                                             TRUE ~ as.numeric(OBS_VALUE))) %>%
    dplyr::select(iso2c = REF_AREA, year = TIME_PERIOD, wt_var, wt_value) %>%
    dplyr::filter(!is.na(iso2c), !is.na(wt_value), nchar(iso2c) == 2)

  unpd <- df[[2]] %>%
    dplyr::mutate(REF_AREA = as.numeric(stringi::stri_replace_all_regex(REF_AREA, "\\b0*(\\d+)\\b", "$1"))) %>%
    dplyr::mutate(wt_value = as.numeric(obsValue) * 1000,
                  iso2c = countrycode::countrycode(sourcevar = .$REF_AREA, origin = "iso3n", destination = "iso2c")) %>%
    dplyr::select(iso2c, year = obsTime, wt_var = AGE, wt_value)

  clean1 <- bind_rows(uis, unpd) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(!is.na(iso2c), nchar(iso2c) == 2)

  clean2 <- clean1 %>%
    dplyr::group_by(iso2c, year) %>%
    tidyr::spread(key = wt_var, value = wt_value) %>%
    dplyr::summarise(Y25T64 = `_T` - (Y_LT5 + Y10T14 + Y15T24 + Y_GE65),
                     Y_GE25 = `_T` - (Y_LT5 + Y10T14 + Y15T24),
                     Y15T64 =  `_T` - (Y_LT5 + Y10T14 + Y_GE65),
                     L1_GLAST_Q1_F = L1_GLAST_F * .2,
                     L1_GLAST_Q1_M = L1_GLAST_M * .2,
                     L2_GLAST_Q1_F = L2_GLAST_F * .2,
                     L2_GLAST_Q1_M = L2_GLAST_M * .2,
                     L3_Q1_F = L3_F * .2,
                     L3_Q1_M = L3_M * .2) %>%
    tidyr::gather(key = "wt_var", value = "wt_value", -iso2c, -year)

  clean3 <- dplyr::bind_rows(clean1, clean2) %>%
    dplyr::group_by(iso2c, wt_var) %>%
    dplyr::filter(!all(is.na(wt_value))) %>%
    ungroup
  #   dplyr::filter(!is.na(wt_value)) %>%
  #   dplyr::mutate(wt_value_z = wt_value / mean(wt_value))

  cleaned <-
    clean3 %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(dplyr::between(year, 2006, .gemrtables.pkg.env$ref_year+1)) %>%
    tidyr::complete(tidyr::nesting(iso2c, wt_var), year) %>%
    # Approx insists on at least two values to interpolate;
    # since anyhow we want the interpolation to hold the latest value constant,
    # a value two years following the reference year is imputed here to be identical to the latest available.
    {dplyr::bind_rows(
      .,
      stats::na.omit(.) %>%
        dplyr::group_by(iso2c, wt_var) %>%
        dplyr::filter(year == max(year)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(year = .gemrtables.pkg.env$ref_year + 2)
    )} %>%
    dplyr::group_by(iso2c, wt_var) %>% #na.omit %>% filter(n() == 1)
    dplyr::mutate(wt_value = wt_value) %>%#stats::approx(year, wt_value, xout = year, rule = 2)$y
    dplyr::filter(year == .gemrtables.pkg.env$ref_year) %>%
    dplyr::ungroup()
}


#' format_wide
#'
#' \code{format_wide} is a function to format stat table data to 'wide' format.
#'
#' Rounds values, converts binary values to 'Yes/No'; converts value to unicode
#' with subscript flags; converts to list of dataframes for export to xlsx.
#'@family clean

format_wide <- function(df) {

  redenominate_6 <- c("IllPop.Ag15t24", "IllPop.Ag15t99", "OFST.1.cp", "OFST.2.cp", "OFST.3.cp", "SAP.02", "SAP.1", "SAP.2t3",
                      "SAP.5t8", "stu.per.02", "stu.per.1", "stu.per.2t3", "stu.per.5t8",
                      "odaflow.volumescholarship", "odaflow.imputecost")

  redenominate_3 <- c("IE.5t8.40510", "teach.per.02", "OE.5t8.40510", "teach.per.1", "teach.per.2t3")

  wide_data <-
    df %>%
    dplyr::mutate(value = dplyr::case_when(ind %in% redenominate_6 ~ value/1000000,
                                           ind %in% redenominate_3 ~ value/1000,
                                           TRUE ~ value)) %>%
    dplyr::group_by(ind) %>%
    dplyr::mutate(
      digits = case_when(
        max(value, na.rm = TRUE) < 2 | stringr::str_detect(ind, 'sal.rel') ~ 2,
        # (ind %in% redenominate_6 | ind %in% redenominate_3) & value >= 1000000 ~ 1,
        # (ind %in% redenominate_6 | ind %in% redenominate_3) & value < 1000000 ~ 3,
        value < 0.5 | stringr::str_detect(ind, "XGDP|XGovExp") ~ 1,
        TRUE ~ 0)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      value_str = format(round(value, digits),
                     big.mark = ',', trim = TRUE,
                     zero.print = '-',
                     nsmall = digits,
                     drop0trailing = FALSE,
                     scientific = FALSE,
                     digits = digits
                     )) %>%
    dplyr::mutate(
      value_str = ifelse(is.na(value) | entity != "country" | !stringr::str_detect(ind, "bully|esd|attack|admi"), value_str,
        ifelse(stringr::str_detect(ind, "bully"), c('Low', 'Medium', 'High')[value],
        ifelse(stringr::str_detect(ind, "esd"), c('None', 'Low', 'Medium', 'High')[value + 1],
        ifelse(stringr::str_detect(ind, "attack"), c('None', 'Sporadic', 'Affected', 'Heavy', 'Very heavy')[value + 1],
        ifelse(stringr::str_detect(ind, "admi"), c('No', 'Yes')[value + 1], value_str)
      ))))) %>%
    # dplyr::select(entity, value, val_status, ind, value_str) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
                  val_status = ifelse(val_status == "A", "", tolower(val_status)),
                  year_diff = year - .gemrtables.pkg.env$ref_year,
                  year_diff = ifelse(year_diff == 0, "", year_diff),
                  val_status_utf = dplyr::case_when(val_status == "e" ~ "\u1d62",
                                                    # val_status == "m" ~ "\u2099",
                                                    TRUE ~ ''),
                  year_diff_utf = dplyr::case_when(year_diff  ==  1 ~ "\u208A\u2081",
                                                    year_diff == -1 ~ "\u208B\u2081",
                                                    year_diff == -2 ~ "\u208B\u2082",
                                                    year_diff == -3 ~ "\u208B\u2083",
                                                    year_diff == -4 ~ "\u208B\u2084",
                                                   TRUE ~ ''),
                  val_utf = ifelse(value_str == 'NA' | is.na(value_str),
                                    "\u2026",
                                    paste0(stringr::str_trim(value_str), ifelse(stringr::str_detect(ind, "admi|esd|odaflow|Entry\\.age|Years\\.dur"), "", year_diff_utf), val_status_utf, sep = ""))) %>%
    dplyr::select(sheet, annex_name, !!.gemrtables.pkg.env$region, ind, val_utf, entity) %>%
    dplyr::mutate(ind = factor(ind, levels = unique(ind)),
                  is_aggregate = ifelse(entity == "country", "country", "aggregate"),
                  sheet = paste("sheet", sheet),
                  val_utf = stringr::str_replace_all(val_utf, "NA", ""),
                  regionx = !!.gemrtables.pkg.env$region,
                  regionx = ifelse(is_aggregate == "aggregate", annex_name, regionx)) %>%
    dplyr::left_join(.gemrtables.pkg.env$regions2[, c(1,4)], by = c("regionx" = "annex_name")) %>%
    split(list(.$is_aggregate, .$sheet)) %>%
    purrr::map(tidyr::spread, key=ind, value = val_utf) %>%
    purrr::map(function(.) dplyr::mutate(., annex_name = ifelse(entity == "subregion" | entity == "income_subgroup", paste("  ", annex_name), annex_name))) %>%
    purrr::map(function(.) dplyr::mutate(., entity = dplyr::case_when(entity == "subregion" ~ "region",
                                                                      entity == "income_subgroup" ~ "income_group",
                                                                      TRUE ~ entity))) %>%
    purrr::map(function(.) dplyr::arrange(., region_order, annex_name)) %>%
    purrr::map(mutate_all, as.character) %>%
    purrr::map_if(suppressWarnings(stringr::str_detect(., "country")), function(.) dplyr::left_join(., .gemrtables.pkg.env$regions[, c("annex_name", "iso3c")], by = "annex_name")) %>%
    purrr::map(function(.) data.table::setDT(.)[.[, c(.I, NA), entity]$V1][!.N]) %>%
    purrr::map(function(.) data.table::setDT(.)[.[, c(.I, NA), eval(.gemrtables.pkg.env$region)]$V1][!.N]) %>%
    purrr::map(function(.) dplyr::select(., -sheet, -is_aggregate, -entity, -!!.gemrtables.pkg.env$region, - regionx, -region_order))
}

#' format_long
#'
#' \code{format_long} is a function to format stat table data to 'long' format.
#'
#'@family clean

format_long <- function(){

#load/ cache for imported/cleaned country data and weights

# function to generate country_data
c_data <- function() {

  uis_data <- uis()
  cedar_data <- cedar()
  other_data <- other()
  dplyr::bind_rows(uis_data, cedar_data, other_data)
}

load_cache_data <- function(df, ref_year = .gemrtables.pkg.env$ref_year) {

  #convert file_paths into of unprocessed datasets into character vector

  source_keys = c("uis_up", "cedar_up", "wb_up", "eurostat_up", "oecd_up")
  sources <- list()
  for(i in seq_along(source_keys)) {
    sources[[i]] <- R.cache::findCache(list(source_keys[i]))
  }
  sources <- unlist(sources, use.names = FALSE)

  # 1. Try to load cached data, if already generated
  key_country <- list(df)
  key_weights <- list(df, ref_year)

  if(df == "country_data") {
    data <- R.cache::loadCache(key_country, sources = sources, removeOldCache=TRUE)
    if(length(sources) !=5) {
      data <- NULL
    }
  }else if (df == "weights_data"){
    data <- R.cache::loadCache(key_weights)
  }

  if (!is.null(data)) {
    message(glue::glue("Loaded cached {df}"))
    return(data);
  }

  # 2. If not available, generate it.
  message(glue::glue("Building {df} ..."))
  if(df == "country_data") {
    data <- c_data()
    R.cache::saveCache(data, key=key_country, comment=df)
    R.cache::loadCache(key_country)
  }else if (df == "weights_data") {
    data <- weights()
    R.cache::saveCache(data, key=key_weights, comment=df)
    R.cache::loadCache(key_weights)
  }
}

country_data <- load_cache_data("country_data")
# if (any(
#   # is.null(.gemrtables.pkg.env$schol_unspec),
#   is.null(.gemrtables.pkg.env$uis_comp),
#   FALSE)) {clearCache(prompt = FALSE)}

dac_recipients <- read.csv(system.file("config", "DAC_recipients.csv", package = "gemrtables"),
                           stringsAsFactors = FALSE) %>% na.omit

weights_data <-
  load_cache_data("weights_data") %>%
  dplyr::left_join(select(.gemrtables.pkg.env$regions, iso3c, iso2c, World, SDG.region, SDG.subregion, income_group, income_subgroup), by = 'iso2c') %>%
  tidyr::gather(wt_region, group, World:income_subgroup) %>%
  #dplyr::filter(!stringr::str_detect(ind, 'odaflow') | iso3c %in% dac_recipients$iso3c) %>%
  dplyr::select(-iso3c) %>%
  dplyr::group_by(wt_var, wt_region, group) %>%
  dplyr::mutate(wt_total = sum(wt_value, na.rm = TRUE)) %>%
  dplyr::ungroup()
pop_data <-
  weights_data %>%
  filter(wt_var == '_T') %>%
  select(iso2c, year, wt_region, group, pop = wt_value, pop_total = wt_total)
.gemrtables.pkg.env$weights_data <-
  weights_data %>%
  dplyr::left_join(select(pop_data, -year), by = c('iso2c', 'wt_region', 'group')) %>%
  dplyr::select(-year)

#clean country data and export statistical tables

country_data1 <- country_data %>%
  dplyr::right_join(.gemrtables.pkg.env$regions, by = "iso2c") %>%
  dplyr::left_join(.gemrtables.pkg.env$indicators, by = c("ind", "source")) %>%
  dplyr::filter(year >= .gemrtables.pkg.env$ref_year - year_cut)

unmatched <- dplyr::anti_join(.gemrtables.pkg.env$indicators, country_data1, by = c("ind", "source")) %>%
  dplyr::select(ind, source, sheet, position)

country_data2 <- country_data1 %>%
  dplyr::select(iso2c, annex_name, World, !!.gemrtables.pkg.env$region, !!.gemrtables.pkg.env$subregion, income_group, income_subgroup, year, ind, value, val_status, source) %>%
  dplyr::right_join(.gemrtables.pkg.env$indicators, by = c("ind", "source")) %>%
  dplyr::group_by(iso2c, ind, source) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(iso2c, ind) %>%
  dplyr::filter(priority == min(priority)) %>%
  # dplyr::left_join(weights_data[, -3], by = c("iso2c", "wt_var")) %>%
  dplyr::mutate(entity = "country") %>%
  # dplyr::left_join(select(pop_data, -year), by = c('iso2c')) %>%
  ungroup() %>%
  dplyr::mutate(year = ifelse(stringr::str_detect(ind, stringr::regex("XGDP|XGovExp", ignore_case = TRUE)) & iso2c == "ST", 2016, year),
                value = case_when(stringr::str_detect(ind, stringr::regex("XGDP", ignore_case = TRUE)) & iso2c == "ST" ~ 5.07533,
                                  stringr::str_detect(ind, stringr::regex("XGovExp", ignore_case = TRUE)) & iso2c == "ST" ~ 15.96804,
                                  TRUE ~ value))

uis_aggregates_extra <- R.cache::evalWithMemoization(uis_extra_aggs())

uis_aggregates <- R.cache::loadCache(list("uis_comp")) %>%
  dplyr::anti_join(uis_aggregates_extra, by = c('iso2c', 'var_concat', 'year')) %>%
  dplyr::bind_rows(uis_aggregates_extra) %>%
  dplyr::inner_join(.gemrtables.pkg.env$indicators, by = "var_concat") %>%
  dplyr::filter(aggregation %in% c("w_mean", "sum") & year >= (.gemrtables.pkg.env$ref_year - 4)) %>%
  dplyr::inner_join(.gemrtables.pkg.env$regions2[, 1:3], by = c("iso2c" = "code")) %>%
  dplyr::select(-iso2c)

schol_unspec <- R.cache::loadCache(list("schol_unspec"))

computed_aggregates <- country_data2 %>%
  aggregates() %>%
  dplyr::filter(!is.na(annex_name)) %>%
  dplyr::inner_join(indicators_unique, by = c("ind", "aggregation", "pc_comp_cut")) %>%
  dplyr::anti_join(uis_aggregates, by = c("annex_name", "ind")) %>%
  dplyr::mutate(value = dplyr::case_when(annex_name == "World" & ind == "odaflow.volumescholarship" ~ value + schol_unspec[[2,2]],
                                         annex_name == "World" & ind == "odaflow.imputecost" ~ value + schol_unspec[[1,2]],
                                         TRUE ~ value))

# if level_country is not TRUE parity indices are calculated directly at the region level
if(!isTRUE(.gemrtables.pkg.env$level_country)){
  region_data <- parity_indices_region()
  region_data1 <- region_data %>%
    right_join(.gemrtables.pkg.env$regions2[,1:3], by = c("annex_name"))
  left_join(.gemrtables.pkg.env$indicators, by = c("ind", "source"))
  #binding aggregates data
  computed_aggregates <- dplyr::bind_rows(computed_aggregates, region_data1)
  # unmatched indicators
  unmatched <- dplyr::anti_join(unmatched, region_data1, by = c("ind", "source")) %>%
    dplyr::select(ind, source, sheet, position)
}

long_data <- dplyr::bind_rows(country_data2, computed_aggregates, uis_aggregates)  %>%
  tidyr:: complete(tidyr::nesting(ind, sheet, position), tidyr::nesting(annex_name, !!.gemrtables.pkg.env$region, !!.gemrtables.pkg.env$subregion, entity),
                   fill = list(value = NA, val_status = "", year_diff = 0)) %>%
  dplyr::arrange(sheet, position, !!.gemrtables.pkg.env$region, entity, annex_name)

}

#' parity_indices_region
#'
#' \code{parity_indices_region} calculates adjusted parity ratios at the region level.
#'
#' @param df a data frame with a key / value columns.
#' @param col indicator key column.
#' @param a indicator for 'disadvantaged' group (numerator).
#' @param b indicator for 'advantaged' group  (denominator).
#' @param varname name for calculated indice (character).
#' @param val_status For use with data with flags for estimated observations. If
#'   `TRUE` will calculate flag for indice (requires flag column to be named
#'   `val_status` and estimates lablled as `E`.
#' @return A data frame.
#' @export
#' @family summarise
#'

parity_indices_region <- function(){
  uis_cleaned1 <- R.cache::loadCache(uis_cleaned) %>%
    dplyr::right_join(.gemrtables.pkg.env$regions, by = "iso2c") %>%
    mutate(ind = var_concat)

  vars_f <- list("STU_PT_L1__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L1__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "LR_PT__Z__Z_F_Y15T24__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "LR_PT__Z__Z_F_Y_GE15__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_READING__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_MATH__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_READING__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_MATH__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L02__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L1__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L2_3__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L5T8__T_F_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA")

  vars_m <- list("STU_PT_L1__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L1__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                 "LR_PT__Z__Z_M_Y15T24__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "LR_PT__Z__Z_M_Y_GE15__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_READING__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_MATH__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_READING__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                 "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_MATH__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L02__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L1__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L2_3__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                 "GER_PT_L5T8__T_M_SCH_AGE_GROUP__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA")

  uis_cleaned_aggregates <- uis_cleaned1 %>%
    aggregates_values() %>%
    filter(var_concat %in% c(unlist(vars_f), unlist(vars_m))) %>%
    mutate(region = paste(World, !!region, income_group, income_subgroup) %>%
             stringr::str_replace_all("NA", "") %>%
             stringr::str_squish()) %>%
    filter(nchar(region) != 0)

  parity_indices_uis <- list(df = rep(list("uis_cleaned_aggregates"), 14),
                             col = rep(list("var_concat"), 14),
                             a = vars_f,
                             b = vars_m,
                             varname = list("Read.Primary.GPIA", "Math.Primary.GPIA", "Read.LowerSec.GPIA", "Math.LowerSec.GPIA",
                                            "LR.Ag15t24.GPIA", "LR.Ag15t99.GPIA", "Read.Primary.WPIA", "Math.Primary.WPIA",
                                            "Read.LowerSec.WPIA", "Math.LowerSec.WPIA", "GER.02.GPIA", "GER.1.GPIA", "GER.2t3.GPIA",
                                            "GER.5t8.GPIA"),
                             val_status = rep(list(TRUE), 14)) %>%
    purrr::pmap(parity_adj) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(source = "UIS") %>%
    dplyr::filter(!is.na(value))

  # cedar
  cedar_cleaned1 <- R.cache::loadCache(cedar_cleaned) %>%
    dplyr::right_join(.gemrtables.pkg.env$regions, by = "iso2c") %>%
    mutate(ind = var_concat)

  cedar_cleaned_aggregates <- cedar_cleaned1 %>%
    aggregates_values() %>%
    filter(var_concat %in% c(unlist(vars_f), unlist(vars_m))) %>%
    mutate(region = paste(World, !!region, income_group, income_subgroup) %>%
             stringr::str_replace_all("NA", "") %>%
             stringr::str_squish()) %>%
    filter(nchar(region) != 0)

  vars_f = list("CR.1.f", "CR.1.rural", "CR.1.q1", "CR.2.f", "CR.2.rural", "CR.2.q1", "CR.3.f", "CR.3.rural", "CR.3.q1", "chores.28plus.12t14.f")
  vars_m = list("CR.1.m", "CR.1.urban", "CR.1.q5","CR.2.m", "CR.2.urban", "CR.2.q5", "CR.3.m", "CR.3.urban", "CR.3.q5", "chores.28plus.12t14.m")
  parity_indices_cedar <- list(df = rep(list("cedar_cleaned_aggregates"), 10),
                               col = rep(list("ind"), 10),
                               a = vars_f,
                               b = vars_m,
                               varname = list("CR.1.GPIA", "CR.1.LPIA", "CR.1.WPIA", "CR.2.GPIA", "CR.2.LPIA", "CR.2.WPIA",
                                              "CR.3.GPIA", "CR.3.LPIA", "CR.3.WPIA", "chores.28plus.12t14.GPIA")) %>%
    purrr::pmap(parity_adj) %>%
    purrr::reduce(dplyr::bind_rows)%>%
    dplyr::mutate(source = "cedar")

  # WB
  vars_f <- list("adult.profiliteracy.f","adult.profinumeracy.f")
  vars_m <- list("adult.profiliteracy.m", "adult.profinumeracy.m")

  wb_cleaned <- country_data %>%
    filter(ind %in% c(unlist(vars_f), unlist(vars_m))) %>%
    dplyr::right_join(.gemrtables.pkg.env$regions, by = "iso2c") %>%
    dplyr::filter(!is.na(ind)) %>%
    dplyr::mutate(var_concat = ind)

  wb_cleaned_aggregates <- wb_cleaned %>%
    aggregates_values() %>%
    filter(var_concat %in% c(unlist(vars_f), unlist(vars_m))) %>%
    mutate(region = paste(World, !!region, income_group, income_subgroup) %>%
             stringr::str_replace_all("NA", "") %>%
             stringr::str_squish()) %>%
    filter(nchar(region) != 0)


  parity_indices_wb <- list(df = list("wb_cleaned_aggregates","wb_cleaned_aggregates"),
                         col = list("ind",  "ind"),
                         a = list("adult.profiliteracy.f","adult.profinumeracy.f"),
                         b = list("adult.profiliteracy.m", "adult.profinumeracy.m"),
                         varname = list("adult.profiliteracy.gpia", "adult.profinumeracy.gpia")) %>%
    purrr::pmap(parity_adj) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(source = "PIAAC")

  # Binded parity indices
  dplyr::bind_rows(parity_indices_uis, parity_indices_cedar, parity_indices_wb)
}

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
#' @family summarise function
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

  indice <- df %>%
    filter(!!col %in% c(a, b))

  indice <- group_by(indice, iso2c, year) %>%
    filter(n()==2 ) %>%
    {if(val_status == FALSE) summarise(., value = ifelse(value[!!col == !!a] > value[!!col == !!b], 2 - (1/(value[!!col == !!a]/value[!!col == !!b])), value[!!col == !!a]/value[!!col == !!b]))
      else
        summarise(., value = ifelse(value[!!col == !!a] > value[!!col == !!b], 2 - (1/(value[!!col == !!a]/value[!!col == !!b])), value[!!col == !!a]/value[!!col == !!b]),
                  val_status = ifelse(val_status[!!col == !!a] == "E" | val_status[!!col == !!b] == "E", "E", "A")) } %>%
    mutate(ind = varname) %>%
    filter(!is.na(value))
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
#'@family clean function

uis_clean <- function(df) {

  clean1 <- df %>%
    unite(col = var_concat, STAT_UNIT, UNIT_MEASURE, EDU_LEVEL, EDU_CAT, SEX, AGE, GRADE, SECTOR_EDU, EDU_ATTAIN, WEALTH_QUINTILE, LOCATION,
          EDU_TYPE, EDU_FIELD, SUBJECT, INFRASTR, SE_BKGRD, TEACH_EXPERIENCE, CONTRACT_TYPE, COUNTRY_ORIGIN, REGION_DEST,
          EXPENDITURE_TYPE, SOURCE_FUND, FUND_FLOW) %>%
    select(iso2c = REF_AREA, var_concat, year = TIME_PERIOD, value = OBS_VALUE, val_status = OBS_STATUS) %>%
    mutate(value = as.numeric(value),
           value = ifelse(val_status == "Z", NA, value)) %>%
    filter(!is.na(value)) %>%
    unique()

  clean2 <- clean1 %>%
    inner_join(indicators[, 1:2], by = "var_concat")

  admin_assessment <- clean1 %>%
    filter(str_detect(var_concat, "ADMIN_NB")) %>%
    group_by(iso2c, year) %>%
    spread(key = var_concat, value = value) %>%
    mutate(admi.grade2or3prim = ifelse(ADMIN_NB_L1__T__T__T_G2_3_INST_T__Z__Z__T__T__T_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA == 1 | ADMIN_NB_L1__T__T__T_G2_3_INST_T__Z__Z__T__T__T_READING__Z__T__Z__Z_W00_W00_NA_NA_NA == 1, 1, 0),
           admi.endofprim = ifelse(ADMIN_NB_L1__T__T__T_GLAST_INST_T__Z__Z__T__T__T_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA == 1 | ADMIN_NB_L1__T__T__T_GLAST_INST_T__Z__Z__T__T__T_READING__Z__T__Z__Z_W00_W00_NA_NA_NA == 1, 1, 0),
           admi.endoflowersec = ifelse(ADMIN_NB_L2__T__T__T_GLAST_INST_T__Z__Z__T__T__T_READING__Z__T__Z__Z_W00_W00_NA_NA_NA == 1 | ADMIN_NB_L2__T__T__T_GLAST_INST_T__Z__Z__T__T__T_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA == 1, 1, 0),
           val_status = "A") %>%
    select(iso2c, year, contains("admi."), val_status) %>%
    gather(key = "ind", value = "value", -iso2c, -year, -val_status) %>%
    mutate(value = ifelse(is.na(value), 0, value))

  free_comp <- clean1 %>%
    filter(str_detect(var_concat, "FREE_EDU|COMP_EDU")) %>%
    group_by(iso2c) %>%
    spread(key = var_concat, value = value) %>%
    mutate(Comp.02 = ifelse(COMP_EDU_YR_L02__T__T__T__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA >= 1, 1, 0),
           Free.02 = ifelse(FREE_EDU_YR_L02__T__T__T__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA >= 1, 1, 0),
           Comp.2t3 = ifelse(COMP_EDU_YR_L1T3__T__T__T__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA >= 9, 1, 0),
           Free.2t3 = ifelse(FREE_EDU_YR_L1T3__T__T__T__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA >= 12, 1, 0)) %>%
    select(iso2c, year, matches("\\."), val_status) %>%
    gather(key = "ind", value = "value", -iso2c, -year, -val_status) %>%
    filter(!is.na(value))

  inbound_stu <- clean1 %>%
    filter(var_concat %in% c("STU_PER_L5T8__T__T__T__T_INST_T__Z__Z__T__T__T__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                             "MSEP_PT_L5T8__T__T__T__T_INST_T__Z__Z__T__T__T__T__Z__Z__Z__Z_W00_W00_NA_NA_NA")) %>%
    group_by(iso2c, year) %>%
    filter(n()==2) %>%
    summarise(value = value[1] * (value[2]/100),
              val_status = ifelse(val_status[1] == "E" | val_status[2] == "E", "E", "A")) %>%
    mutate(ind = "IE.5t8.40510") %>%
    select(iso2c, year, ind, value, val_status)

  parity_indices <- list(df = list("clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1"),
                         col = list("var_concat",  "var_concat", "var_concat", "var_concat", "var_concat",  "var_concat", "var_concat",
                                    "var_concat", "var_concat", "var_concat"),
                         a = list("STU_PT_L1__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L1__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T_F__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "LR_PT__Z__Z_F_Y15T24__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                                  "LR_PT__Z__Z_F_Y_GE15__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_READING__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_MATH__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_READING__Z_LOW__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q1__T__T_ISC_F00_MATH__Z_LOW__Z__Z_W00_W00_NA_NA_NA"),
                         b = list("STU_PT_L1__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L1__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_READING__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T_M__T_GLAST_INST_T__Z__T__T__T_ISC_F00_MATH__Z__T__Z__Z_W00_W00_NA_NA_NA",
                                  "LR_PT__Z__Z_M_Y15T24__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                                  "LR_PT__Z__Z_M_Y_GE15__Z__Z__Z__Z__T__Z__Z__Z__Z__Z__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_READING__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L1__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_MATH__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_READING__Z_HIGH__Z__Z_W00_W00_NA_NA_NA",
                                  "STU_PT_L2__T__T__T_GLAST_INST_T__Z_Q5__T__T_ISC_F00_MATH__Z_HIGH__Z__Z_W00_W00_NA_NA_NA"),
                         varname = list("Read.Primary.GPIA", "Math.Primary.GPIA", "Read.LowerSec.GPIA", "Math.LowerSec.GPIA",
                                        "LR.Ag15t24.GPIA", "LR.Ag15t99.GPIA", "Read.Primary.WPIA", "Math.Primary.WPIA",
                                        "Read.LowerSec.WPIA", "Math.LowerSec.WPIA"),
                         val_status = list(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)) %>%
    pmap(parity_adj) %>%
    reduce(bind_rows)


  cleaned <- bind_rows(clean2, admin_assessment, free_comp, inbound_stu, parity_indices) %>%
    mutate(source = "UIS", year = as.numeric(year)) %>%
    select(iso2c, year, ind, value, val_status, source) %>%
    filter(!is.na(value))

}

#' cedar_clean
#'
#' \code{cedar_clean} is a function to clean data imported by \code{read_cedar}.
#'
#' Cleans data imported from cedar database and computes several variables
#' (CR.1.GPIA, CR.1.LPIA, CR.1.WPIA, CR.2.GPIA, CR.2.LPIA, CR.2.WPIA, CR.3.GPIA,
#' CR.3.LPIA, CR.3.WPIA, chores.28plus.12t14.GPIA)
#'@family clean function
#'@seealso \code{\link{read_cedar}}

cedar_clean <- function(df) {

  clean1 <- df %>%
    mutate(ind = case_when(indicator == "trans_prim_m" & is.na(sex_id) ~ "TranRA.2.GPV.cp",
                           indicator == "comp_prim_v2_m" & is.na(sex_id)  & is.na(location_id) & is.na(wealth_id) ~ "CR.1",
                           indicator == "comp_prim_v2_m" & sex_id == 1 & is.na(wealth_id) ~ "CR.1.f",
                           indicator == "comp_prim_v2_m" & sex_id == 2 & is.na(wealth_id) ~ "CR.1.m",
                           indicator == "comp_prim_v2_m" & location_id == 5 ~ "CR.1.rural",
                           indicator == "comp_prim_v2_m" & location_id == 6 ~ "CR.1.urban",
                           indicator == "comp_prim_v2_m" & wealth_id == 6 & is.na(sex_id) ~ "CR.1.q1",
                           indicator == "comp_prim_v2_m" & wealth_id == 10 & is.na(sex_id) ~ "CR.1.q5",
                           indicator == "comp_prim_v2_m" & wealth_id == 6 & sex_id == 1 ~ "CR.1.q1.f",
                           indicator == "comp_prim_v2_m" & wealth_id == 10 & sex_id == 2 ~ "CR.1.q1.m",
                           indicator == "comp_lowsec_v2_m"  & is.na(sex_id)  & is.na(location_id) & is.na(wealth_id) ~ "CR.2",
                           indicator == "comp_lowsec_v2_m" & sex_id == 1 & is.na(location_id) & is.na(wealth_id) ~ "CR.2.f",
                           indicator == "comp_lowsec_v2_m" & sex_id == 2 & is.na(location_id) & is.na(wealth_id) ~ "CR.2.m",
                           indicator == "comp_lowsec_v2_m" & location_id == 5 ~ "CR.2.rural",
                           indicator == "comp_lowsec_v2_m" & location_id == 6 ~ "CR.2.urban",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 6 & is.na(sex_id) ~ "CR.2.q1",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 10 & is.na(sex_id) ~ "CR.2.q5",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 6 & sex_id == 1 ~ "CR.2.q1.f",
                           indicator == "comp_lowsec_v2_m" & wealth_id == 10 & sex_id == 2 ~ "CR.2.q1.m",
                           indicator == "comp_upsec_v2_m" & is.na(sex_id)  & is.na(location_id) & is.na(wealth_id) ~ "CR.3",
                           indicator == "comp_upsec_v2_m" & sex_id == 1 & is.na(wealth_id) ~ "CR.3.f",
                           indicator == "comp_upsec_v2_m" & sex_id == 2 & is.na(wealth_id) ~ "CR.3.m",
                           indicator == "comp_upsec_v2_m" & location_id == 5 ~ "CR.3.rural",
                           indicator == "comp_upsec_v2_m" & location_id == 6 ~ "CR.3.urban",
                           indicator == "comp_upsec_v2_m" & wealth_id == 6 & is.na(sex_id) ~ "CR.3.q1",
                           indicator == "comp_upsec_v2_m" & wealth_id == 10 & is.na(sex_id) ~ "CR.3.q5",
                           indicator == "comp_upsec_v2_m" & wealth_id == 6 & sex_id == 1 ~ "CR.3.q1.f",
                           indicator == "comp_upsec_v2_m" & wealth_id == 10 & sex_id == 2 ~ "CR.3.q1.m",
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
           value = case_when(str_detect(ind, "esd") & value >=2 ~ 1,
                             str_detect(ind, "esd") & value <2 ~ 0,
                             !str_detect(ind, "esd") ~ value)) %>%
    inner_join(regions, by = c("country_code" = "iso3c")) %>%
    filter(!is.na(ind))

  parity_indices <- list(df = list("clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1", "clean1"),
                         col = list("ind",  "ind", "ind", "ind", "ind",  "ind", "ind", "ind", "ind", "ind"),
                         a = list("CR.1.f", "CR.1.rural", "CR.1.q1", "CR.2.f", "CR.2.rural", "CR.2.q1", "CR.3.f", "CR.3.rural", "CR.3.q1", "chores.28plus.12t14.f"),
                         b = list("CR.1.m", "CR.1.urban", "CR.1.q5","CR.2.m", "CR.2.urban", "CR.2.q5", "CR.3.m", "CR.3.urban", "CR.3.q5", "chores.28plus.12t14.m"),
                         varname = list("CR.1.GPIA", "CR.1.LPIA", "CR.1.WPIA", "CR.2.GPIA", "CR.2.LPIA", "CR.2.WPIA",
                                        "CR.3.GPIA", "CR.3.LPIA", "CR.3.WPIA", "chores.28plus.12t14.GPIA")) %>%
    pmap(parity_adj) %>%
    reduce(bind_rows)

  cleaned <- bind_rows(clean1, parity_indices) %>%
    ungroup() %>%
    select(iso2c, year, ind, value) %>%
    mutate(source = "cedar") %>%
    filter(!is.na(value)) %>%
    unique()

}

#' wb_clean
#'
#' \code{wb_clean} is a function to clean data imported by \code{[wbstats]{wb}} within \code{\link{other}} .
#'
#' Cleans data imported by \code{[wbstats]{wb}} and computes two variables
#' (adult.profiliteracy.gpia, adult.profinumeracy.gpia)
#'@family clean function
#'@seealso \code{\link[wbstats]{wb}}, \code{\link{other}}

wb_clean <- function(df) {

  clean1 <- df %>%
    mutate(ind = case_when(indicatorID == "SH.STA.STNT.ZS" ~ "stunt.u5",
                           indicatorID == "LO.TIMSS.SCI8.LOW" | str_detect(indicatorID, "PISA")  ~ "sci.lowerSec",
                           str_detect(indicatorID, "PIAAC.LIT.YOU")  ~ "youth.profiliteracy",
                           str_detect(indicatorID, "PIAAC.NUM.YOU")  ~ "youth.profinumeracy",
                           str_detect(indicatorID, "LIT.BE|LIT.1")  ~ "adult.profinumeracy",
                           str_detect(indicatorID, "NUM.BE|NUM.1")  ~ "adult.profiliteracy",
                           str_detect(indicatorID, "LIT.MA.BE|LIT.MA.1")  ~ "adult.profiliteracy.m",
                           str_detect(indicatorID, "LIT.FE.BE|LIT.FE.1")  ~ "adult.profiliteracy.f",
                           str_detect(indicatorID, "NUM.MA.BE|NUM.MA.1")  ~ "adult.profinumeracy.m",
                           str_detect(indicatorID, "NUM.FE.BE|NUM.FE.1")  ~ "adult.profinumeracy.f"),
           source = case_when(indicatorID == "SH.STA.STNT.ZS" ~ "World Bank",
                              indicatorID == "LO.TIMSS.SCI8.LOW" ~ "TIMSS",
                              str_detect(indicatorID, "PISA")  ~ "PISA",
                              str_detect(indicatorID, "PIAAC")  ~ "PIAAC"))

  clean2 <- clean1 %>%
    filter(str_detect(indicatorID, "PISA|PIAAC")) %>%
    group_by(source, iso2c, ind, date) %>%
    summarise(value = 100 - (sum(value)))  %>%
    bind_rows(clean1) %>%
    filter(indicatorID %in% c("SH.STA.STNT.ZS", "LO.TIMSS.SCI8.LOW", NA)) %>%
    select(iso2c, ind, year = date, value, source)

  parity_indices <- list(df = list("clean2","clean2"),
                         col = list("ind",  "ind"),
                         a = list("adult.profiliteracy.f","adult.profinumeracy.f"),
                         b = list("adult.profiliteracy.m", "adult.profinumeracy.m"),
                         varname = list("adult.profiliteracy.gpia", "adult.profinumeracy.gpia")) %>%
    pmap(parity_adj) %>%
    reduce(bind_rows) %>%
    mutate(source = "PIAAC")

  cleaned <- bind_rows(clean2, parity_indices) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year), !is.na(value)) %>%
    mutate(val_status = "A",
           year = as.numeric(year)) %>%
    select(iso2c, year, ind, value, val_status, source)

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
    mutate(ind = case_when(TRAINING == "FE_NFE" ~ "prya.25t64",
                           INDIC_IS == "I_CCPY"  ~ "yadult.porcentICTskill.copi"),
           source = "eurostat",
           val_status = "A",
           obsTime = as.numeric(obsTime)) %>%
    select(iso2c = GEO, year = obsTime, ind, source, value = obsValue, val_status ) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year))

}

#' oecd_clean
#'
#' \code{oecd_clean} is a function to clean data from OECD API queries defined
#' in \code{other}.
#'
#' Cleans data from OECD api queries and computes one variable.
#' (sal.rel.2t3,)
#'
#'@family clean function
#'@seealso \code{\link{other}}

oecd_clean <- function(df) {

  schol <- df[[1]] %>%
    mutate(ind = case_when(AIDTYPE == "E02" ~ "odaflow.imputecost",
                           AIDTYPE == "E01"  ~ "odaflow.volumescholarship"),
           RECIPIENT = as.numeric(RECIPIENT)) %>%
    left_join(regions, by = c("RECIPIENT" = "oecd.crs.recipientcode")) %>%
    select(iso2c, year = REFERENCEPERIOD, ind, value = obsValue)

  sal1 <- df[[2]] %>%
    mutate(ind = case_when(ISC11 == "L0" ~ "sal.rel.02",
                           ISC11 == "L1" ~ "sal.rel.1",
                           ISC11 == "L2_C4" ~ "sal.rel.2",
                           ISC11 == "L3_C4" ~ "sal.rel.3")) %>%
    inner_join(regions, by = c("COUNTRY" = "iso3c")) %>%
    select(iso2c, year = YEAR, ind, value = obsValue)

  sal2 <- df[[3]] %>%
    mutate(ind = case_when(ISC11_LEVEL == "L2" ~ "teachers.2",
                           ISC11_LEVEL == "L3" ~ "teachers.3")) %>%
    inner_join(regions, by = c("COUNTRY" = "iso3c")) %>%
    select(iso2c, year = obsTime, ind, value = obsValue) %>%
    bind_rows(sal1)  %>%
    spread(key = ind, value = value) %>%
    mutate(value = ((sal.rel.2 * teachers.2) + (sal.rel.3 * teachers.3))/(teachers.2 + teachers.3), ind = "sal.rel.2t3") %>%
    select(iso2c, year, ind, value)

  cleaned <- bind_rows(schol, sal1, sal2) %>%
    mutate(source = "OECD",
           val_status = "A",
           year = as.numeric(year)) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year)) %>%
    filter(!is.na(iso2c), !is.na(value))

}


#' un_aids_clean
#'
#' \code{un_aids_clean} is a function to clean data from the UN AIDS spreadsheet
#' on google drive.
#'
#' Information on survey used per observation is removed.
#'
#'@family clean function
#'@seealso \code{\link{other}}

un_aids_clean <- function(df) {

  cleaned <- df %>%
    mutate(year = paste0(stri_extract_first(source, regex ="[0-9]{2}"), stri_extract_last(source, regex ="[0-9]{2}"), sep =""),
           year = suppressWarnings(ifelse(year == "NANA", NA, as.numeric(year))),
           survey = str_replace_all(source, "Source: |[^A-Za-z ]+", ""),
           source = "UNAIDS",
           value = as.numeric(value),
           ind = "hiv.prev.15t24",
           iso2c = countrycode(sourcevar = df$Country, origin = "country.name", destination = "iso2c"),
           val_status = "A") %>%
    filter(!is.na(year)) %>%
    select(iso2c, year, ind, value, val_status, source) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year))

}

#' gcpea_clean
#'
#' \code{gcpea_clean} is a function to clean data from Global Campaign to
#' protect education from attack (GCPEA) on google drive.
#'
#' 'm' val_status indicates observation comes from a multi-year period and does
#' not reflect the number of attacks on education in a single year period).
#'
#'@family clean function
#'@seealso \code{\link{other}}

gcpea_clean <- function(df) {

  cleaned <- df %>%
    gather(key = "year", value = "value", - Country) %>%
    mutate( year =  str_replace(year, pattern = "X", replacement = ""),
            year = as.numeric(year),
            ind = "edattacks",
            val_status = ifelse(str_detect(value, "\\*"), "m", "A"),
            value = str_replace(value, pattern = "\\*|-", replacement = ""),
            value = as.numeric(value),
            source = "GCPEA",
            iso2c = countrycode(sourcevar = .$Country, origin = "country.name", destination = "iso2c", warn = FALSE)) %>%
    select(iso2c, year, ind, value, val_status, source) %>%
    filter(!is.na(iso2c), !is.na(value)) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year))

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
#'@family clean function
#'@seealso \code{\link{other}}


unicef_ecce_clean <- function(df, ind, source) {

  cleaned <- df %>%
    mutate(year = paste0(stri_extract_first(survey, regex ="[0-9]{2}"), stri_extract_last(survey, regex ="[0-9]{2}"), sep =""),
           year = suppressWarnings(ifelse(year == "NANA", NA, as.numeric(year))),
           value = str_replace(value, "-|\\?", ""), value = suppressWarnings(as.numeric(value)),
           survey = str_replace_all(survey, "Source: |[^A-Za-z ]+", ""),
           source = source,
           ind = ind,
           iso2c = suppressWarnings(countrycode(sourcevar = df$Country, origin = "country.name", destination = "iso2c")),
           val_status = "A") %>%
    filter(!is.na(year)) %>%
    select(iso2c, year, ind, value, val_status, source) %>%
    group_by(iso2c, ind) %>%
    filter(year == max(year))

}

#' unicef_wash_clean
#'
#' \code{unicef_wash_clean} is a function to clean data from the UNICEF WASH
#' file on google drive.
#'
#'@family clean function
#'@seealso \code{\link{other}}


unicef_wash_clean <- function(df) {

  cleaned <- df %>%
    gather(key = "ind", value = "value", - Country, - year) %>%
    mutate(value = str_replace(value, "-", ""),
           value = as.numeric(value),
           iso2c = countrycode(sourcevar = .$Country, origin = "country.name", destination = "iso2c", warn = FALSE),
           year = as.numeric(year),
           ind = case_when(str_detect(ind, "drinking") ~ "SchBSP.WPoWat",
                           str_detect(ind, "sanitation") ~ "SchBSP.WToilssx",
                           str_detect(ind, "handwashing")~ "SchBSP.WHwash"),
           val_status = "E",
           source = "UNICEF") %>%
    filter(!is.na(iso2c), !is.na(value)) %>%
    select(iso2c, year, ind, value, val_status, source)

}

#' weights_clean
#'
#' \code{weights_clean} is a function to clean weights data imported from SDMX
#' queries in `weights` function.
#'
#' Several variables are calculated (Y25T65, Y_GE25, Y15T64, L1_GLAST_Q1_F,
#' L1_GLAST_Q1_M, L2_GLAST_Q1_F, L2_GLAST_Q1_M, L3_Q1_F = L3_F, L3_Q1_M = L3_M).
#'@family clean function
#'@seealso \code{\link{weights}}

weights_clean <- function(df) {

  clean1 <- df %>%
    unique() %>%
    mutate(wt_var = case_when(AGE =="SCH_AGE_GROUP" & SEX == "_T" ~ EDU_LEVEL,
                              AGE =="SCH_AGE_GROUP" & SEX != "_T" ~ paste(EDU_LEVEL, SEX, sep = "_"),
                              AGE == "TH_ENTRY_GLAST" & SEX == "_T" ~ paste(EDU_LEVEL, "GLAST", sep = "_"),
                              AGE == "TH_ENTRY_GLAST" & SEX != "_T" ~ paste(EDU_LEVEL, "GLAST", SEX, sep = "_"),
                              is.na(EDU_LEVEL) | EDU_LEVEL == "_T"  ~ AGE,
                              STAT_UNIT == "TEACH" ~ paste(STAT_UNIT, EDU_LEVEL, sep = "_"),
                              GRADE == "GLAST" ~ paste(EDU_LEVEL, STAT_UNIT, GRADE, sep = "_"),
                              STAT_UNIT == "ILLPOP" ~ paste(AGE, STAT_UNIT, sep = "_"),
                              STAT_UNIT == "STU" & GRADE == "_T" ~ paste(EDU_LEVEL, STAT_UNIT)),
           year = case_when(is.na(obsTime) ~ TIME_PERIOD,
                            is.na(TIME_PERIOD) ~ obsTime),
           wt_value = case_when(is.na(OBS_VALUE) ~ obsValue,
                                is.na(obsValue) ~ as.numeric(OBS_VALUE)),
           iso3n = suppressWarnings(ifelse(str_detect(REF_AREA, "[0-9]"), as.numeric(REF_AREA), NA))) %>%
    left_join(regions, by = "iso3n") %>%
    mutate(iso2c = ifelse(is.na(iso2c) & !str_detect(REF_AREA, "[0-9]"), REF_AREA, iso2c),
           wt_value = ifelse(STAT_UNIT == "SAP", wt_value/1000, wt_value)) %>%
    select(iso2c, year, wt_var, wt_value) %>%
    filter(!is.na(iso2c))

  clean2 <- clean1 %>%
    group_by(iso2c, year) %>%
    spread(key = wt_var, value = wt_value) %>%
    summarise(Y25T65 = `_T` - (Y_LT5 + Y10T14 + Y15T24 + Y_GE65),
              Y_GE25 = `_T` - (Y_LT5 + Y10T14 + Y15T24),
              Y15T64 =  `_T` - (Y_LT5 + Y10T14 + Y_GE65),
              L1_GLAST_Q1_F = L1_GLAST_F * .2,
              L1_GLAST_Q1_M = L1_GLAST_M * .2,
              L2_GLAST_Q1_F = L2_GLAST_F * .2,
              L2_GLAST_Q1_M = L2_GLAST_M * .2,
              L3_Q1_F = L3_F * .2,
              L3_Q1_M = L3_M * .2) %>%
    gather(key = "wt_var", value = "wt_value", -iso2c, -year)

  cleaned <- bind_rows(clean1, clean2) %>%
    group_by(year, wt_var) %>%
    filter(!is.na(wt_value)) %>%
    mutate(wt_value_z = (wt_value - mean(wt_value))/sd(wt_value))

}


#' format_wide
#'
#' \code{format_wide} is a function to format stat table data to 'wide' format.
#'
#' Rounds values, converts binary values to 'Yes/No'; converts value to unicode
#' with subscript flags; converts to list of dataframs for export to xlsx.
#'@family clean function

format_wide <- function(df) {

  wide_data <- df %>%
    mutate(value = case_when(str_detect(ind, regex("wpia|gpia|lpia|sal\\.rel", ignore_case = TRUE)) ~ round(value, digits = 2),
                             str_detect(ind, regex("XGDP|scholarship", ignore_case = TRUE)) ~ round(value, digits = 1),
                             !str_detect(ind, regex("wpia|gpia|lpia|sal\\.rel|XGDP|scholarship", ignore_case = TRUE)) ~ round(value)),
           value = ifelse(is.na(value), "", value),
           value = ifelse(entity == "country" & aggregation == "pc_true", ifelse(value==1, "Yes", "No"), value),
           val_status = ifelse(val_status == "A", "", tolower(val_status)),
           year_diff = year - ref_year, year_diff = ifelse(year_diff == 0, "", year_diff),
           val_status_utf = case_when(val_status == "e" ~ "\u1d62",
                                      val_status == "m" ~ "\u2099"),
           year_diff_utf = case_when(year_diff == 1 ~ "\u208A\u2081",
                                     year_diff == -1 ~ "\u208B\u2081",
                                     year_diff == -2 ~ "\u208B\u2082",
                                     year_diff == -3 ~ "\u208B\u2083",
                                     year_diff == -4 ~ "\u208B\u2084"),
           val_utf = paste0(value, year_diff_utf, val_status_utf, sep = "")) %>%
    select(sheet, annex_name, SDG.region, ind, val_utf, entity) %>%
    mutate(ind = factor(ind, levels = unique(ind)),
           is_aggregate = ifelse(entity == "country", "country", "aggregate"),
           sheet = paste("sheet", sheet),
           val_utf = str_replace_all(val_utf, "NA", "")) %>%
    split(list(.$is_aggregate, .$sheet)) %>%
    map(spread, key=ind, value = val_utf) %>%
    map(function(.) arrange(., SDG.region, entity, annex_name)) %>%
    map(mutate_all, as.character) %>%
    map(function(.) setDT(.)[.[, c(.I, NA), entity]$V1][!.N]) %>%
    map(function(.) setDT(.)[.[, c(.I, NA), SDG.region]$V1][!.N]) %>%
    map(function(.) select(., -sheet, - entity, -is_aggregate, -SDG.region))

}

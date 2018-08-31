# fallback access to data.uis.unesco.org SDMX interface,
# for aggregates missing from api.uis.unesco.org

get_uis_altcodes <- function() {

  dsd_uis <- rsdmx::readSDMX("http://data.uis.unesco.org/RestSDMX/sdmx.ashx/GetDataStructure/EDULIT_DS", resource = "datastructure")

  uis_qualcodes <- as.data.frame(slot(dsd_uis, "codelists"), codelistId = "CL_EDULIT_DS_OBS_STATUS") %>%
    dplyr::select(-label.fr, -description.fr)

  uis_indcodes <- as.data.frame(slot(dsd_uis, "codelists"), codelistId = "CL_EDULIT_DS_EDULIT_IND") %>%
    dplyr::select(-label.fr, -description.fr)

  uis_loccodes <- as.data.frame(slot(dsd_uis, "codelists"), codelistId = "CL_EDULIT_DS_LOCATION") %>%
    dplyr::select(-label.fr, -description.fr, -description.en) %>%
    dplyr::filter(parentCode %in% c('SDG', 'WB')) %>%
    dplyr::filter(!id %in% c('40651', '40656', '40334', '40344', '40330')) %>%
    dplyr::mutate(label.en = stringr::str_trim(stringr::str_replace_all(label.en, "(.*)\\((.*)\\)", "\\2 \\1"))) %>%
    dplyr::mutate(label.en = stringr::str_trim(stringr::str_replace_all(label.en, "income countries", ""))) %>%
    dplyr::mutate(label.en = dplyr::case_when(
      label.en == 'Northern America and Europe' ~ 'Europe and Northern America',
      label.en == 'Western Asia and Northern Africa' ~ 'Northern Africa and Western Asia',
      TRUE ~ label.en
    ))

  list(uis_indcodes = uis_indcodes, uis_loccodes = uis_loccodes, uis_qualcodes = uis_qualcodes)
}

paste_sdmx <- function(x) paste(x, sep = "", collapse = "+")

uis_extra_aggs <- function() {
  uis_altcodes <- get_uis_altcodes()

  rsdmx::readSDMX(stringr::str_glue("http://data.uis.unesco.org/RestSDMX/sdmx.ashx/GetData/EDULIT_DS/\\
{paste_sdmx(c('26637','20060','20062','20082','25053','20160','20162','20182'))}.\\
{paste_sdmx(uis_altcodes$uis_loccodes$id)}/all?startTime={.gemrtables.pkg.env$ref_year-4}&endTime={.gemrtables.pkg.env$ref_year}")) %>%
    as.data.frame %>%
    dplyr::left_join(uis_altcodes$uis_loccodes, by = c('LOCATION' = 'id')) %>%
    dplyr::select(ind = EDULIT_IND, year = obsTime, value = obsValue, val_status = OBS_STATUS, annex_name = label.en) %>%
    dplyr::left_join(uis_altcodes$uis_indcodes, by = c('ind' = 'id')) %>%
    dplyr::select(year, value, val_status, annex_name, ind_lab = label.en) %>%
    dplyr::mutate(ind_lab = dplyr::case_when(
      stringr::str_detect(ind_lab, 'Enrolment') & !stringr::str_detect(ind_lab, 'all programmes') ~ stringr::str_replace_all(ind_lab, 'both sexes', 'all programmes, both sexes'),
      stringr::str_detect(ind_lab, 'Teachers') ~ stringr::str_replace_all(ind_lab, 'all programmes, both sexes', 'both sexes'),
      stringr::str_detect(ind_lab, 'inbound') ~ stringr::str_replace_all(ind_lab, 'mobile students', 'mobile tertiary students, all countries'),
      TRUE ~ ind_lab
      )
    ) %>%
    dplyr::left_join(dplyr::select(.gemrtables.pkg.env$indicators, var_concat, ind_lab), by = 'ind_lab') %>%
    dplyr::left_join(.gemrtables.pkg.env$regions2, by = 'annex_name') %>%
    dplyr::mutate(val_status = dplyr::case_when(
             val_status %in% c('+', '‡') ~ 'E',
             TRUE ~ 'A'
           )) %>%
    dplyr::group_by(code, var_concat) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::select(iso2c = code, var_concat, year, value, val_status)
}

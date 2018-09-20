#' inds
#'
#' \code{inds} is a function to import statistical table indicators.
#'
#' This dataframe defines indicators to be included in the statistical tables.
#' It's purpose is to filter, rename and order variables, and define methods of
#' aggregation.
#'@family import/clean

inds <- function() {
  read.csv(system.file("config", "indicators.csv", package = "gemrtables"), stringsAsFactors = FALSE) %>%
  # read.csv("https://drive.google.com/uc?export=download&id=1_VGBQ3x4DDrcmEO_5192oPP4xjOuz12I", stringsAsFactors = FALSE) %>%
    dplyr::mutate(var_concat = ifelse(var_concat == "", NA, var_concat)) %>%
    dplyr::as_tibble()
}

#' region_groups
#'
#' \code{region_groups} is a function to import country names and regional groupings.
#'
#' This dataframe defines various country names, iso / OECD CRS codes, and
#' regional groupings on which to calculate aggregates.
#'@family import/clean
#'
region_groups <- function() {
  read.csv(system.file("config", "regions.csv", package = "gemrtables"), stringsAsFactors = FALSE) %>%
  # read.csv("https://drive.google.com/uc?export=download&id=13-dMaPNS6-DwzMTxhIR4OKP2zrm5s_dx", stringsAsFactors = FALSE) %>%
    dplyr::mutate(iso2c = ifelse(annex_name == "Namibia", "NA", iso2c))
}

#' region_groups2
#'
#' \code{region_groups2} is a function import regional groupings, ordering, and REF_AREA api codes.
#'
#'@family import/clean
#'
region_groups2 <- function() {
  read.csv(system.file("config", "regions2.csv", package = "gemrtables"), stringsAsFactors = FALSE)
  # read.csv("https://drive.google.com/uc?export=download&id=1Izklg_r1eNPN-ECLGkagRnp5u9K2EfYt", stringsAsFactors = FALSE)
}


#' weights
#'
#' \code{weights} is a function to import and clean weights data
#'
#' Defines SDMX queries to the UIS / UN APIs and applies the `weights_clean`
#' function
#'@family import/clean
#'@seealso \code{\link{weights_clean}}
#'
weights <- function() {

  key <- list("weights_up")
  weights_up <- R.cache::loadCache(key)

  if(is.null(weights_up)) {
    cat(paste("   generating", key[[1]], "from scratch...\n", sep = " "))

    uis <- list("https://api.uis.unesco.org/sdmx/data/UNESCO,DEM_ECO,1.0/POP.PER._T.Y15T24+Y_GE65+_T.?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2017&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,DEM_ECO,1.0/ILO_POP.PER._T.Y_GE15.?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2017&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER._T._T._T.Y3T7..INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L02._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2010&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2011&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2010&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2011&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L3._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2010&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L3._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2011&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2_3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1T3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L4._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L5T8._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1._T..TH_ENTRY_GLAST._T..............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2010&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1._T..TH_ENTRY_GLAST._T..............?format=sdmx-compact-2.1&startPeriod=2011&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2._T..TH_ENTRY_GLAST._T..............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L1+L2+L3._T._T._T._T.INST_T...._T.........?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L2_3+L4+L5T8._T._T._T._T.INST_T...._T.........?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L1.._T..GLAST.INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ILLPOP.PER..._T.Y_GE15+Y15T24...............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TEACH.PER.L1+L02+L2_3+L5T8._T._T._T..INST_T.............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=",
                "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER._T.._T.Y12T14...............?format=sdmx-compact-2.1&startPeriod=2005&endPeriod=2030&subscription-key=") %>%
    read_urls(key = .gemrtables.pkg.env$key)

  unpd <-  read_urls("http://data.un.org/ws/rest/data/DF_UNDATA_WPP/SP_POP_TOTL.A.Y_LT5+Y5T10+Y10T14._T.....?startPeriod=2005&endPeriod=2017")

  weights_up <- list(uis, unpd)

  R.cache::saveCache(weights_up, key=key, comment="weights_up")

  }
  weights_up %>%
    weights_clean()
}



#' other
#'
#' \code{other} is a function to import and clean miscellaneous data.
#'
#' Defines  queries to the Eurostat, OECD and World Bank APIs, miscellaneous
#' spreadsheets, and applies respective `clean` functions.
#'@family import/clean
#'@seealso #'\code{\link{eurostat_clean}},  '\code{\link{gcpea_clean}},  '\code{\link{un_aids_clean}},
#'  '\code{\link{unicef_ecce_clean}},  \code{\link{unicef_wash_clean}},  \code{\link{wb_clean}},

other <- function() {

  #world bank data

  wb_import <-  function() {

    key <- list("wb_up")
    wb_up <- R.cache::loadCache(key)

    if(is.null(wb_up)) {
      cat(paste("   generating", key[[1]], "from scratch...\n", sep = " "))

      wb_up <- wbstats::wb(country = "countries_only", indicator = c("SH.STA.STNT.ZS",
                                                                     "LO.TIMSS.SCI8.LOW", "LO.PISA.SCI.0", "LO.PISA.SCI.1B", "LO.PISA.SCI.1A",
                                                                     "LO.PIAAC.LIT.YOU.BE", "LO.PIAAC.LIT.YOU.1", "LO.PIAAC.LIT.BE", "LO.PIAAC.LIT.1",
                                                                     "LO.PIAAC.NUM.YOU.BE", "LO.PIAAC.NUM.YOU.1", "LO.PIAAC.NUM.BE", "LO.PIAAC.NUM.1",
                                                                     "LO.PIAAC.LIT.FE.BE", "LO.PIAAC.LIT.FE.1", "LO.PIAAC.LIT.MA.BE", "LO.PIAAC.LIT.MA.1",
                                                                     "LO.PIAAC.NUM.FE.BE", "LO.PIAAC.NUM.FE.1", "LO.PIAAC.NUM.MA.BE", "LO.PIAAC.NUM.MA.1"))
      R.cache::saveCache(wb_up, key=key, comment="wb_up")
    }

    wb_up %>%
      wb_clean()
  }

  wb_data <- wb_import()


  #eurostat data

  eurostat_import <-  function() {

    key <- list("eurostat_up")
    eurostat_up <- R.cache::loadCache(key)

    if(is.null(eurostat_up)) {
      cat(paste("   generating", key[[1]], "from scratch...\n", sep = " "))

      eurostat_up <- list("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/trng_aes_100/.T.FE_NFE.PC../?startperiod=2010&endPeriod=2050",
                          "ec.europa.eu/eurostat/SDMX/diss-web/rest/data/isoc_sk_cskl_i/A.I_CCPY.IND_TOTAL.PC_IND./?startperiod=2010&endPeriod=2050") %>%
        read_urls()

      R.cache::saveCache(eurostat_up, key=key, comment="eurostat_up")
    }

    eurostat_up %>%
      eurostat_clean()
  }

  eurostat_data <- eurostat_import()

  #oecd data

  oecd_import <-  function() {

    key <- list("oecd_up")
    oecd_up <- R.cache::loadCache(key)

    if(is.null(oecd_up)) {
      cat(paste("   generating", key[[1]], "from scratch...\n", sep = " "))

      oecd_up <- list("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20005..1000.100.100.D.112.E01+E02/all?startTime=2016&endTime=2050",
                      "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_TS_ACT/..L0+L1+L2_C4+L3_C4.Y25T64.T.RATIO_ACTL_TER/all?",
                      "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EDU_PERS_INST/.T.INST_T.T.L2+L3.T.TEACH.PER/all?startTime=2010&endTime=2050") %>%
        read_urls(bind = FALSE)

      R.cache::saveCache(oecd_up, key=key, comment="oecd_up")
    }

    oecd_up %>%
      oecd_clean()
  }

  oecd_data <- oecd_import()

  #google drive data

  un_aids_data <- read.csv("https://drive.google.com/uc?export=download&id=1S6E4WwosJHjpu-d557tUjlJ6awMAwLsa", stringsAsFactors = FALSE) %>%
    un_aids_clean()

  gcpea_data <- read.csv("https://drive.google.com/uc?export=download&id=1tlya_2MkfFuAZC_fXw8Q3BZqZ3Qq08Q1", stringsAsFactors = FALSE) %>%
    gcpea_clean()

  unicef_wash_data <- read.csv("https://drive.google.com/uc?export=download&id=1I4XbP4fCzlkE-e-98BVgh6UPjM0oEHDI", stringsAsFactors = FALSE) %>%
    unicef_wash_clean()

  unicef_ecce_learn_data <- read.csv("https://drive.google.com/uc?export=download&id=1JzOo7rt8O3ZE9eSAL3v1jKexLwbICtM3", stringsAsFactors = FALSE) %>%
    unicef_ecce_clean(ind = "home.lrn.env.3t7", source = "UNICEF")

  unicef_ecce_books_data <- read.csv("https://drive.google.com/uc?export=download&id=15oDSTYqYm4Z4lHx7nQi31k_G1tjHfcGz", stringsAsFactors = FALSE) %>%
    unicef_ecce_clean(ind = "home.book.u5", source = "UNICEF")

  bullying_data <- read.csv("https://drive.google.com/uc?export=download&id=1QgrTGKWuhlgd_oYtCKAiUw4SSgdj4xts", stringsAsFactors = FALSE) %>%
    bullying_clean()

  ict_skills_data <- read.csv("https://drive.google.com/uc?export=download&id=1dV4T7RSDbJmYvhy7ydUzLCkLCTcUkZzl", stringsAsFactors = FALSE) %>%
    ict_skills_clean()

  chores_data <-  read.csv("https://drive.google.com/uc?export=download&id=1tTQB6CtgGkBwj4SBsjLdfR7l_4mVlFfs", stringsAsFactors = FALSE) %>%
    chores_clean()

  #bind all

  other_data <- dplyr::bind_rows(wb_data, eurostat_data, oecd_data, un_aids_data, gcpea_data, unicef_wash_data,
                                 unicef_ecce_learn_data, unicef_ecce_books_data, bullying_data, ict_skills_data, chores_data)

}






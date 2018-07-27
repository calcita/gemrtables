inds <- function() {
  read.csv("https://drive.google.com/uc?export=download&id=1_VGBQ3x4DDrcmEO_5192oPP4xjOuz12I", stringsAsFactors = FALSE) %>%
    mutate(var_concat = ifelse(var_concat == "", NA, var_concat)) %>%
    as_tibble()
}


region_groups <- function() { read.csv("https://drive.google.com/uc?export=download&id=13-dMaPNS6-DwzMTxhIR4OKP2zrm5s_dx", stringsAsFactors = FALSE)
}

weights <- function() {

  list("https://api.uis.unesco.org/sdmx/data/UNESCO,DEM_ECO,1.0/POP.PER._T.Y15T24+Y_GE65+_T.?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,DEM_ECO,1.0/ILO_POP.PER._T.Y_GE15.?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER._T._T._T.Y3T7..INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L02._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L3._T..SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2_3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1T3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L4._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L5T8._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2+L1+L3._T..TH_ENTRY_GLAST._T..............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L1+L2+L2_3+L5T8._T._T._T._T.INST_T...._T.........?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L1.._T..GLAST.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ILLPOP.PER..._T.Y_GE15+Y15T24...............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TEACH.PER.L1+L02+L2_3+L5T8._T._T._T..INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER._T.._T.Y12T14...............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
       "http://data.un.org/ws/rest/data/DF_UNDATA_WPP/SP_POP_TOTL.A.Y_LT5+Y5T10+Y10T14._T.....?startPeriod=2015&endPeriod=2015") %>%
    read_urls(key = "43ef903b32e74ebabe916007e9cf89e9") %>%
    weights_clean()

}

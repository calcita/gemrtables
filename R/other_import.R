wb_data <-  wb(country = "countries_only", indicator = c("SH.STA.STNT.ZS",
                                                         "LO.TIMSS.SCI8.LOW", "LO.PISA.SCI.0.MA", "LO.PISA.SCI.1B.MA", "LO.PISA.SCI.1A.MA",
                                                         "LO.PIAAC.LIT.YOU.BE", "LO.PIAAC.LIT.YOU.1", "LO.PIAAC.LIT.BE", "LO.PIAAC.LIT.1",
                                                         "LO.PIAAC.NUM.YOU.BE", "LO.PIAAC.NUM.YOU.1", "LO.PIAAC.NUM.BE", "LO.PIAAC.NUM.1",
                                                         "LO.PIAAC.LIT.FE.BE", "LO.PIAAC.LIT.FE.1", "LO.PIAAC.LIT.MA.BE", "LO.PIAAC.LIT.MA.1",
                                                         "LO.PIAAC.NUM.FE.BE", "LO.PIAAC.NUM.FE.1", "LO.PIAAC.NUM.MA.BE", "LO.PIAAC.NUM.MA.1")) %>%
            wb_clean()

eurostat_data <- list("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/trng_aes_100/.T.FE_NFE.PC../?startperiod=2010&endPeriod=2050",
                    "ec.europa.eu/eurostat/SDMX/diss-web/rest/data/isoc_sk_cskl_i/A.I_CCPY.IND_TOTAL.PC_IND./?startperiod=2010&endPeriod=2050") %>%
                   read_urls() %>%
                   eurostat_clean()

oecd_data <- list("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CRS1/20005..1000.100.100.D.112.E+E01/all?startTime=2016&endTime=2050",
                   "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_TS_ACT/..L0+L1+L2_C4+L3_C4.Y25T64.T.RATIO_ACTL_TER/all?",
                  "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EDU_PERS_INST/.T.INST_T.T.L2+L3.T.TEACH.PER/all?startTime=2010&endTime=2050") %>%
               read_urls(bind = FALSE) %>%
               oecd_clean()

un_aids_data <- read.csv("https://drive.google.com/uc?export=download&id=1S6E4WwosJHjpu-d557tUjlJ6awMAwLsa", stringsAsFactors = FALSE) %>%
  un_aids_clean()


gcpea_data <- read.csv("https://drive.google.com/uc?export=download&id=1tlya_2MkfFuAZC_fXw8Q3BZqZ3Qq08Q1", stringsAsFactors = FALSE) %>%
  gcpea_clean()

unicef_wash_data <- read.csv("https://drive.google.com/uc?export=download&id=1I4XbP4fCzlkE-e-98BVgh6UPjM0oEHDI", stringsAsFactors = FALSE) %>%
  unicef_clean()


weights_data <- list("https://api.uis.unesco.org/sdmx/data/UNESCO,DEM_ECO,1.0/POP.PER._T.Y15T24+Y_GE65+_T.?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,DEM_ECO,1.0/ILO_POP.PER._T.Y_GE15.?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER._T._T._T.Y3T7..INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L02._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2_3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L1T3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L4._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L5T8._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L2+L1+L3._T._T.TH_ENTRY_GLAST._T..............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L1+L2+L2_3+L5T8._T._T._T._T.INST_T...._T.........?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L1.._T..GLAST.INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ILLPOP.PER..._T.Y_GE15+Y15T24...............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TEACH.PER.L1+L02+L2_3+L5T8._T._T._T..INST_T.............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER._T.._T.Y12T14...............?format=sdmx-compact-2.1&startPeriod=2015&endPeriod=2015&subscription-key=",
                 "http://data.un.org/ws/rest/data/DF_UNDATA_WPP/SP_POP_TOTL.A.Y_LT5+Y5T10+Y10T14._T.....?startPeriod=2015&endPeriod=2015") %>%
  read_urls(key = "43ef903b32e74ebabe916007e9cf89e9") %>%
  pop_clean()



#' uis
#'
#' \code{uis} is a function to import and clean UIS data.
#'
#' Defines SDMX queries to the UIS API and applies the `uis_clean` function
#'@family import/clean
#'@seealso \code{\link{uis_clean}}

uis <- function() {

  uis_urls <- list(

    # UIS api REST API urls

    #table 1

    #vars 1:4
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/COMP_EDU+FREE_EDU.YR.L1T3+L02+L1+L2_3._T._T.._T..............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 5
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TH_ENTRY_AGE.YR.L1.._T.TH_ENTRY_AGE.G1..............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 6:8
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TH_DUR.YR.L02+L1+L2+L3...._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 9:12
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SAP.PER.L02+L1+L2_3+L5T8._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 13:16
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L02+L1+L2_3+L5T8._T._T._T._T.INST_T...._T.........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 17:18
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_FINANCE,1.0/EDU_EXP.GDP+GOV_EXP_T._T._T._T._T.GOV.....?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 19:26
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_FINANCE,1.0/XUNIT.PPP_CONST+GDP_CAP.L02+L1+L2_3+L5T8._T._T._T.GOV.FFNTR...?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 27
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_FINANCE,1.0/EDU_EXP.GDP._T._T.._T.HH.FFNTR...?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",

    #table 2

    #vars 1, 3, 5
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/OFST.PER+PT.L1+L2+L3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 2, 4, 6
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ROFST.PT.L1+L2+L3._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 7:9
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/CR.PT.L2+L1+L3._T._T._T.GLAST.INST_T.._T._T.._T........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 10:11
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PT.L2+L1.._T.GE2_OVER_AGE._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 12:13
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/GER+NERA.PT.L1._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 14, 17
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/AIR.PT.L2+L1.._T..GLAST..............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 15
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TRANRA.PT.L2.._T._T.G1.INST_T....INIT.........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    # vars 16, 18
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/NERT.PT.L2+L3._T._T.SCH_AGE_GROUP._T..............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 19:21
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ADMIN.NB.L2+L1._T._T..G2_3+GLAST.INST_T......MATH+READING.......?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 22:27
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PT.L2+L1....G2_3+GLAST.INST_T.._T._T...MATH+READING.._T.....?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",

    #table 3
    #var 1
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/CHILD_TRACK_T.PT._Z.._T.Y_LT5...............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 5
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/GER.PT.L02._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 6
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/NERA.PT.L1._T._T.UNDER1_AGE...............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",

    #table 4
    #var 1
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/PRYA.PT._T._T._T.Y_GE15...............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 2
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PT.L2_3.C5._T.Y15T24._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 3:4
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/GTVP.PT.L4+L2_3.C5._T._T._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 5
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/GTRANR.PT.L3._T._T._T.G1.INST_T....INIT.........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 6
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/GENTR.PT.L5T7._T._T._T._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 7
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/GER.PT.L5T8._T._T.SCH_AGE_GROUP._T.INST_T.............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 8:10
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ICT_SKILL_DUP+ICT_SKILL_FORM+ICT_SKILL_PROG.PT._Z._Z._T._T...............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 11:14
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/EA.PT._Z._Z._T.Y_GE25._T.INST_T.L1T8+L2T8+L3T8+L4T8............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 15:18
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/FUNCT_LIT+FUNCT_NUM.PT._Z._Z._T._T._Z._Z._Z._Z...........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 19:20
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/LR.PT._Z._Z._T.Y15T24+Y_GE15...............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 21, 23
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ILLPOP.PER._Z._Z._T+F.Y15T24+Y_GE15...............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 22, 24
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/ILLPOP.PT._Z._Z.F.Y_GE15+Y15T24.........._Z.....?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",

    #table 5
    #vars 1:3
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/CR.GPIA.L1+L2+L3._T._T._T.GLAST.INST_T._Z._T._T._T.........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 4:7 (GPIA to be calculated)
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PT.L2+L1._T.F+M._T.GLAST.INST_T.._T._T..ISC_F00.MATH+READING.._T.....?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 8:9  (GPIA to be calculated)
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/LR.PT._Z._Z.M+F.Y_GE15+Y15T24..._Z............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 12:15
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/FTP.PT.L02+L1+L2_3+L5T8._T.._T..INST_T._T............?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 20:21, 24:25, 28:29
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/CR.GLPIA+WPIA.L1+L2+L3._T._T._T.GLAST.INST_T._Z._T._T._T.........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 22:23, 26:27, 30:31
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/CR.PT.._T.F+M._T.GLAST.INST_T..Q1._T.._T........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 32:33 (WPIA to be calculated)
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PT.L1+L2._T._T..GLAST...Q1+Q5._T...MATH+READING.......?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",

    #table 6
    #vars 5,
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SCH.PT.L2._T.._T._T.INST_T.._Z.....HIV_SEX_EDU......?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #11:14
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/SCH.PT.L1._T.._T._T.INST_T.._Z.....ADAPT_INFR_MAT_DIS+COMP_PP+NET_PP+ELEC......?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 18, 21
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/MOR+MSEP.PT.L5T8._T._T...INST_T...........W00.W00.?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #var 19 (unavailable in UIS API) - total tertiary enrolment needed to calculate
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/STU.PER.L5T8._T._T._T._T.INST_T...._T.........?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    # "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/MENF.PER.L5T8._T._T.._T.INST_T............W00.?format=sdmx-compact-2.1&startPeriod=2010&endPeriod=2020&subscription-key=",
    #var 20
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/OE.PER.L5T8._T._T.._T.INST_T............W00.?format=sdmx-compact-2.1&startPeriod=2010&endPeriod=2020&subscription-key=",

    #table 7
    #vars 1:2, 8:9, 15:16
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/TEACH+PTR.PER.L02+L1+L2_3._T._T._T._T.INST_T......_T..._T....?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=",
    #vars 3:6, 10:13, 17:20
    "https://api.uis.unesco.org/sdmx/data/UNESCO,EDU_NON_FINANCE,2.0/QUTP+TRTP+TISP+TATTRR.PT.L02+L1+L2_3._T._T._T..INST_T......_T..._T._T...?format=sdmx-compact-2.1&lastNObservations=1&subscription-key=")


  uis_data <- read_urls(uis_urls, key = .gemrtables.pkg.env$key) %>%
    uis_clean()
}









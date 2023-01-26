# Attach DREAMS identity field to shapefiles ----

attachDREAMSField <- function(x, y) {
  a <- x
  
  a$DREAMSDistrict <- "No"
  
  if (y == "Botswana") {
    internal_list <- DREAMS_Districts_Botswana_USCB
  } else if  (y == "CDI") {
    internal_list <- DREAMS_Districts_CotedIvoire
  } else if  (y == "Eswatini") {
    internal_list <- DREAMS_Districts_Eswatini
  } else if  (y == "Haiti") {
    internal_list <- DREAMS_Districts_Haiti
  } else if (y == "Kenya") {
    internal_list <- DREAMS_Districts_Kenya_USCB
  } else if  (y == "Lesotho") {
    internal_list <- DREAMS_Districts_Lesotho_USCB
  } else if  (y == "Malawi") {
    internal_list <- DREAMS_Districts_Malawi_USCB
  } else if  (y == "Mozambique") {
    internal_list <- DREAMS_Districts_Mozambique_USCB
  } else if  (y == "Namibia") {
    internal_list <- DREAMS_Districts_Namibia_USCB
  } else if  (y == "Rwanda") {
    internal_list <- DREAMS_Districts_Rwanda_USCB
  } else if  (y == "South Africa") {
    internal_list <- DREAMS_Districts_SouthAfrica_USCB
  } else if  (y == "Tanzania") {
    internal_list <- DREAMS_Districts_Tanzania
  } else if  (y == "Uganda") {
    internal_list <- DREAMS_Districts_Uganda_USCB
  } else if  (y == "Zambia") {
    internal_list <- DREAMS_Districts_Zambia_USCB
  } else if (y == "Zimbabwe") {
    internal_list <- DREAMS_Districts_Zimbabwe_USCB
  }
  
  internal_ADM1_Countries <- c("Kenya",
                              "Lesotho",
                              "Rwanda",
                              "South Africa", #actually a PSNU country, but here for functionality (workaround), improve later
                              "Uganda")
  
  internal_ADM2_Countries <- c("Botswana",
                            "Malawi",
                            "Mozambique",
                            "Namibia",
                            "Zambia",
                            "Zimbabwe")
  
  internal_NSO_NAME_Countries <- c("Tanzania")
  
  internal_WorldPop_Countries <- c("CDI",
                                   "Eswatini",
                                   "Haiti")
  
  if(y %in% internal_ADM1_Countries) {   #SNUs
    b <- a %>%
      mutate(
        DREAMSDistrict = case_when(
          (ADM1_NAME %in% internal_list) ~ as.character("Yes"),
          TRUE ~ as.character("No")
        )
      )
  } else if (y %in% internal_ADM2_Countries) { #PSNUs
    b <- a %>%
      mutate(
        DREAMSDistrict = case_when(
          (ADM2_NAME %in% internal_list) ~ as.character("Yes"),
          TRUE ~ as.character("No")
        )
      )
  } else if (y %in% internal_NSO_NAME_Countries) { #NSO_NAME Matching
    b <- a %>%
      mutate(
        DREAMSDistrict = case_when(
          (NSO_NAME %in% internal_list) ~ as.character("Yes"),
          TRUE ~ as.character("No")
        )
      )
  } else if (y %in% internal_WorldPop_Countries) { #NSO_NAME Matching
    b <- a %>%
      mutate(
        DREAMSDistrict = case_when(
          (AREA_NAME %in% internal_list) ~ as.character("Yes"),
          TRUE ~ as.character("No"))) %>%
      mutate(
        JOIN_NAME = case_when(
          (AREA_NAME == "X") ~ as.character("ERROR"),
          TRUE ~ as.character(AREA_NAME))
      )
  }
    
}


dataParametersImportandMutate <- function(x) {
  
  readxl::read_xlsx(x) %>%
    dplyr::mutate(
      ageasentered = case_when(
        (AgeCohort == "10 to 14") ~ as.character("10-14"),
        (AgeCohort == "15 to 19") ~ as.character("15-19"),
        (AgeCohort == "20 to 24") ~ as.character("20-24"),
        (AgeCohort == "25 to 29") ~ as.character("25-29")
      )
    ) %>%
    dplyr::select(-"AgeCohort")
}

##############################

# Integrate custom data table for population structure ----

dataParametersPivot1Year <- function(x) {
  
  a <- x %>%
    pivot_longer(
      cols = GradStructure_2018:GradStructure_2022,
      names_to = "fiscalyear",
      names_prefix = "GradStructure_",
      values_to = "GradStructure"
    )
  
  firstQ <- c(10, 15, 20, 25)
  secondQ <- c(11, 16, 21, 26)
  thirdQ <- c(12, 17, 22, 27)
  fourthQ <- c(13, 18, 23, 28)
  fifthQ <- c(14, 19, 24, 29)
  
  a$GradStructure <- a$GradStructure/100
  
  b <- a %>%
    mutate(
      Age = case_when(
        (Age %in% firstQ) ~ as.character("firstQ"),
        (Age %in% secondQ) ~ as.character("secondQ"),
        (Age %in% thirdQ) ~ as.character("thirdQ"),
        (Age %in% fourthQ) ~ as.character("fourthQ"),
        (Age %in% fifthQ) ~ as.character("fifthQ")
      )) %>%
    pivot_wider(
      names_from = c("Age", "fiscalyear"),
      values_from = "GradStructure"

    )
  
  return(b)
  
}

##############################

dataParametersPivot5Year <- function(x) {
  
  Prevalence <- x %>%
    dplyr::select(-(10:30)) %>%
    pivot_longer(
      cols = Prevalence_2018:Prevalence_2023,
      names_to = "fiscal_year",
      names_prefix = "Prevalence_",
      values_to = "Prevalence"
    )
  
  Vulnerable <- x %>%
    dplyr::select(-(4:9)) %>%
    dplyr::select(-(10:24)) %>%
    pivot_longer(
      cols = Vulnerable_2018:Vulnerable_2023,
      names_to = "fiscal_year",
      names_prefix = "Vulnerable_",
      values_to = "Vulnerable"
    )
  
  Mobility <- x %>%
    dplyr::select(-(4:15)) %>%
    dplyr::select(-(9:18)) %>%
    pivot_longer(
      cols = Mobility_2018:Mobility_2022,
      names_to = "fiscal_year",
      names_prefix = "Mobility_",
      values_to = "Mobility"
    )
  
  Enrollment <- x %>%
    dplyr::select(-(4:20)) %>%
    dplyr::select(-(9:13)) %>%
    pivot_longer(
      cols = Enrollment_2018:Enrollment_2022,
      names_to = "fiscal_year",
      names_prefix = "Enrollment_",
      values_to = "Enrollment"
    )

  PrimarySecondaryDoubleCounts <- x %>%
    dplyr::select(-(4:25)) %>%
    pivot_longer(
      cols = PrimarySecondaryDoubleCounts_2018:PrimarySecondaryDoubleCounts_2022,
      names_to = "fiscal_year",
      names_prefix = "PrimarySecondaryDoubleCounts_",
      values_to = "PrimarySecondaryDoubleCounts"
    )

  PrevVuln <- left_join(Prevalence,
                        Vulnerable,
                        by = c("Country" = "Country", "District" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered"))
  
  PrevVulnMob <- left_join(PrevVuln,
                            Mobility,
                            by = c("Country" = "Country", "District" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered"))
  
  PrevVulnMobEnro <- left_join(PrevVulnMob,
                        Enrollment,
                        by = c("Country" = "Country", "District" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered"))
  
  All <- left_join(PrevVulnMobEnro,
                   PrimarySecondaryDoubleCounts,
                   by = c("Country" = "Country", "District" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered")) %>%
    dplyr::select(-c("Join_Name.x",
                      "Join_Name.y",
                      "Join_Name.x.x",
                      "Join_Name.y.y"))
   
  All$fiscal_year <- All$fiscal_year %>%
    as.numeric()
  
  return(All)

}

#Reshape to match PoC

splitForReshapeWide <- function(x, y) {
  
  x %>%
    dplyr::select(c("AREA_NAME",
                    "JOIN_NAME",
                    "NSO_NAME",
                    "uid",
                    "ageasentered",
                    "country",
                    "fiscal_year",
                    all_of(y)))
             
}

reshapeWide <- function(x) {

  PrevalenceDF <- splitForReshapeWide(x, "Prevalence")
  VulnerableDF <- splitForReshapeWide(x, "Vulnerable")
  MobilityDF <- splitForReshapeWide(x, "Mobility")
  PrimarySecondaryDoubleCountsDF <- splitForReshapeWide(x, "PrimarySecondaryDoubleCounts")
  EnrollmentDF <- splitForReshapeWide(x, "Enrollment")
  PopDF <- splitForReshapeWide(x, "population")
  AGYW_PREVDF <- splitForReshapeWide(x, "AGYW_PREV")

  PrevalenceDF <- PrevalenceDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "Prev_{fiscal_year}",
      values_from = Prevalence
    )
  
  VulnerableDF <- VulnerableDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "Vuln_{fiscal_year}",
      values_from = Vulnerable
    )

  PrimarySecondaryDoubleCountsDF <- PrimarySecondaryDoubleCountsDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "PSDC_{fiscal_year}",
      values_from = PrimarySecondaryDoubleCounts
    )
  
  MobilityDF <- MobilityDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "Mobility_{fiscal_year}",
      values_from = Mobility
    )
  
  EnrollmentDF <- EnrollmentDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "Enrollment_{fiscal_year}",
      values_from = Enrollment
    )

  PopDF <- PopDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "Pop_{fiscal_year}",
      values_from = population
    )

  AGYW_PREVDF <- AGYW_PREVDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  JOIN_NAME,
                  NSO_NAME,
                  uid,
                  ageasentered,
                  country),
      names_from = fiscal_year,
      names_glue = "AGYW_PREV_{fiscal_year}",
      values_from = AGYW_PREV
    )
  
  PrevVuln <- left_join(PrevalenceDF,
                        VulnerableDF,
                   by = c("country" = "country", 
                          "AREA_NAME" = "AREA_NAME",
                          "JOIN_NAME" = "JOIN_NAME",
                          "NSO_NAME" = "NSO_NAME",
                          "uid" = "uid",
                          "ageasentered" = "ageasentered"))
  
  PrevVulnPSDC <- left_join(PrevVuln,
                        PrimarySecondaryDoubleCountsDF,
                        by = c("country" = "country", 
                               "AREA_NAME" = "AREA_NAME",
                               "JOIN_NAME" = "JOIN_NAME",
                               "NSO_NAME" = "NSO_NAME",
                               "uid" = "uid",
                               "ageasentered" = "ageasentered"))
  
  PrevVulnPSDCMob <- left_join(PrevVulnPSDC,
                                      MobilityDF,
                                      by = c("country" = "country", 
                                             "AREA_NAME" = "AREA_NAME",
                                             "JOIN_NAME" = "JOIN_NAME",
                                             "NSO_NAME" = "NSO_NAME",
                                             "uid" = "uid",
                                             "ageasentered" = "ageasentered"))
  
  PrevVulnPSDCMobEnrollment <- left_join(PrevVulnPSDCMob,
                               EnrollmentDF,
                               by = c("country" = "country", 
                                      "AREA_NAME" = "AREA_NAME",
                                      "JOIN_NAME" = "JOIN_NAME",
                                      "NSO_NAME" = "NSO_NAME",
                                      "uid" = "uid",
                                      "ageasentered" = "ageasentered"))
  
  PrevVulnPSDCMobEnrollmentPop <- left_join(PrevVulnPSDCMobEnrollment,
                            PopDF,
                            by = c("country" = "country", 
                                   "AREA_NAME" = "AREA_NAME",
                                   "JOIN_NAME" = "JOIN_NAME",
                                   "NSO_NAME" = "NSO_NAME",
                                   "uid" = "uid",
                                   "ageasentered" = "ageasentered"))
  
  All <- left_join(PrevVulnPSDCMobEnrollmentPop,
                   AGYW_PREVDF,
                   by = c("country" = "country", 
                          "AREA_NAME" = "AREA_NAME",
                          "JOIN_NAME" = "JOIN_NAME",
                          "NSO_NAME" = "NSO_NAME",
                          "uid" = "uid", 
                          "ageasentered" = "ageasentered"))
  
  return(All)
}


# Derive statistics

deriveStatisticsPreSat <- function(x) {
  a <- x 
  
  a$PLHIV_2018 <- round((a$Pop_2018 * ((a$Prev_2018)/100)),
                        0)
  
  a$PLHIV_2019 <- round((a$Pop_2019 * ((a$Prev_2019)/100)), 
                        0)
  
  a$PLHIV_2020 <- round((a$Pop_2020 * ((a$Prev_2020)/100)), 
                        0)
  
  a$PLHIV_2021 <- round((a$Pop_2021 * ((a$Prev_2021)/100)), 
                        0)
  
  a$PLHIV_2022 <- round((a$Pop_2022 * ((a$Prev_2022)/100)), 
                        0)
  
  a$PLHIV_2023 <- round((a$Pop_2023 * ((a$Prev_2023)/100)), 
                        0)
  
  a$NonPLHIV_2018 <- round((a$Pop_2018 - a$PLHIV_2018),
                           0)
  
  a$NonPLHIV_2019 <- round((a$Pop_2019 - a$PLHIV_2019),
                           0)
  
  a$NonPLHIV_2020 <- round((a$Pop_2020 - a$PLHIV_2020),
                           0)
  
  a$NonPLHIV_2021 <- round((a$Pop_2021 - a$PLHIV_2021),
                           0)
  
  a$NonPLHIV_2022 <- round((a$Pop_2022 - a$PLHIV_2022),
                           0)
  
  a$NonPLHIV_2023 <- round((a$Pop_2023 - a$PLHIV_2023),
                           0)
  
  a$VulnerableNonPLHIV_2018 <- round(a$NonPLHIV_2018 * (a$Vuln_2018/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2019 <- round(a$NonPLHIV_2019 * (a$Vuln_2019/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2020 <- round(a$NonPLHIV_2020 * (a$Vuln_2020/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2021 <- round(a$NonPLHIV_2021 * (a$Vuln_2021/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2022 <- round(a$NonPLHIV_2022 * (a$Vuln_2022/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2023 <- round(a$NonPLHIV_2023 * (a$Vuln_2023/100), 
                                     0)
  
  a <- replaceAGYWNAs(a) 
  
  
  a$DeDuplicatedAGYW_PREV_2018 <- round(a$AGYW_PREV_2018 * ((100-a$PSDC_2018)/100),
                                        0)
  
  a$DeDuplicatedAGYW_PREV_2019 <- round(a$AGYW_PREV_2019 * ((100-a$PSDC_2019)/100),
                                       0)

  a$DeDuplicatedAGYW_PREV_2020 <- round(a$AGYW_PREV_2020 * ((100-a$PSDC_2020)/100),
                                       0)

  a$DeDuplicatedAGYW_PREV_2021 <- round(a$AGYW_PREV_2021 * ((100-a$PSDC_2021)/100),
                                       0)

  a$DeDuplicatedAGYW_PREV_2022 <- round(a$AGYW_PREV_2022 * ((100-a$PSDC_2022)/100),
                                       0)
  
  a$MobilityStandardizedAGYW_PREV_2018 <- round(a$DeDuplicatedAGYW_PREV_2018 * ((100-a$Mobility_2018)/100), 
                                                  0)
  
  a$MobilityStandardizedAGYW_PREV_2019 <- round(a$DeDuplicatedAGYW_PREV_2019 * ((100-a$Mobility_2019)/100), 
                                                  0)
  
  a$MobilityStandardizedAGYW_PREV_2020 <- round(a$DeDuplicatedAGYW_PREV_2020 * ((100-a$Mobility_2020)/100), 
                                                  0)
  
  a$MobilityStandardizedAGYW_PREV_2021 <- round(a$DeDuplicatedAGYW_PREV_2021 * ((100-a$Mobility_2021)/100), 
                                                  0)
  
  a$MobilityStandardizedAGYW_PREV_2022 <- round(a$DeDuplicatedAGYW_PREV_2022 * ((100-a$Mobility_2022)/100), 
                                                  0)
  
  a$EnrollmentStandardizedAGYW_PREV_2018 <- round(a$MobilityStandardizedAGYW_PREV_2018 * ((100-a$Enrollment_2018)/100), 
                                                  0)

  a$EnrollmentStandardizedAGYW_PREV_2019 <- round(a$MobilityStandardizedAGYW_PREV_2019 * ((100-a$Enrollment_2019)/100), 
                                                                            0)
  
  a$EnrollmentStandardizedAGYW_PREV_2020 <- round(a$MobilityStandardizedAGYW_PREV_2020 * ((100-a$Enrollment_2020)/100), 
                                                  0)
  
  a$EnrollmentStandardizedAGYW_PREV_2021 <- round(a$MobilityStandardizedAGYW_PREV_2021 * ((100-a$Enrollment_2021)/100), 
                                                  0)
  
  a$EnrollmentStandardizedAGYW_PREV_2022 <- round(a$MobilityStandardizedAGYW_PREV_2022 * ((100-a$Enrollment_2022)/100), 
                                                  0)
  
  return(a)
}

deriveStatisticsSat <- function(x) {
  
  a <- x
  b <- x
  c <- x
  
  a$PopStructure <- "Custom"
  
  b$PopStructure <- "Default"
  
  c$PopStructure <- "National"
  
  a$Actual_Served_2018 <- round(a$EnrollmentStandardizedAGYW_PREV_2018, #Make work if 2018 AGYW_PREV ingestion becomes an option, incorporate into successive calculations
                                0)
  
  a$Actual_Served_2019 <- round(a$EnrollmentStandardizedAGYW_PREV_2019,
                                   0)
  
  a$Actual_Served_2020 <- round((a$EnrollmentStandardizedAGYW_PREV_2020 + (a$EnrollmentStandardizedAGYW_PREV_2019*(1-a$fifthQ_2019))),
                                   0)
  
  a$Actual_Served_2021 <- round((a$EnrollmentStandardizedAGYW_PREV_2021 + (a$EnrollmentStandardizedAGYW_PREV_2020*(1-a$fifthQ_2020)) + (a$EnrollmentStandardizedAGYW_PREV_2019*(1-(a$fifthQ_2019+a$fourthQ_2019)))),
                                   0)
  
  a$Actual_Served_2022 <- round((a$EnrollmentStandardizedAGYW_PREV_2022 + (a$EnrollmentStandardizedAGYW_PREV_2021*(1-a$fifthQ_2021)) + (a$EnrollmentStandardizedAGYW_PREV_2020*(1-(a$fifthQ_2020+a$fourthQ_2020))) + (a$EnrollmentStandardizedAGYW_PREV_2019*(1-(a$fifthQ_2019+a$fourthQ_2019+a$thirdQ_2019)))),
                                   0)
  
  a$Actual_Served_2023 <- round(((a$EnrollmentStandardizedAGYW_PREV_2022*(1-a$fifthQ_2022)) + (a$EnrollmentStandardizedAGYW_PREV_2021*(1-a$fifthQ_2021)) + (a$EnrollmentStandardizedAGYW_PREV_2020*(1-(a$fifthQ_2020+a$fourthQ_2020+a$thirdQ_2020))) + (a$EnrollmentStandardizedAGYW_PREV_2019*(1-(a$fifthQ_2019+a$fourthQ_2019+a$thirdQ_2019+a$secondQ_2019)))),
                                0)
  
  a$Sat_2018 <- round(((a$Actual_Served_2018/a$VulnerableNonPLHIV_2018)*100),
                      1)
  
  a$Sat_2019 <- round(((a$Actual_Served_2019/a$VulnerableNonPLHIV_2019)*100),
                      1)
  
  a$Sat_2020 <- round(((a$Actual_Served_2020/a$VulnerableNonPLHIV_2020)*100),
                      1)
  
  a$Sat_2021 <- round(((a$Actual_Served_2021/a$VulnerableNonPLHIV_2021)*100),
                      1)
  
  a$Sat_2022 <- round(((a$Actual_Served_2022/a$VulnerableNonPLHIV_2022)*100),
                      1)
  
  a$Sat_2023 <- round(((a$Actual_Served_2023/a$VulnerableNonPLHIV_2023)*100),
                      1)

  b$Actual_Served_2018 <- round(b$EnrollmentStandardizedAGYW_PREV_2018, #Make work if 2018 AGYW_PREV ingestion becomes an option, incorporate into successive calculations
                                0)
  
  b$Actual_Served_2019 <- round(b$EnrollmentStandardizedAGYW_PREV_2019,
                      0)
  
  b$Actual_Served_2020 <- round((b$EnrollmentStandardizedAGYW_PREV_2020+(b$EnrollmentStandardizedAGYW_PREV_2019*.8)),
                      0)
  
  b$Actual_Served_2021 <- round((b$EnrollmentStandardizedAGYW_PREV_2021 + (b$EnrollmentStandardizedAGYW_PREV_2020*.8) + (b$EnrollmentStandardizedAGYW_PREV_2019*.6)),
                      0)
  
  b$Actual_Served_2022 <- round((b$EnrollmentStandardizedAGYW_PREV_2022 + (b$EnrollmentStandardizedAGYW_PREV_2021*.8) + (b$EnrollmentStandardizedAGYW_PREV_2020*.6) + (b$EnrollmentStandardizedAGYW_PREV_2019*.4)),
                      0)
  
  b$Actual_Served_2023 <- round(((b$EnrollmentStandardizedAGYW_PREV_2022*.8) + (b$EnrollmentStandardizedAGYW_PREV_2021*.6) + (b$EnrollmentStandardizedAGYW_PREV_2020*.4) + (b$EnrollmentStandardizedAGYW_PREV_2019*.2)),
                                0)
  
  b$Sat_2018 <- round(((b$Actual_Served_2018/b$VulnerableNonPLHIV_2018)*100),
                      1)
  
  b$Sat_2019 <- round(((b$Actual_Served_2019/b$VulnerableNonPLHIV_2019)*100),
                      1)
  
  b$Sat_2020 <- round(((b$Actual_Served_2020/b$VulnerableNonPLHIV_2020)*100),
                      1)
  
  b$Sat_2021 <- round(((b$Actual_Served_2021/b$VulnerableNonPLHIV_2021)*100),
                      1)
  
  b$Sat_2022 <- round(((b$Actual_Served_2022/b$VulnerableNonPLHIV_2022)*100),
                      1)
  
  b$Sat_2023 <- round(((b$Actual_Served_2023/b$VulnerableNonPLHIV_2023)*100),
                      1)

  c$Actual_Served_2018 <- round(c$EnrollmentStandardizedAGYW_PREV_2018, #Make work if 2018 AGYW_PREV ingestion becomes an option, incorporate into successive calculations
                                 0)
  
  c$Actual_Served_2019 <- round(c$EnrollmentStandardizedAGYW_PREV_2019,
                                0)
  
  c$Actual_Served_2020 <- round((c$EnrollmentStandardizedAGYW_PREV_2020 + (c$EnrollmentStandardizedAGYW_PREV_2019*(1-c$fifthQNat_2019))),
                                0)
  
  c$Actual_Served_2021 <- round((c$EnrollmentStandardizedAGYW_PREV_2021 + (c$EnrollmentStandardizedAGYW_PREV_2020*(1-c$fifthQNat_2020)) + (c$EnrollmentStandardizedAGYW_PREV_2019*(1-(c$fifthQNat_2019+c$fourthQNat_2019)))),
                                0)
  
  c$Actual_Served_2022 <- round((c$EnrollmentStandardizedAGYW_PREV_2022 + (c$EnrollmentStandardizedAGYW_PREV_2021*(1-c$fifthQNat_2021)) + (c$EnrollmentStandardizedAGYW_PREV_2020*(1-(c$fifthQNat_2020+c$fourthQNat_2020))) + (c$EnrollmentStandardizedAGYW_PREV_2019*(1-(c$fifthQNat_2019+c$fourthQNat_2019+c$thirdQNat_2019)))),
                                0)
  
  c$Actual_Served_2023 <- round(((c$EnrollmentStandardizedAGYW_PREV_2022*(1-c$fifthQNat_2022)) + (c$EnrollmentStandardizedAGYW_PREV_2021*(1-(c$fifthQNat_2021+c$fourthQNat_2021))) + (c$EnrollmentStandardizedAGYW_PREV_2020*(1-(c$fifthQNat_2020+c$fourthQNat_2020+c$thirdQNat_2020))) + (c$EnrollmentStandardizedAGYW_PREV_2019*(1-(c$fifthQNat_2019+c$fourthQNat_2019+c$thirdQNat_2019+c$secondQNat_2019)))),
                                0)
  
  c$Sat_2018 <- round(((c$Actual_Served_2018/c$VulnerableNonPLHIV_2018)*100),
                      1)
  
  c$Sat_2019 <- round(((c$Actual_Served_2019/c$VulnerableNonPLHIV_2019)*100),
                      1)
  
  c$Sat_2020 <- round(((c$Actual_Served_2020/c$VulnerableNonPLHIV_2020)*100),
                      1)
  
  c$Sat_2021 <- round(((c$Actual_Served_2021/c$VulnerableNonPLHIV_2021)*100),
                      1)
  
  c$Sat_2022 <- round(((c$Actual_Served_2022/c$VulnerableNonPLHIV_2022)*100),
                      1)
  
  c$Sat_2023 <- round(((c$Actual_Served_2023/c$VulnerableNonPLHIV_2023)*100),
                      1)
  
  d <- rbind(a,
             b)
  
  e <- rbind(d,
             c)
  
  
  e$IsSelected <- "Unselected"
  
  return(e)
  
}

deriveStatisticsGirlsRemaining <- function(x) {
  
  a <- x
  
  a$PopRemaining_2018 <- a$VulnerableNonPLHIV_2018 - a$Actual_Served_2018
  
  a$PopRemaining_2019 <- a$VulnerableNonPLHIV_2019 - a$Actual_Served_2019
  
  a$PopRemaining_2020 <- a$VulnerableNonPLHIV_2020 - a$Actual_Served_2020
  
  a$PopRemaining_2021 <- a$VulnerableNonPLHIV_2021 - a$Actual_Served_2021
  
  a$PopRemaining_2022 <- a$VulnerableNonPLHIV_2022 - a$Actual_Served_2022
  
  a$PopRemaining_2023 <- a$VulnerableNonPLHIV_2023 - a$Actual_Served_2023
  
  return(a)
  
}

deriveStatistics <- function(x) {
  a <- x %>%
    deriveStatisticsPreSat() %>%
    deriveStatisticsSat() %>%
    deriveStatisticsGirlsRemaining()

  return(a)
}

replaceAGYWNAs <- function(x) {
  
  a <- x %>%
    mutate(
      AGYW_PREV_2018 = case_when(
        (is.na(AGYW_PREV_2018)) ~ 0,
        TRUE ~ as.numeric(AGYW_PREV_2018)
      )
    ) %>%
    mutate(
      AGYW_PREV_2019 = case_when(
        (is.na(AGYW_PREV_2019)) ~ 0,
        TRUE ~ as.numeric(AGYW_PREV_2019)
      )
    ) %>%
    mutate(
      AGYW_PREV_2020 = case_when(
        (is.na(AGYW_PREV_2020)) ~ 0,
        TRUE ~ as.numeric(AGYW_PREV_2020)
      )
    ) %>%
    mutate(
      AGYW_PREV_2021 = case_when(
        (is.na(AGYW_PREV_2021)) ~ 0,
        TRUE ~ as.numeric(AGYW_PREV_2021)
      )
    ) %>%
    mutate(
      AGYW_PREV_2022 = case_when(
        (is.na(AGYW_PREV_2022)) ~ 0,
        TRUE ~ as.numeric(AGYW_PREV_2022)
      )
    )
  
  return(a)
  
}

mergeAccountingForCDI <- function(x, y) {
  a <- y %>%
    mutate(
      country = case_when(
        (country == "Côte d'Ivoire") ~ as.character("Cote d'Ivoire"),
        TRUE ~ as.character(country)
      )
    )
  
  b <- merge(x,
             a)
  
  return(b)
  
}

attachParameters_5year <- function(x, y, z) {

  a <- left_join(x,
            y,
            by = c("country" = "Country", 
                   "AREA_NAME" = "District", 
                   "fiscal_year" = "fiscal_year", 
                   "ageasentered" = "ageasentered"))

  return(a)
}
  
attachParameters_1year <- function(x, y) {
  
  a <- y %>%
    dplyr::select(-c("Join_Name"))
  
  b <- left_join(x,
                 a,
                 by = c("country" = "Country", 
                        "AREA_NAME" = "District", 
                        "ageasentered" = "ageasentered"))
  
  
  return(b)
}

reduceTo2022Export <- function(x) {
  
  col_order <- c("Country",
                 "District",
                 "Cohort",
                 "Pop_2022",
                 "Total DREAMS eligible AGYW",
                 "Percent coverage (saturation)",
                 "Remaining unserved AGYW"
  )
  
  a <- x %>%
    dplyr::filter(IsSelected == "Selected") %>%
    dplyr::select(-c("IsSelected",
                     "PopStructure")) %>%
    dplyr::select((-ends_with(c("2018",
                                "2019", 
                                "2020", 
                                "2021")))&(-starts_with(c("first", 
                                                          "second", 
                                                          "third", 
                                                          "fourth", 
                                                          "fifth")))) %>%
    dplyr::select(-c("Prev_2022", 
                     "PSDC_2022", 
                     "Vuln_2022",
                     "Pop_2022_Default",
                     "AGYW_PREV_2022",
                     "PLHIV_2022",
                     "NonPLHIV_2022",
                     "DeDuplicatedAGYW_PREV_2022",
                     "MobilityStandardizedAGYW_PREV_2022",
                     "EnrollmentStandardizedAGYW_PREV_2022",
                     "Actual_Served_2022",
                     "Prev_2023",
                     "Vuln_2023")) %>%
    dplyr::mutate(
      PopRemaining_2022 = case_when(
        (PopRemaining_2022 < 0) ~ 0,
        TRUE ~ as.numeric(PopRemaining_2022)
      )
    ) %>%
    dplyr::rename("Country" = "country",
                  "Cohort" = "ageasentered",
                  "District" = "AREA_NAME",
                  "Total DREAMS eligible AGYW" = "VulnerableNonPLHIV_2022",
                  "Percent coverage (saturation)" = "Sat_2022",
                  "Remaining unserved AGYW" = "PopRemaining_2022")
  
  a <- a[, col_order]
  
  return(a)
}


reduceToCOPExport <- function(x) {
  
  col_order <- c("Country",
                 "District",
                 "Cohort",
                 "Pop_2023",
                 "Total DREAMS eligible AGYW 2023",
                 "Percent coverage (saturation) 2023",
                 "Remaining unserved AGYW 2023"
                 )
  
  a <- x %>%
    dplyr::filter(IsSelected == "Selected") %>%
    dplyr::select(-c("IsSelected",
                     "PopStructure")) %>%
    dplyr::select((-ends_with(c("2018",
                                "2019", 
                                "2020", 
                                "2021",
                                "2022")))&(-starts_with(c("first", 
                                                          "second", 
                                                          "third", 
                                                          "fourth", 
                                                          "fifth")))) %>%
    dplyr::select(-c("Prev_2023", 
                     "Vuln_2023",
                     "Pop_2023_Default",
                     "PLHIV_2023",
                     "NonPLHIV_2023")) %>%
    dplyr::mutate(
      PopRemaining_2023 = case_when(
        (PopRemaining_2023 < 0) ~ 0,
        TRUE ~ as.numeric(PopRemaining_2023)
      )
    ) %>%
    dplyr::rename("Country" = "country",
           "Cohort" = "ageasentered",
           "District" = "AREA_NAME",
           "Pop_2023" = "Pop_2023",
           "Total DREAMS eligible AGYW 2023" = "VulnerableNonPLHIV_2023",
           "Percent coverage (saturation) 2023" = "Sat_2023",
           "Remaining unserved AGYW 2023" = "PopRemaining_2023")
  
  a <- a[, col_order]
    
  return(a)
}

reduceForAnalyticsPlots <- function(x) {

  a <- x

  a$AGYW_PREV_Sum <- a$AGYW_PREV_2018 + a$AGYW_PREV_2019 + a$AGYW_PREV_2020 + a$AGYW_PREV_2021 + a$AGYW_PREV_2022
  a$DeDuplicatedAGYW_PREV_Sum <- a$DeDuplicatedAGYW_PREV_2018 + a$DeDuplicatedAGYW_PREV_2019 + a$DeDuplicatedAGYW_PREV_2020 + a$DeDuplicatedAGYW_PREV_2021 + a$DeDuplicatedAGYW_PREV_2022
  a$MobilityStandardizedAGYW_PREV_Sum <- a$MobilityStandardizedAGYW_PREV_2018 + a$MobilityStandardizedAGYW_PREV_2019 + a$MobilityStandardizedAGYW_PREV_2020 + a$MobilityStandardizedAGYW_PREV_2021 + a$MobilityStandardizedAGYW_PREV_2022
  a$EnrollmentStandardizedAGYW_PREV_Sum <- a$EnrollmentStandardizedAGYW_PREV_2018 + a$EnrollmentStandardizedAGYW_PREV_2019 + a$EnrollmentStandardizedAGYW_PREV_2020 + a$EnrollmentStandardizedAGYW_PREV_2021 + a$EnrollmentStandardizedAGYW_PREV_2022

  b <- a %>%
    dplyr::select((-ends_with(c("2018",
                                "2019",
                                "2020",
                                "2021")))&(-starts_with(c("first",
                                                          "second",
                                                          "third",
                                                          "fourth",
                                                          "fifth")))) %>%
    dplyr::select(-c("country",
                     "AGYW_PREV_2022",
                     "DeDuplicatedAGYW_PREV_2022",
                     "MobilityStandardizedAGYW_PREV_2022",
                     "EnrollmentStandardizedAGYW_PREV_2022",
                     "PopRemaining_2022",
                     "PopStructure"))
}




# Plot theme ----

theme_plot <- function(...) {
  theme(
    plot.title.position = "plot",
    #text = element_text(family = "Lato"), #UPDATE THIS ONCE I PICK A FONT
    
    # background colors
    plot.background = element_rect(fill = "transparent",
                                   color = NA),
    panel.background = element_rect(fill = "transparent",
                                   color = NA),
    legend.background = element_rect(fill = "transparent",
                                   color = NA),
    
    #titles
    legend.title = element_blank(),
    legend.text = element_text(size=12,
                               color = "black",
                               face = "plain"),
    plot.title = element_text(size=14,
                              color = "black",
                              face = "bold",
                              lineheight = 1.2),
    plot.subtitle = element_text(size=13,
                                 color = "black",
                                 face = "italic"),
    plot.caption = element_text(size=8,
                                color = "black",
                                vjust = 3),
    axis.title.x = element_text(size=12,
                                color = "black",
                                face = "bold"),
    axis.title.y = element_text(size=12,
                                color = "black",
                                face = "bold"),
    axis.text.x = element_text(size=8.5,
                               color = "black"),
    axis.text.y = element_text(size=9.5,
                               color = "black"),
    axis.ticks.y = element_blank(),
    ...
  )
}

# Prep Q data for pop structure plots ----

prepQDataforPopStructurePlots <- function(x) {
  
  a <- x
  
  b <- a %>%
    tidyr::pivot_longer(
      cols = contains("Q"),
      names_to = c("age_quintile", "fiscal_year"), 
      names_sep = "_",
      values_to = "prop"
    ) %>%
    dplyr::mutate(
      age_quintile = case_when(
        (age_quintile == "firstQNat") ~ "firstQ",
        (age_quintile == "secondQNat") ~ "secondQ",
        (age_quintile == "thirdQNat") ~ "thirdQ",
        (age_quintile == "fourthQNat") ~ "fourthQ",
        (age_quintile == "fifthQNat") ~ "fifthQ",
        (age_quintile == "firstQCustom") ~ "firstQ",
        (age_quintile == "secondQCustom") ~ "secondQ",
        (age_quintile == "thirdQCustom") ~ "thirdQ",
        (age_quintile == "fourthQCustom") ~ "fourthQ",
        (age_quintile == "fifthQCustom") ~ "fifthQ",
        TRUE ~ as.character(age_quintile)
      )
    ) %>%
    dplyr::mutate(
      age = case_when(
        (age_quintile == "firstQ" & ageasentered == "10-14") ~ as.numeric(10),
        (age_quintile == "secondQ" & ageasentered == "10-14") ~ as.numeric(11),
        (age_quintile == "thirdQ" & ageasentered == "10-14") ~ as.numeric(12),
        (age_quintile == "fourthQ" & ageasentered == "10-14") ~ as.numeric(13),
        (age_quintile == "fifthQ" & ageasentered == "10-14") ~ as.numeric(14),
        (age_quintile == "firstQ" & ageasentered == "15-19") ~ as.numeric(15),
        (age_quintile == "secondQ" & ageasentered == "15-19") ~ as.numeric(16),
        (age_quintile == "thirdQ" & ageasentered == "15-19") ~ as.numeric(17),
        (age_quintile == "fourthQ" & ageasentered == "15-19") ~ as.numeric(18),
        (age_quintile == "fifthQ" & ageasentered == "15-19") ~ as.numeric(19),
        (age_quintile == "firstQ" & ageasentered == "20-24") ~ as.numeric(20),
        (age_quintile == "secondQ" & ageasentered == "20-24") ~ as.numeric(21),
        (age_quintile == "thirdQ" & ageasentered == "20-24") ~ as.numeric(22),
        (age_quintile == "fourthQ" & ageasentered == "20-24") ~ as.numeric(23),
        (age_quintile == "fifthQ" & ageasentered == "20-24") ~ as.numeric(24),
        (age_quintile == "firstQ" & ageasentered == "25-29") ~ as.numeric(25),
        (age_quintile == "secondQ" & ageasentered == "25-29") ~ as.numeric(26),
        (age_quintile == "thirdQ" & ageasentered == "25-29") ~ as.numeric(27),
        (age_quintile == "fourthQ" & ageasentered == "25-29") ~ as.numeric(28),
        (age_quintile == "fifthQ" & ageasentered == "25-29") ~ as.numeric(29),
      )
    ) %>%
    dplyr::select(-c("age_quintile")) %>%
    dplyr::rename()
  
  b$prop <- round((b$prop*100),1) 
  
  return(b)

}


# connection function
s3_connect <- function() {
  
  #Set the profile and region here
  Sys.setenv(SECRET_NAME = Sys.getenv("SECRET_NAME"),
             AWS_DEFAULT_REGION = "us-east-1",
             AWS_REGION = "us-east-1")
  
  svc <- secretsmanager()
  
  #Put the name of the secret which contains the aws key info
  see <- svc$get_secret_value(
    SecretId = Sys.getenv("SECRET_NAME")
  )
  
  see <- fromJSON(see$SecretString)
  
  #Fill in the strings
  Sys.setenv(AWS_ACCESS_KEY_ID = see$aws_access_key,
             AWS_SECRET_ACCESS_KEY = see$aws_secret_access_key,
             #AWS_PROFILE = Sys.getenv("AWS_PROFILE"),
             AWS_DEFAULT_REGION = "us-east-1",
             AWS_REGION = "us-east-1")
  
  #Delete the secret info as its now an env variable
  rm(see)
  rm(svc)
  
  
}





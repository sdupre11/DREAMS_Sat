#CONTENT HERE

dataParametersImportandMutate <- function(x) {
  
  readxl::read_xlsx(x) %>%
    mutate(
      ageasentered = case_when(
        (AgeCohort == "10 to 14") ~ as.character("10-14"),
        (AgeCohort == "15 to 19") ~ as.character("15-19"),
        (AgeCohort == "20 to 24") ~ as.character("20-24"),
        (AgeCohort == "25 to 29") ~ as.character("25-29")
      )
    ) %>%
    select(-"AgeCohort")
}

##############################

dataParametersPivot1Year <- function(x) {
  
  a <- x %>%
    pivot_longer(
      cols = GradStructure_2019:GradStructure_2022,
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
    select(-(7:14)) %>%
    pivot_longer(
      cols = Prevalence_2019:Prevalence_2022,
      names_to = "fiscal_year",
      names_prefix = "Prevalence_",
      values_to = "Prevalence"
    )
  
  Vulnerable <- x %>%
    select(-(3:6)) %>%
    select(-(7:10)) %>%
    pivot_longer(
      cols = Vulnerable_2019:Vulnerable_2022,
      names_to = "fiscal_year",
      names_prefix = "Vulnerable_",
      values_to = "Vulnerable"
    )

  PrimarySecondaryDoubleCounts <- x %>%
    select(-(3:10)) %>%
    pivot_longer(
      cols = PrimarySecondaryDoubleCounts_2019:PrimarySecondaryDoubleCounts_2022,
      names_to = "fiscal_year",
      names_prefix = "PrimarySecondaryDoubleCounts_",
      values_to = "PrimarySecondaryDoubleCounts"
    )

  PrevVuln <- left_join(Prevalence,
                        Vulnerable,
                        by = c("Country" = "Country", "District" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered"))
  
  All <- left_join(PrevVuln,
                   PrimarySecondaryDoubleCounts,
                   by = c("Country" = "Country", "District" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered"))
  
  All$fiscal_year <- All$fiscal_year %>%
    as.numeric()
  
  return(All)

}

#Reshape to match PoC

splitForReshapeWide <- function(x, y) {
  
  x %>%
    select(c("AREA_NAME",
             "ageasentered",
             "country",
             "fiscal_year",
             "populationtx",
             all_of(y)))
  
}

reshapeWide <- function(x) {

  PrevalenceDF <- splitForReshapeWide(x, "Prevalence")
  VulnerableDF <- splitForReshapeWide(x, "Vulnerable")
  PrimarySecondaryDoubleCountsDF <- splitForReshapeWide(x, "PrimarySecondaryDoubleCounts")
  PopDF <- splitForReshapeWide(x, "population")
  AGYW_PREVDF <- splitForReshapeWide(x, "AGYW_PREV")

  PrevalenceDF <- PrevalenceDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  country,
                  populationtx),
      names_from = fiscal_year,
      names_glue = "Prev_{fiscal_year}",
      values_from = Prevalence
    )
  
  VulnerableDF <- VulnerableDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  country,
                  populationtx),
      names_from = fiscal_year,
      names_glue = "Vuln_{fiscal_year}",
      values_from = Vulnerable
    )

  PrimarySecondaryDoubleCountsDF <- PrimarySecondaryDoubleCountsDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  country,
                  populationtx),
      names_from = fiscal_year,
      names_glue = "PSDC_{fiscal_year}",
      values_from = PrimarySecondaryDoubleCounts
    )

  PopDF <- PopDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  country,
                  populationtx),
      names_from = fiscal_year,
      names_glue = "Pop_{fiscal_year}",
      values_from = population
    )

  AGYW_PREVDF <- AGYW_PREVDF %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  country,
                  populationtx),
      names_from = fiscal_year,
      names_glue = "AGYW_PREV_{fiscal_year}",
      values_from = AGYW_PREV
    )
  
  PrevVuln <- left_join(PrevalenceDF,
                        VulnerableDF,
                   by = c("country" = "country", 
                          "AREA_NAME" = "AREA_NAME", 
                          "ageasentered" = "ageasentered",
                          "populationtx" = "populationtx"))
  
  PrevVulnPSDC <- left_join(PrevVuln,
                        PrimarySecondaryDoubleCountsDF,
                        by = c("country" = "country", 
                               "AREA_NAME" = "AREA_NAME", 
                               "ageasentered" = "ageasentered",
                               "populationtx" = "populationtx"))
  PrevVulnPSDCPop <- left_join(PrevVulnPSDC,
                            PopDF,
                            by = c("country" = "country", 
                                   "AREA_NAME" = "AREA_NAME", 
                                   "ageasentered" = "ageasentered",
                                   "populationtx" = "populationtx"))
  
  All <- left_join(PrevVulnPSDCPop,
                   AGYW_PREVDF,
                   by = c("country" = "country", 
                          "AREA_NAME" = "AREA_NAME", 
                          "ageasentered" = "ageasentered",
                          "populationtx" = "populationtx"))
  
  return(All)
}


# Derive statistics

deriveStatisticsPreSat <- function(x) {
  a <- x 
  
  a$PLHIV_2019 <- round((a$Pop_2019 * ((a$Prev_2019)/100)), 
                        0)
  
  a$PLHIV_2020 <- round((a$Pop_2020 * ((a$Prev_2020)/100)), 
                        0)
  
  a$PLHIV_2021 <- round((a$Pop_2021 * ((a$Prev_2021)/100)), 
                        0)
  
  a$PLHIV_2022 <- round((a$Pop_2022 * ((a$Prev_2022)/100)), 
                        0)
  
  a$NonPLHIV_2019 <- round((a$Pop_2019 - a$PLHIV_2019),
                           0)
  
  a$NonPLHIV_2020 <- round((a$Pop_2020 - a$PLHIV_2020),
                           0)
  
  a$NonPLHIV_2021 <- round((a$Pop_2021 - a$PLHIV_2021),
                           0)
  
  a$NonPLHIV_2022 <- round((a$Pop_2022 - a$PLHIV_2022),
                           0)
  
  a$VulnerableNonPLHIV_2019 <- round(a$NonPLHIV_2019 * (a$Vuln_2019/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2020 <- round(a$NonPLHIV_2020 * (a$Vuln_2020/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2021 <- round(a$NonPLHIV_2021 * (a$Vuln_2021/100), 
                                     0)
  
  a$VulnerableNonPLHIV_2022 <- round(a$NonPLHIV_2022 * (a$Vuln_2022/100), 
                                     0)
  
  a <- replaceAGYWNAs(a) 
  
  a$DeDuplicatedAGYW_PREV_2019 <- round(a$AGYW_PREV_2019 * ((100-a$PSDC_2019)/100),
                                       0)

  a$DeDuplicatedAGYW_PREV_2020 <- round(a$AGYW_PREV_2020 * ((100-a$PSDC_2020)/100),
                                       0)

  a$DeDuplicatedAGYW_PREV_2021 <- round(a$AGYW_PREV_2021 * ((100-a$PSDC_2021)/100),
                                       0)

  a$DeDuplicatedAGYW_PREV_2022 <- round(a$AGYW_PREV_2022 * ((100-a$PSDC_2022)/100),
                                       0)

  
  return(a)
}

deriveStatisticsSat <- function(x) {
  
  a <- x
  b <- x
  
  a$PopStructure <- "Custom"
  
  b$PopStructure <- "Default"
  
  a$Actual_Served_2019 <- round(a$DeDuplicatedAGYW_PREV_2019,
                                   0)
  
  a$Actual_Served_2020 <- round((a$DeDuplicatedAGYW_PREV_2020 + (a$DeDuplicatedAGYW_PREV_2019*(1-a$fifthQ_2019))),
                                   0)
  
  a$Actual_Served_2021 <- round((a$DeDuplicatedAGYW_PREV_2021 + (a$DeDuplicatedAGYW_PREV_2020*(1-a$fifthQ_2020)) + (a$DeDuplicatedAGYW_PREV_2019*(1-(a$fifthQ_2019+a$fourthQ_2019)))),
                                   0)
  
  a$Actual_Served_2022 <- round((a$DeDuplicatedAGYW_PREV_2022 + (a$DeDuplicatedAGYW_PREV_2021*(1-a$fifthQ_2021)) + (a$DeDuplicatedAGYW_PREV_2020*(1-(a$fifthQ_2020+a$fourthQ_2020))) + (a$DeDuplicatedAGYW_PREV_2019*(1-(a$fifthQ_2019+a$fourthQ_2019+a$thirdQ_2019)))),
                                   0)
  
  
  a$Sat_2019 <- round(((a$Actual_Served_2019/a$VulnerableNonPLHIV_2019)*100),
                      1)
  
  a$Sat_2020 <- round(((a$Actual_Served_2020/a$VulnerableNonPLHIV_2020)*100),
                      1)
  
  a$Sat_2021 <- round(((a$Actual_Served_2021/a$VulnerableNonPLHIV_2021)*100),
                      1)
  
  a$Sat_2022 <- round(((a$Actual_Served_2022/a$VulnerableNonPLHIV_2022)*100),
                      1)

  
  b$Actual_Served_2019 <- round(b$DeDuplicatedAGYW_PREV_2019,
                      0)
  
  b$Actual_Served_2020 <- round((b$DeDuplicatedAGYW_PREV_2020+(b$DeDuplicatedAGYW_PREV_2019*.8)),
                      0)
  
  b$Actual_Served_2021 <- round((b$DeDuplicatedAGYW_PREV_2021 + (b$DeDuplicatedAGYW_PREV_2020*.8) + (b$DeDuplicatedAGYW_PREV_2019*.6)),
                      0)
  
  b$Actual_Served_2022 <- round((b$DeDuplicatedAGYW_PREV_2022 + (b$DeDuplicatedAGYW_PREV_2021*.8) + (b$DeDuplicatedAGYW_PREV_2020*.6) + (b$DeDuplicatedAGYW_PREV_2019*.4)),
                      0)
  
  
  b$Sat_2019 <- round(((b$Actual_Served_2019/b$VulnerableNonPLHIV_2019)*100),
                      1)
  
  b$Sat_2020 <- round(((b$Actual_Served_2020/b$VulnerableNonPLHIV_2020)*100),
                      1)
  
  b$Sat_2021 <- round(((b$Actual_Served_2021/b$VulnerableNonPLHIV_2021)*100),
                      1)
  
  b$Sat_2022 <- round(((b$Actual_Served_2022/b$VulnerableNonPLHIV_2022)*100),
                      1)

  c <- rbind(a,
             b)
  
  c$IsSelected <- "Unselected"
  
  return(c)
  
}

deriveStatisticsGirlsRemaining <- function(x) {
  
  a <- x
  
  a$PopRemaining_2019 <- a$VulnerableNonPLHIV_2019 - a$Actual_Served_2019
  
  a$PopRemaining_2020 <- a$VulnerableNonPLHIV_2020 - a$Actual_Served_2020
  
  a$PopRemaining_2021 <- a$VulnerableNonPLHIV_2021 - a$Actual_Served_2021
  
  a$PopRemaining_2022 <- a$VulnerableNonPLHIV_2022 - a$Actual_Served_2022
  
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
    ) %>%
  
  return(a)
  
}


attachParameters_5year <- function(x, y, z) {

  a <- left_join(x,
            y,
            by = c("country" = "Country", "AREA_NAME" = "District", "fiscal_year" = "fiscal_year", "ageasentered" = "ageasentered"))

  return(a)
}
  
attachParameters_1year <- function(x, y) {
  
  a <- left_join(x,
                 y,
                 by = c("country" = "Country", "AREA_NAME" = "District", "ageasentered" = "ageasentered"))
  
  
  return(a)
}


reduceToCOPExport <- function(x) {
  
  col_order <- c("OU",
                 "District",
                 "Cohort",
                 "Total DREAMS eligible AGYW",
                 "Percent coverage (saturation)",
                 "Remaining unserved AGYW"
                 )
  
  a <- x %>%
    dplyr::filter(IsSelected == "Selected") %>%
    dplyr::select(-c("populationtx", 
                     "IsSelected",
                     "PopStructure")) %>%
    dplyr::select((-ends_with(c("2019", 
                                "2020", 
                                "2021")))&(-starts_with(c("first", 
                                                          "second", 
                                                          "third", 
                                                          "fourth", 
                                                          "fifth")))) %>%
    dplyr::select(-c("Prev_2022", 
                     "PSDC_2022", 
                     "Vuln_2022",
                     "Pop_2022",
                     "AGYW_PREV_2022",
                     "PLHIV_2022",
                     "NonPLHIV_2022",
                     "DeDuplicatedAGYW_PREV_2022",
                     "Actual_Served_2022")) %>%
    dplyr::rename("OU" = "country",
           "Cohort" = "ageasentered",
           "District" = "AREA_NAME",
           "Total DREAMS eligible AGYW" = "VulnerableNonPLHIV_2022",
           "Percent coverage (saturation)" = "Sat_2022",
           "Remaining unserved AGYW" = "PopRemaining_2022")
  
  a <- a[, col_order]
    
  return(a)
}
  



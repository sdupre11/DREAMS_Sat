

SingleYearNationalAGYWPops <- readxl::read_xlsx("preprocessing/data/Census_IDB_1Year.xlsx") %>%
  dplyr::rename("country" = `Country/Area Name`,
                "Age" = "GROUP",
                "Pop" = `Female Population`) %>%
  dplyr::mutate(
    ageasentered = case_when(
      (Age %in% c(10, 11, 12, 13, 14)) ~ as.character("10-14"),
      (Age %in% c(15, 16, 17, 18, 19)) ~ as.character("15-19"),
      (Age %in% c(20, 21, 22, 23, 24)) ~ as.character("20-24"),
      (Age %in% c(25, 26, 27, 28, 29)) ~ as.character("25-29"),
    ))


CohortPops <- SingleYearNationalAGYWPops %>%
  dplyr::group_by(country,
                  Year,
                  ageasentered) %>%
  dplyr::summarize(CohortPop=sum(Pop))

Joined <- merge(SingleYearNationalAGYWPops,
                CohortPops)

Joined$CohortProportion <- round((Joined$Pop/Joined$CohortPop),3) 

firstQ <- c(10, 15, 20, 25)
secondQ <- c(11, 16, 21, 26)
thirdQ <- c(12, 17, 22, 27)
fourthQ <- c(13, 18, 23, 28)
fifthQ <- c(14, 19, 24, 29)

rm(SingleYearNationalAGYWPops)

SingleYearNationalAGYWPops <- Joined %>%
  dplyr::select(-c(
    "Pop",
    "CohortPop"
  )) %>%
  mutate(
    Age = case_when(
      (Age %in% firstQ) ~ as.character("firstQNat"),
      (Age %in% secondQ) ~ as.character("secondQNat"),
      (Age %in% thirdQ) ~ as.character("thirdQNat"),
      (Age %in% fourthQ) ~ as.character("fourthQNat"),
      (Age %in% fifthQ) ~ as.character("fifthQNat")
    )) %>%
  pivot_wider(
    names_from = c("Age", "Year"),
    values_from = "CohortProportion"
  )

rm(CohortPops)
rm(Joined)

saveRDS(SingleYearNationalAGYWPops, file = "data/SingleYearNationalAGYWPops.RDS")




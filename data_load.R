DREAMS_Districts_Botswana <- c("CENTRAL", 
                               "KGATLENG", 
                               "KWENENG", 
                               "NORTH EAST", 
                               "SOUTH EAST", 
                               "SOUTHERN")

DREAMS_Districts_Kenya <- c("HOMA BAY", 
                            "KIAMBU", 
                            "KISUMU", 
                            "MIGORI", 
                            "MOMBASA", 
                            "NAIROBI CITY", 
                            "SIAYA")

DREAMS_Districts_Lesotho <- c("BEREA", 
                              "MAFETENG",
                              "MASERU",
                              "MOHALE'S HOEK")

DREAMS_Districts_Malawi <- c("BLANTYRE", 
                              "MACHINGA",
                              "ZOMBA")

DREAMS_Districts_SouthAfrica <- c("ALFRED NZO",
                                  "BOJANALA",
                                  "BUFFALO CITY",
                                  "CAPRICORN",
                                  "CITY OF CAPE TOWN",
                                  "CITY OF JOHANNESBURG",
                                  "CITY OF TSHWANE",
                                  "DOCTOR KENNETH KAUNDA",
                                  "EHLANZENI",
                                  "EKURHULENI",
                                  "ETHEKWINI",
                                  "GERT SIBANDE",
                                  "LEJWELEPUTSWA",
                                  "MOPANI",
                                  "NGAKA MODIRI MOLEMA",
                                  "NKANGALA",
                                  "O.R. TAMBO",
                                  "SEDIBENG",
                                  "THABO MOFUTSANYANE",
                                  "UGU",
                                  "UMGUNGUNDLOVU",
                                  "UTHUKELA",
                                  "UTHUNGULU",
                                  "ZULULAND")

# DREAMS_Districts_Tanzania <- c("MWANZA",
#                                "KAGERA")

DREAMS_Districts_Zimbabwe <- c("BULAWAYO", 
                               "MANICALAND", 
                               "MASHONALAND CENTRAL", 
                               "MATABELELAND NORTH", 
                               "MATABELELAND SOUTH", 
                               "MIDLANDS")

countryData <- readRDS("data/countryData.RDS")

AGYW_PREV <- s3read_using(FUN = read.csv,
             bucket = Sys.getenv("TEST_BUCKET_WRITE"),
             object = "system_dreams_saturation/AGYW_PREVbyCountry.csv") %>%
  as.data.frame()

countryDataJoined <- left_join(countryData,
                               AGYW_PREV,
                               by = c("AREA_NAME" = "AREA_NAME", 
                                      "fiscal_year" = "fiscal_year", 
                                      "country" = "country", 
                                      "ageasentered" = "ageasentered")) %>%
  select(-c("snu1",
            "psnu",
            "X"))

botADM1.sf <- readRDS('data/BotswanaADM1.RDS') %>%
  attachDREAMSField("Botswana")
botADM2.sf <- readRDS('data/BotswanaADM2.RDS') %>%
  attachDREAMSField("Botswana")
kenADM1.sf <- readRDS('data/KenyaADM1.RDS') %>%
  attachDREAMSField("Kenya")
kenADM2.sf <- readRDS('data/KenyaADM2.RDS') %>%
  attachDREAMSField("Kenya")
lesADM1.sf <- readRDS('data/LesothoADM1.RDS') %>%
  attachDREAMSField("Lesotho")
malADM1.sf <- readRDS('data/MalawiADM1.RDS') %>%
  attachDREAMSField("Malawi")
safADM1.sf <- readRDS('data/SAfricaADM1.RDS') %>%
  attachDREAMSField("South Africa") %>%
  mutate(
    AREA_NAME = case_when(
      (AREA_NAME == "OR TAMBO") ~ "O.R. TAMBO",
      TRUE ~ as.character(AREA_NAME)
    )
  )
safADM2.sf <- readRDS('data/SAfricaADM2.RDS') %>%
  attachDREAMSField("South Africa") 
# tanADM1.sf <- readRDS('data/TanzaniaADM1.RDS') %>%
#   attachDREAMSField("Tanzania")
# tanADM2.sf <- readRDS('data/TanzaniaADM2.RDS') %>%
#   attachDREAMSField("Tanzania")
zimADM1.sf <- readRDS('data/ZimbabweADM1.RDS') %>%
  attachDREAMSField("Zimbabwe")
zimADM2.sf <- readRDS('data/ZimbabweADM2.RDS') %>%
  attachDREAMSField("Zimbabwe")

dataParameters_1Year <- dataParametersImportandMutate("www/defaultTemplate_1year.xlsx") %>%
  dataParametersPivot1Year()

dataParameters_5Year  <- dataParametersImportandMutate("www/defaultTemplate_5year.xlsx") %>%
  dataParametersPivot5Year()

SingleYearNatAGYWPops <- readRDS('data/SingleYearNationalAGYWPops.RDS')


small_countries <- c("Cote d'Ivoire",
                     "Eswatini",
                     "Haiti",
                     "Lesotho",
                     "Rwanda")
medium_countries <- c("Malawi",
                      "Mozambique",
                      "Uganda",
                      "Zambia",
                      "Zimbabwe")
large_countries <- c("Botswana", 
                     "Kenya", 
                     "Namibia",
                     "South Africa", 
                     "Tanzania")

neighborsLookup <- readRDS('data/neighborsLookupAll.RDS')

default1YearTemplate <- readxl::read_xlsx('www/defaultTemplate_1year.xlsx') 
default5YearTemplate <- readxl::read_xlsx('www/defaultTemplate_5year.xlsx') %>%
  mutate(
    AgeCohort = case_when(
      (AgeCohort == "10 to 14") ~ as.character("10-14"),
      (AgeCohort == "15 to 19") ~ as.character("15-19"),
      (AgeCohort == "20 to 24") ~ as.character("20-24"),
      (AgeCohort == "25 to 29") ~ as.character("25-29")
    )
  )

defaultData <- attachParameters_5year(countryDataJoined,
                                      dataParameters_5Year) %>%
  reshapeWide() %>%
  attachParameters_1year(dataParameters_1Year) %>%
  merge(SingleYearNatAGYWPops) %>%
  deriveStatistics()
  
defaultStatsCOP <- defaultData %>%
  mutate(
    IsSelected = case_when(
      (PopStructure == "Default" & populationtx == "DistrictOnly") ~ as.character("Selected"),
      TRUE ~ as.character("Unselected")
    )
  ) %>%
  reduceToCOPExport()
  
  
rm(countryData)
rm(countryDataJoined)
rm(AGYW_PREV)



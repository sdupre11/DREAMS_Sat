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
                               by = c("AREA_NAME" = "AREA_NAME", "fiscal_year" = "fiscal_year", "country" = "country", "ageasentered" = "ageasentered")) %>%
  select(-c("snu1",
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
zimADM1.sf <- readRDS('data/ZimbabweADM1.RDS') %>%
  attachDREAMSField("Zimbabwe")
zimADM2.sf <- readRDS('data/ZimbabweADM2.RDS') %>%
  attachDREAMSField("Zimbabwe")

dataParameters_1Year <- dataParametersImportandMutate("www/defaultTemplate_1year.xlsx") %>%
  dataParametersPivot1Year()

dataParameters_5Year  <- dataParametersImportandMutate("www/defaultTemplate_5year.xlsx") %>%
  dataParametersPivot5Year()

SingleYearNatAGYWPops <- readRDS('data/SingleYearNationalAGYWPops.RDS')


small_countries <- c("Lesotho")
medium_countries <- c("Zimbabwe")
large_countries <- c("Botswana", "Kenya")

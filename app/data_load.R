

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

botADM1.sf <- readRDS('data/BotswanaADM1.RDS')
botADM2.sf <- readRDS('data/BotswanaADM2.RDS')
kenADM1.sf <- readRDS('data/KenyaADM1.RDS')
kenADM2.sf <- readRDS('data/KenyaADM2.RDS')
lesADM1.sf <- readRDS('data/LesothoADM1.RDS')
zimADM1.sf <- readRDS('data/ZimbabweADM1.RDS')
zimADM2.sf <- readRDS('data/ZimbabweADM2.RDS')

dataParameters_1Year <- dataParametersImportandMutate("www/defaultTemplate_1year.xlsx") %>%
  dataParametersPivot1Year()

dataParameters_5Year  <- dataParametersImportandMutate("www/defaultTemplate_5year.xlsx") %>%
  dataParametersPivot5Year()

small_countries <- c("Lesotho")
medium_countries <- c("Zimbabwe")
large_countries <- c("Botswana", "Kenya")
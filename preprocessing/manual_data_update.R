library(aws.s3)
library(readxl)
library(paws)
library(jsonlite)

# connect ---------------------------------------

#Set the profile and region here
Sys.setenv(SECRET_NAME = Sys.getenv("SECRET_NAME"),
           AWS_DEFAULT_REGION = Sys.getenv("REGION"),
           AWS_REGION = Sys.getenv("REGION"))

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
           AWS_DEFAULT_REGION = Sys.getenv("REGION"),
           AWS_REGION = Sys.getenv("REGION"))

#Delete the secret info as its now an env variable
rm(see)
rm(svc)


# read from the main read bucket -----------------------------------------------
my_bucket <- Sys.getenv("TEST_BUCKET")

# Lists all of bucket contents, fill in your bucket
choices <- aws.s3::get_bucket(bucket = my_bucket)

# get just path names
choices <- lapply(choices, "[[", 1)

# get just file names
cleaned_choices <- lapply(choices, function(x) gsub(".*\\/", "", x))

# make dataframe of file names and path names
choices <- do.call(rbind, Map(data.frame, file_names = cleaned_choices,
                              path_names = choices, stringsAsFactors = FALSE))
# filter just files that end in txt or xlsx or csv
choices <- choices[grepl("txt$|xlsx$|csv$|xlsx$", choices$file_names), ]

print(choices)
# reset row names
rownames(choices) <- NULL

choices_recent_countries_all_historic <- subset(choices, grepl("MER_Structured_Datasets/Current_Frozen/PSNU_Historic/txt/", path_names))

#Jan 23, 2024 removed Eswatini and Rwanda throughout
choices_recent_countries_all2_historic <- subset(choices_recent_countries_all_historic, grepl("Botswana|Cote d'Ivoire|Haiti|Kenya|Lesotho|Malawi|Mozambique|Namibia|South Africa|Tanzania|Uganda|Zambia|Zimbabwe", path_names))


my_data_historic_psnus <-
  lapply(choices_recent_countries_all2_historic$path_names, function(the_file) {

    print(the_file)

    # read the data
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ),
                                 bucket = my_bucket,
                                 object = the_file)

    res <- data %>%
      filter(indicator == "AGYW_PREV" & sex == "Female" & (standardizeddisaggregate == "Age/Sex/Time/Complete" |standardizeddisaggregate == "Age/Sex/Time/Complete+")) %>%
      filter(numeratordenom != "D") %>% #New Jan 23, 2024. Was getting some duplication (e.g., Bobirwa 2022, 10-14 population)
      filter(country %in% c("Botswana",
                            "Cote d'Ivoire",
                            "Haiti",
                            "Malawi",
                            "Mozambique",
                            "Namibia",
                            "South Africa",
                            "Tanzania",
                            "Uganda",
                            "Zambia",
                            "Zimbabwe")) %>%
      filter(!is.na(qtr4)) %>%
      #group_by(country, snu1, psnu, ageasentered, fiscal_year) %>% #Original
      group_by(country, snu1, cop22_psnu, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))

  }) %>%
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (cop22_psnu == "Bobirwa District" & country == "Botswana") ~ "BOBONONG",
      (cop22_psnu == "Gaborone District" & country == "Botswana") ~ "GABORONE",
      (cop22_psnu == "Kgatleng District" & country == "Botswana") ~ "KGATLENG",
      (cop22_psnu == "Kweneng East District" & country == "Botswana") ~ "KWENENG EAST",
      (cop22_psnu == "Mahalapye District" & country == "Botswana") ~ "MAHALAPYE",
      (cop22_psnu == "North East District" & country == "Botswana") ~ "NORTH EAST",
      (cop22_psnu == "Serowe District" & country == "Botswana") ~ "SEROWE PALAPYE",
      (cop22_psnu == "Southern District" & country == "Botswana") ~ "SOUTHERN",
      (cop22_psnu == "Abobo-Est" & country == "Cote d'Ivoire") ~ "Abobo-Est",
      (cop22_psnu == "Cocody-Bingerville" & country == "Cote d'Ivoire") ~ "Cocody-Bingerville",
      (cop22_psnu == "Daloa" & country == "Cote d'Ivoire") ~ "Daloa",
      (cop22_psnu == "Man" & country == "Cote d'Ivoire") ~ "Man",
      (cop22_psnu == "Cap-Haïtien" & country == "Haiti") ~ "Cap-Haïtien",
      (cop22_psnu == "Dessalines" & country == "Haiti") ~ "Dessalines",
      (cop22_psnu == "Port-au-Prince" & country == "Haiti") ~ "Port-au-Prince",
      (cop22_psnu == "Saint-Marc" & country == "Haiti") ~ "Saint-Marc",
      (cop22_psnu == "Blantyre District" & country == "Malawi") ~ "BLANTYRE", #cop22_psnu for Malawi
      (cop22_psnu == "Chiradzulu District" & country == "Malawi") ~ "CHIRADZULU", #cop22_psnu for Malawi
      (cop22_psnu == "Machinga District" & country == "Malawi") ~ "MACHINGA", #cop22_psnu for Malawi
      (cop22_psnu == "Phalombe District" & country == "Malawi") ~ "PHALOMBE", #cop22_psnu for Malawi
      (cop22_psnu == "Zomba District" & country == "Malawi") ~ "ZOMBA", #cop22_psnu for Malawi
      (cop22_psnu == "Boane" & country == "Mozambique") ~ "BOANE",
      (cop22_psnu == "Caia" & country == "Mozambique") ~ "CAIA",
      (cop22_psnu == "Beira" & country == "Mozambique") ~ "CIDADE DA BEIRA",
      (cop22_psnu == "Chimoio" & country == "Mozambique") ~ "CIDADE DE CHIMOIO",
      (cop22_psnu == "Matola" & country == "Mozambique") ~ "CIDADE DA MATOLA",
      (cop22_psnu == "Maxixe" & country == "Mozambique") ~ "CIDADE DA MAXIXE",
      (cop22_psnu == "Nampula" & country == "Mozambique") ~ "CIDADE DE NAMPULA",
      (cop22_psnu == "Pemba" & country == "Mozambique") ~ "CIDADE DE PEMBA",
      (cop22_psnu == "Xai-Xai" & country == "Mozambique") ~ "CIDADE DE XAI-XAI",
      (cop22_psnu == "Chokwe" & country == "Mozambique") ~ "CHÓKWÈ",
      (cop22_psnu == "Chonguene" & country == "Mozambique") ~ "CHONGOENE",
      (cop22_psnu == "Erati" & country == "Mozambique") ~ "ERÁTI",
      (cop22_psnu == "Gile" & country == "Mozambique") ~ "GILÉ",
      (cop22_psnu == "Guija" & country == "Mozambique") ~ "GUIJÁ",
      (cop22_psnu == "Ile" & country == "Mozambique") ~ "ILE",
      (cop22_psnu == "Inhassunge" & country == "Mozambique") ~ "INHASSUNGE",
      (cop22_psnu == "Limpopo" & country == "Mozambique") ~ "LIMPOPO",
      (cop22_psnu == "Lugela" & country == "Mozambique") ~ "LUGELA",
      (cop22_psnu == "Maganja Da Costa" & country == "Mozambique") ~ "MAGANJA DA COSTA",
      (cop22_psnu == "Magude" & country == "Mozambique") ~ "MAGUDE",
      (cop22_psnu == "Manhiça" & country == "Mozambique") ~ "MANHIÇA",
      (cop22_psnu == "Marracuene" & country == "Mozambique") ~ "MARRACUENE",
      (cop22_psnu == "Matutuine" & country == "Mozambique") ~ "MATUTUINE",
      (cop22_psnu == "Milange" & country == "Mozambique") ~ "MILANGE",
      (cop22_psnu == "Moamba" & country == "Mozambique") ~ "MOAMBA",
      (cop22_psnu == "Mocuba" & country == "Mozambique") ~ "MOCUBA",
      (cop22_psnu == "Mocubela" & country == "Mozambique") ~ "MOCUBELA",
      (cop22_psnu == "Namaacha" & country == "Mozambique") ~ "NAMAACHA",
      (cop22_psnu == "Namacurra" & country == "Mozambique") ~ "NAMACURRA",
      (cop22_psnu == "Nicoadala" & country == "Mozambique") ~ "NICOADALA",
      (cop22_psnu == "Pebane" & country == "Mozambique") ~ "PEBANE",
      (cop22_psnu == "Quelimane" & country == "Mozambique") ~ "QUELIMANE",
      (cop22_psnu == "Katima Mulilo" & country == "Namibia") ~ "KATIMA MULILO",
      (cop22_psnu == "Andara" & country == "Namibia") ~ "MUKWE",
      (cop22_psnu == "Nyangana" & country == "Namibia") ~ "NDIYONA",
      (cop22_psnu == "Omuthiya" & country == "Namibia") ~ "OMUTHIYAGWIIPUNDI",
      (cop22_psnu == "Onandjokwe" & country == "Namibia") ~ "ONIIPA",
      (cop22_psnu == "Oshakati" & country == "Namibia") ~ "OSHAKATI",
      (cop22_psnu == "Rundu" & country == "Namibia") ~ "RUNDU",
      (cop22_psnu == "Tsumeb" & country == "Namibia") ~ "TSUMEB",
      (cop22_psnu == "Windhoek" & country == "Namibia") ~ "WINDHOEK",
      (cop22_psnu == "ec Alfred Nzo District Municipality" & country == "South Africa") ~ "ALFRED NZO", #cop22_psnu for South Africa
      (cop22_psnu == "nw Bojanala Platinum District Municipality" & country == "South Africa") ~ "BOJANALA", #cop22_psnu for South Africa
      (cop22_psnu == "ec Buffalo City Metropolitan Municipality" & country == "South Africa") ~ "BUFFALO CITY", #cop22_psnu for South Africa
      (cop22_psnu == "lp Capricorn District Municipality" & country == "South Africa") ~ "CAPRICORN", #cop22_psnu for South Africa
      (cop22_psnu == "wc City of Cape Town Metropolitan Municipality" & country == "South Africa") ~ "CITY OF CAPE TOWN", #cop22_psnu for South Africa
      (cop22_psnu == "gp City of Johannesburg Metropolitan Municipality" & country == "South Africa") ~ "CITY OF JOHANNESBURG", #cop22_psnu for South Africa
      (cop22_psnu == "gp City of Tshwane Metropolitan Municipality" & country == "South Africa") ~ "CITY OF TSHWANE", #cop22_psnu for South Africa
      (cop22_psnu == "nw Dr Kenneth Kaunda District Municipality" & country == "South Africa") ~ "DOCTOR KENNETH KAUNDA", #cop22_psnu for South Africa
      (cop22_psnu == "mp Ehlanzeni District Municipality" & country == "South Africa") ~ "EHLANZENI", #cop22_psnu for South Africa
      (cop22_psnu == "gp Ekurhuleni Metropolitan Municipality" & country == "South Africa") ~ "EKURHULENI", #cop22_psnu for South Africa
      (cop22_psnu == "kz eThekwini Metropolitan Municipality" & country == "South Africa") ~ "ETHEKWINI", #cop22_psnu for South Africa
      (cop22_psnu == "mp Gert Sibande District Municipality" & country == "South Africa") ~ "GERT SIBANDE", #cop22_psnu for South Africa
      (cop22_psnu == "fs Lejweleputswa District Municipality" & country == "South Africa") ~ "LEJWELEPUTSWA", #cop22_psnu for South Africa
      (cop22_psnu == "lp Mopani District Municipality" & country == "South Africa") ~ "MOPANI", #cop22_psnu for South Africa
      (cop22_psnu == "nw Ngaka Modiri Molema District Municipality" & country == "South Africa") ~ "NGAKA MODIRI MOLEMA", #cop22_psnu for South Africa
      (cop22_psnu == "mp Nkangala District Municipality" & country == "South Africa") ~ "NKANGALA", #cop22_psnu for South Africa
      (cop22_psnu == "ec Oliver Tambo District Municipality" & country == "South Africa") ~ "OR TAMBO", #cop22_psnu for South Africa
      (cop22_psnu == "gp Sedibeng District Municipality" & country == "South Africa") ~ "SEDIBENG", #cop22_psnu for South Africa
      (cop22_psnu == "fs Thabo Mofutsanyane District Municipality" & country == "South Africa") ~ "THABO MOFUTSANYANE", #cop22_psnu for South Africa
      (cop22_psnu == "kz Ugu District Municipality" & country == "South Africa") ~ "UGU", #cop22_psnu for South Africa
      (cop22_psnu == "kz uMgungundlovu District Municipality" & country == "South Africa") ~ "UMGUNGUNDLOVU", #cop22_psnu for South Africa
      (cop22_psnu == "kz Uthukela District Municipality" & country == "South Africa") ~ "UTHUKELA", #cop22_psnu for South Africa
      (cop22_psnu == "kz King Cetshwayo District Municipality" & country == "South Africa") ~ "UTHUNGULU", #cop22_psnu for South Africa
      (cop22_psnu == "kz Zululand District Municipality" & country == "South Africa") ~ "ZULULAND", #cop22_psnu for South Africa
      (cop22_psnu == "Bukoba MC" & country == "Tanzania") ~ "BUKOBA MUNICIPAL COUNCIL",
      (cop22_psnu == "Iringa MC" & country == "Tanzania") ~ "IRINGA MUNICIPAL COUNCIL",
      (cop22_psnu == "Kahama TC" & country == "Tanzania") ~ "KAHAMA TOWN COUNCIL",
      (cop22_psnu == "Kyela DC" & country == "Tanzania") ~ "KYELA DISTRICT COUNCIL",
      (cop22_psnu == "Msalala DC" & country == "Tanzania") ~ "MSALALA",
      (cop22_psnu == "Mbarali DC" & country == "Tanzania") ~ "MBARALI DISTRICT COUNCIL",
      (cop22_psnu == "Mbeya CC" & country == "Tanzania") ~ "MBEYA CITY COUNCIL",
      (cop22_psnu == "Mufindi DC" & country == "Tanzania") ~ "MUFINDI DISTRICT COUNCIL",
      (cop22_psnu == "Muleba DC" & country == "Tanzania") ~ "MULEBA DISTRIC COUNCIL",
      (cop22_psnu == "Nyamagana MC" & country == "Tanzania") ~ "NYAMAGANA MUNICIPAL COUNCIL",
      (cop22_psnu == "Shinyanga DC" & country == "Tanzania") ~ "SHINYANGA DISTRICT COUNCIL",
      (cop22_psnu == "Shinyanga MC" & country == "Tanzania") ~ "SHINYANGA MUNICIPAL COUNCIL",
      (cop22_psnu == "Temeke MC" & country == "Tanzania") ~ "TEMEKE MUNICIPAL COUNCIL",
      (cop22_psnu == "Ushetu DC" & country == "Tanzania") ~ "USHETU DISTRICT COUNCIL",
      # (cop22_psnu == "Agago District" & country == "Uganda") ~ "AGAGO",
      # (cop22_psnu == "Apac District" & country == "Uganda") ~ "APAC",
      # (cop22_psnu == "Bukomansimbi District" & country == "Uganda") ~ "BUKOMANSIMBI",
      # (cop22_psnu == "Fort Portal City" & country == "Uganda") ~ "Fort Portal City",
      # (cop22_psnu == "Gomba District" & country == "Uganda") ~ "GOMBA",
      # (cop22_psnu == "Gulu District" & country == "Uganda") ~ "GULU",
      # (cop22_psnu == "Kalangala District" & country == "Uganda") ~ "KALANGALA",
      # (cop22_psnu == "Kampala District" & country == "Uganda") ~ "KAMPALA",
      # (cop22_psnu == "Kassanda District" & country == "Uganda") ~ "KASANDA",
      # (cop22_psnu == "Kayunga District" & country == "Uganda") ~ "KAYUNGA",
      # (cop22_psnu == "Kwania District" & country == "Uganda") ~ "KWANIA",
      # (cop22_psnu == "Kyotera District" & country == "Uganda") ~ "KYOTERA",
      # (cop22_psnu == "Lira City" & country == "Uganda") ~ "LIRA", ###XXX
      # (cop22_psnu == "Lira District" & country == "Uganda") ~ "LIRA",
      # (cop22_psnu == "Luwero District" & country == "Uganda") ~ "LUWERO",
      # (cop22_psnu == "Lwengo District" & country == "Uganda") ~ "LWENGO",
      # (cop22_psnu == "Lyantonde District" & country == "Uganda") ~ "LYANTONDE",
      # (cop22_psnu == "Masaka City" & country == "Uganda") ~ "MASAKA", ###XXX
      # (cop22_psnu == "Masaka District" & country == "Uganda") ~ "MASAKA",
      # (cop22_psnu == "Mbarara City" & country == "Uganda") ~ "MASAKA", ###XXX
      # (cop22_psnu == "Mbarara District" & country == "Uganda") ~ "MBARARA",
      # (cop22_psnu == "Mityana District" & country == "Uganda") ~ "MITYANA",
      # (cop22_psnu == "Mubende District" & country == "Uganda") ~ "MUBENDE",
      # (cop22_psnu == "Mukono District" & country == "Uganda") ~ "MUKONO",
      # (cop22_psnu == "Omoro District" & country == "Uganda") ~ "OMORO",
      # (cop22_psnu == "Oyam District" & country == "Uganda") ~ "OYAM",
      # (cop22_psnu == "Rakai District" & country == "Uganda") ~ "RAKAI",
      # (cop22_psnu == "Sembabule District" & country == "Uganda") ~ "SEMBABULE",
      # (cop22_psnu == "Wakiso District" & country == "Uganda") ~ "WAKISO",
      (cop22_psnu == "Chingola District" & country == "Zambia") ~ "CHINGOLA",
      (cop22_psnu == "Chipata District" & country == "Zambia") ~ "CHIPATA",
      (cop22_psnu == "Kabwe District" & country == "Zambia") ~ "KABWE",
      (cop22_psnu == "Kafue District" & country == "Zambia") ~ "KAFUE",
      (cop22_psnu == "Kapiri-Mposhi District" & country == "Zambia") ~ "KAPIRI MPOSHI",
      (cop22_psnu == "Kasama District" & country == "Zambia") ~ "KASAMA",
      (cop22_psnu == "Kitwe District" & country == "Zambia") ~ "KITWE",
      (cop22_psnu == "Livingstone District" & country == "Zambia") ~ "LIVINGSTONE",
      (cop22_psnu == "Luanshya District" & country == "Zambia") ~ "LUANSHYA",
      (cop22_psnu == "Lusaka District" & country == "Zambia") ~ "LUSAKA",
      (cop22_psnu == "Mazabuka District" & country == "Zambia") ~ "MAZABUKA",
      (cop22_psnu == "Monze District" & country == "Zambia") ~ "MONZE",
      (cop22_psnu == "Mongu District" & country == "Zambia") ~ "MONGU",
      (cop22_psnu == "Mufulira District" & country == "Zambia") ~ "MUFULIRA",
      (cop22_psnu == "Namwala District" & country == "Zambia") ~ "NAMWALA",
      (cop22_psnu == "Ndola District" & country == "Zambia") ~ "NDOLA",
      (cop22_psnu == "Sesheke District" & country == "Zambia") ~ "SESHEKE",
      (cop22_psnu == "Beitbridge" & country == "Zimbabwe") ~ "BEITBRIDGE",
      (cop22_psnu == "Bubi" & country == "Zimbabwe") ~ "BUBI",
      (cop22_psnu == "Bulawayo" & country == "Zimbabwe") ~ "BULAWAYO",
      (cop22_psnu == "Bulilima" & country == "Zimbabwe") ~ "BULILIMA",
      (cop22_psnu == "Chipinge" & country == "Zimbabwe") ~ "CHIPINGE",
      (cop22_psnu == "Gwanda" & country == "Zimbabwe") ~ "GWANDA",
      (cop22_psnu == "Gweru" & country == "Zimbabwe") ~ "GWERU",
      (cop22_psnu == "Insiza" & country == "Zimbabwe") ~ "INSIZA",
      (cop22_psnu == "Lupane" & country == "Zimbabwe") ~ "LUPANE",
      (cop22_psnu == "Makoni" & country == "Zimbabwe") ~ "MAKONI",
      (cop22_psnu == "Mangwe" & country == "Zimbabwe") ~ "MANGWE",
      (cop22_psnu == "Matobo" & country == "Zimbabwe") ~ "MATOBO",
      (cop22_psnu == "Mazowe" & country == "Zimbabwe") ~ "MAZOWE",
      (cop22_psnu == "Mutare" & country == "Zimbabwe") ~ "MUTARE",
      (cop22_psnu == "Nkayi" & country == "Zimbabwe") ~ "NKAYI",
      (cop22_psnu == "Tsholotsho" & country == "Zimbabwe") ~ "TSHOLOTSHO"
    )
  )
  


my_data_historic_snu1s <-
  lapply(choices_recent_countries_all2_historic$path_names, function(the_file) {
    
    print(the_file)
    
    # read the data
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ),
                                 bucket = my_bucket,
                                 object = the_file)
    
    res <- data %>%
      filter(indicator == "AGYW_PREV" & sex == "Female" & (standardizeddisaggregate == "Age/Sex/Time/Complete" |standardizeddisaggregate == "Age/Sex/Time/Complete+")) %>%
      filter(numeratordenom != "D") %>% #New Jan 23, 2024. Was getting some duplication (e.g., Bobirwa 2022, 10-14 population)
      filter(country %in% c("Kenya",
                            "Lesotho")) %>%
      filter(!is.na(qtr4)) %>%
      group_by(country, snu1, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>%
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (snu1 == "Homa Bay County" & country == "Kenya") ~ "HOMA BAY",
      (snu1 == "Kiambu County" & country == "Kenya") ~ "KIAMBU",
      (snu1 == "Kisumu County" & country == "Kenya") ~ "KISUMU",
      (snu1 == "Migori County" & country == "Kenya") ~ "MIGORI",
      (snu1 == "Mombasa County" & country == "Kenya") ~ "MOMBASA",
      (snu1 == "Nairobi County" & country == "Kenya") ~ "NAIROBI CITY",
      (snu1 == "Siaya County" & country == "Kenya") ~ "SIAYA",
      (snu1 == "Berea" & country == "Lesotho") ~ "BEREA",
      (snu1 == "Mafeteng" & country == "Lesotho") ~ "MAFETENG",
      (snu1 == "Maseru" & country == "Lesotho") ~ "MASERU",
      (snu1 == "Mohale's Hoek" & country == "Lesotho") ~ "MOHALE'S HOEK"
    )
  )

my_data_historic <- rbind(my_data_historic_snu1s, 
                 my_data_historic_psnus)

choices_recent_countries_all_recent <- subset(choices, grepl("MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/", path_names))

choices_recent_countries_all2_recent <- subset(choices_recent_countries_all_recent, grepl("Botswana|Cote d'Ivoire|Haiti|Kenya|Lesotho|Malawi|Mozambique|Namibia|South Africa|Tanzania|Uganda|Zambia|Zimbabwe", path_names))


my_data_recent_psnus <- 
  lapply(choices_recent_countries_all2_recent$path_names, function(the_file) {
    
    print(the_file)
    
    # read the data
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ), 
                                 bucket = my_bucket,
                                 object = the_file)
    
    res <- data %>% 
      filter(indicator == "AGYW_PREV" & sex == "Female" & (standardizeddisaggregate == "Age/Sex/Time/Complete" |standardizeddisaggregate == "Age/Sex/Time/Complete+")) %>%
      filter(numeratordenom != "D") %>% #New Jan 23, 2024. Was getting some duplication (e.g., Bobirwa 2022, 10-14 population)
      filter(country %in% c("Botswana",
                            "Cote d'Ivoire",
                            "Haiti",
                            "Malawi",
                            "Mozambique",
                            "Namibia",
                            "South Africa",
                            "Tanzania",
                            "Uganda",
                            "Zambia",
                            "Zimbabwe")) %>%
      filter(!is.na(qtr4)) %>%
      #group_by(country, snu1, psnu, ageasentered, fiscal_year) %>% #Original
      group_by(country, snu1, cop22_psnu, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>% 
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (cop22_psnu == "Bobirwa District" & country == "Botswana") ~ "BOBONONG",
      (cop22_psnu == "Gaborone District" & country == "Botswana") ~ "GABORONE",
      (cop22_psnu == "Kgatleng District" & country == "Botswana") ~ "KGATLENG",
      (cop22_psnu == "Kweneng East District" & country == "Botswana") ~ "KWENENG EAST",
      (cop22_psnu == "Mahalapye District" & country == "Botswana") ~ "MAHALAPYE",
      (cop22_psnu == "North East District" & country == "Botswana") ~ "NORTH EAST",
      (cop22_psnu == "Serowe District" & country == "Botswana") ~ "SEROWE PALAPYE",
      (cop22_psnu == "Southern District" & country == "Botswana") ~ "SOUTHERN",
      (cop22_psnu == "Abobo-Est" & country == "Cote d'Ivoire") ~ "Abobo-Est",
      (cop22_psnu == "Cocody-Bingerville" & country == "Cote d'Ivoire") ~ "Cocody-Bingerville",
      (cop22_psnu == "Daloa" & country == "Cote d'Ivoire") ~ "Daloa",
      (cop22_psnu == "Man" & country == "Cote d'Ivoire") ~ "Man",
      (cop22_psnu == "Cap-Haïtien" & country == "Haiti") ~ "Cap-Haïtien",
      (cop22_psnu == "Dessalines" & country == "Haiti") ~ "Dessalines",
      (cop22_psnu == "Port-au-Prince" & country == "Haiti") ~ "Port-au-Prince",
      (cop22_psnu == "Saint-Marc" & country == "Haiti") ~ "Saint-Marc",
      (cop22_psnu == "Blantyre District" & country == "Malawi") ~ "BLANTYRE", #cop22_psnu for Malawi
      (cop22_psnu == "Chiradzulu District" & country == "Malawi") ~ "CHIRADZULU", #cop22_psnu for Malawi
      (cop22_psnu == "Machinga District" & country == "Malawi") ~ "MACHINGA", #cop22_psnu for Malawi
      (cop22_psnu == "Phalombe District" & country == "Malawi") ~ "PHALOMBE", #cop22_psnu for Malawi
      (cop22_psnu == "Zomba District" & country == "Malawi") ~ "ZOMBA", #cop22_psnu for Malawi
      (cop22_psnu == "Boane" & country == "Mozambique") ~ "BOANE",
      (cop22_psnu == "Caia" & country == "Mozambique") ~ "CAIA",
      (cop22_psnu == "Beira" & country == "Mozambique") ~ "CIDADE DA BEIRA",
      (cop22_psnu == "Chimoio" & country == "Mozambique") ~ "CIDADE DE CHIMOIO",
      (cop22_psnu == "Matola" & country == "Mozambique") ~ "CIDADE DA MATOLA",
      (cop22_psnu == "Maxixe" & country == "Mozambique") ~ "CIDADE DA MAXIXE",
      (cop22_psnu == "Nampula" & country == "Mozambique") ~ "CIDADE DE NAMPULA",
      (cop22_psnu == "Pemba" & country == "Mozambique") ~ "CIDADE DE PEMBA",
      (cop22_psnu == "Xai-Xai" & country == "Mozambique") ~ "CIDADE DE XAI-XAI",
      (cop22_psnu == "Chokwe" & country == "Mozambique") ~ "CHÓKWÈ",
      (cop22_psnu == "Chonguene" & country == "Mozambique") ~ "CHONGOENE",
      (cop22_psnu == "Erati" & country == "Mozambique") ~ "ERÁTI",
      (cop22_psnu == "Gile" & country == "Mozambique") ~ "GILÉ",
      (cop22_psnu == "Guija" & country == "Mozambique") ~ "GUIJÁ",
      (cop22_psnu == "Ile" & country == "Mozambique") ~ "ILE",
      (cop22_psnu == "Inhassunge" & country == "Mozambique") ~ "INHASSUNGE",
      (cop22_psnu == "Limpopo" & country == "Mozambique") ~ "LIMPOPO",
      (cop22_psnu == "Lugela" & country == "Mozambique") ~ "LUGELA",
      (cop22_psnu == "Maganja Da Costa" & country == "Mozambique") ~ "MAGANJA DA COSTA",
      (cop22_psnu == "Magude" & country == "Mozambique") ~ "MAGUDE",
      (cop22_psnu == "Manhiça" & country == "Mozambique") ~ "MANHIÇA",
      (cop22_psnu == "Marracuene" & country == "Mozambique") ~ "MARRACUENE",
      (cop22_psnu == "Matutuine" & country == "Mozambique") ~ "MATUTUINE",
      (cop22_psnu == "Milange" & country == "Mozambique") ~ "MILANGE",
      (cop22_psnu == "Moamba" & country == "Mozambique") ~ "MOAMBA",
      (cop22_psnu == "Mocuba" & country == "Mozambique") ~ "MOCUBA",
      (cop22_psnu == "Mocubela" & country == "Mozambique") ~ "MOCUBELA",
      (cop22_psnu == "Namaacha" & country == "Mozambique") ~ "NAMAACHA",
      (cop22_psnu == "Namacurra" & country == "Mozambique") ~ "NAMACURRA",
      (cop22_psnu == "Nicoadala" & country == "Mozambique") ~ "NICOADALA",
      (cop22_psnu == "Pebane" & country == "Mozambique") ~ "PEBANE",
      (cop22_psnu == "Quelimane" & country == "Mozambique") ~ "QUELIMANE",
      (cop22_psnu == "Katima Mulilo" & country == "Namibia") ~ "KATIMA MULILO",
      (cop22_psnu == "Andara" & country == "Namibia") ~ "MUKWE",
      (cop22_psnu == "Nankudu" & country == "Namibia") ~ "NANKUDU",
      (cop22_psnu == "Nyangana" & country == "Namibia") ~ "NDIYONA",
      (cop22_psnu == "Omuthiya" & country == "Namibia") ~ "OMUTHIYAGWIIPUNDI",
      (cop22_psnu == "Onandjokwe" & country == "Namibia") ~ "ONIIPA",
      (cop22_psnu == "Oshakati" & country == "Namibia") ~ "OSHAKATI",
      (cop22_psnu == "Rundu" & country == "Namibia") ~ "RUNDU",
      (cop22_psnu == "Tsumeb" & country == "Namibia") ~ "TSUMEB",
      (cop22_psnu == "Windhoek" & country == "Namibia") ~ "WINDHOEK",
      (cop22_psnu == "ec Alfred Nzo District Municipality" & country == "South Africa") ~ "ALFRED NZO", #cop22_psnu for South Africa
      (cop22_psnu == "nw Bojanala Platinum District Municipality" & country == "South Africa") ~ "BOJANALA", #cop22_psnu for South Africa
      (cop22_psnu == "ec Buffalo City Metropolitan Municipality" & country == "South Africa") ~ "BUFFALO CITY", #cop22_psnu for South Africa
      (cop22_psnu == "lp Capricorn District Municipality" & country == "South Africa") ~ "CAPRICORN", #cop22_psnu for South Africa
      (cop22_psnu == "wc City of Cape Town Metropolitan Municipality" & country == "South Africa") ~ "CITY OF CAPE TOWN", #cop22_psnu for South Africa
      (cop22_psnu == "gp City of Johannesburg Metropolitan Municipality" & country == "South Africa") ~ "CITY OF JOHANNESBURG", #cop22_psnu for South Africa
      (cop22_psnu == "gp City of Tshwane Metropolitan Municipality" & country == "South Africa") ~ "CITY OF TSHWANE", #cop22_psnu for South Africa
      (cop22_psnu == "nw Dr Kenneth Kaunda District Municipality" & country == "South Africa") ~ "DOCTOR KENNETH KAUNDA", #cop22_psnu for South Africa
      (cop22_psnu == "mp Ehlanzeni District Municipality" & country == "South Africa") ~ "EHLANZENI", #cop22_psnu for South Africa
      (cop22_psnu == "gp Ekurhuleni Metropolitan Municipality" & country == "South Africa") ~ "EKURHULENI", #cop22_psnu for South Africa
      (cop22_psnu == "kz eThekwini Metropolitan Municipality" & country == "South Africa") ~ "ETHEKWINI", #cop22_psnu for South Africa
      (cop22_psnu == "mp Gert Sibande District Municipality" & country == "South Africa") ~ "GERT SIBANDE", #cop22_psnu for South Africa
      (cop22_psnu == "fs Lejweleputswa District Municipality" & country == "South Africa") ~ "LEJWELEPUTSWA", #cop22_psnu for South Africa
      (cop22_psnu == "lp Mopani District Municipality" & country == "South Africa") ~ "MOPANI", #cop22_psnu for South Africa
      (cop22_psnu == "nw Ngaka Modiri Molema District Municipality" & country == "South Africa") ~ "NGAKA MODIRI MOLEMA", #cop22_psnu for South Africa
      (cop22_psnu == "mp Nkangala District Municipality" & country == "South Africa") ~ "NKANGALA", #cop22_psnu for South Africa
      (cop22_psnu == "ec Oliver Tambo District Municipality" & country == "South Africa") ~ "OR TAMBO", #cop22_psnu for South Africa
      (cop22_psnu == "gp Sedibeng District Municipality" & country == "South Africa") ~ "SEDIBENG", #cop22_psnu for South Africa
      (cop22_psnu == "fs Thabo Mofutsanyane District Municipality" & country == "South Africa") ~ "THABO MOFUTSANYANE", #cop22_psnu for South Africa
      (cop22_psnu == "kz Ugu District Municipality" & country == "South Africa") ~ "UGU", #cop22_psnu for South Africa
      (cop22_psnu == "kz uMgungundlovu District Municipality" & country == "South Africa") ~ "UMGUNGUNDLOVU", #cop22_psnu for South Africa
      (cop22_psnu == "kz Uthukela District Municipality" & country == "South Africa") ~ "UTHUKELA", #cop22_psnu for South Africa
      (cop22_psnu == "kz King Cetshwayo District Municipality" & country == "South Africa") ~ "UTHUNGULU", #cop22_psnu for South Africa
      (cop22_psnu == "kz Zululand District Municipality" & country == "South Africa") ~ "ZULULAND", #cop22_psnu for South Africa
      (cop22_psnu == "Bukoba MC" & country == "Tanzania") ~ "BUKOBA MUNICIPAL COUNCIL",
      (cop22_psnu == "Iringa MC" & country == "Tanzania") ~ "IRINGA MUNICIPAL COUNCIL",
      (cop22_psnu == "Kahama TC" & country == "Tanzania") ~ "KAHAMA TOWN COUNCIL",
      (cop22_psnu == "Kyela DC" & country == "Tanzania") ~ "KYELA DISTRICT COUNCIL",
      (cop22_psnu == "Msalala DC" & country == "Tanzania") ~ "MSALALA",
      (cop22_psnu == "Mbarali DC" & country == "Tanzania") ~ "MBARALI DISTRICT COUNCIL",
      (cop22_psnu == "Mbeya CC" & country == "Tanzania") ~ "MBEYA CITY COUNCIL",
      (cop22_psnu == "Mufindi DC" & country == "Tanzania") ~ "MUFINDI DISTRICT COUNCIL",
      (cop22_psnu == "Muleba DC" & country == "Tanzania") ~ "MULEBA DISTRIC COUNCIL",
      (cop22_psnu == "Nyamagana MC" & country == "Tanzania") ~ "NYAMAGANA MUNICIPAL COUNCIL",
      (cop22_psnu == "Shinyanga DC" & country == "Tanzania") ~ "SHINYANGA DISTRICT COUNCIL",
      (cop22_psnu == "Shinyanga MC" & country == "Tanzania") ~ "SHINYANGA MUNICIPAL COUNCIL",
      (cop22_psnu == "Temeke MC" & country == "Tanzania") ~ "TEMEKE MUNICIPAL COUNCIL",
      (cop22_psnu == "Tunduma TC" & country == "Tanzania") ~ "TUNDUMA TOWN COUNCIL",
      (cop22_psnu == "Ushetu DC" & country == "Tanzania") ~ "USHETU DISTRICT COUNCIL",
      (cop22_psnu == "Agago District" & country == "Uganda") ~ "AGAGO",
      (cop22_psnu == "Apac District" & country == "Uganda") ~ "APAC",
      (cop22_psnu == "Bukomansimbi District" & country == "Uganda") ~ "BUKOMANSIMBI",
      (cop22_psnu == "Gomba District" & country == "Uganda") ~ "GOMBA",
      (cop22_psnu == "Gulu District" & country == "Uganda") ~ "GULU",
      (cop22_psnu == "Kalangala District" & country == "Uganda") ~ "KALANGALA",
      (cop22_psnu == "Kampala District" & country == "Uganda") ~ "KAMPALA",
      (cop22_psnu == "Kassanda District" & country == "Uganda") ~ "KASANDA",
      (cop22_psnu == "Kayunga District" & country == "Uganda") ~ "KAYUNGA",
      (cop22_psnu == "Kwania District" & country == "Uganda") ~ "KWANIA",
      (cop22_psnu == "Kyotera District" & country == "Uganda") ~ "KYOTERA",
      (cop22_psnu == "Lira District" & country == "Uganda") ~ "LIRA",
      (cop22_psnu == "Luwero District" & country == "Uganda") ~ "LUWERO",
      (cop22_psnu == "Lwengo District" & country == "Uganda") ~ "LWENGO",
      (cop22_psnu == "Lyantonde District" & country == "Uganda") ~ "LYANTONDE",
      (cop22_psnu == "Masaka District" & country == "Uganda") ~ "MASAKA",
      (cop22_psnu == "Mbarara District" & country == "Uganda") ~ "MBARARA",
      (cop22_psnu == "Mityana District" & country == "Uganda") ~ "MITYANA",
      (cop22_psnu == "Mubende District" & country == "Uganda") ~ "MUBENDE",
      (cop22_psnu == "Mukono District" & country == "Uganda") ~ "MUKONO",
      (cop22_psnu == "Omoro District" & country == "Uganda") ~ "OMORO",
      (cop22_psnu == "Oyam District" & country == "Uganda") ~ "OYAM",
      (cop22_psnu == "Rakai District" & country == "Uganda") ~ "RAKAI",
      (cop22_psnu == "Sembabule District" & country == "Uganda") ~ "SEMBABULE",
      (cop22_psnu == "Wakiso District" & country == "Uganda") ~ "WAKISO",
      (cop22_psnu == "Chingola District" & country == "Zambia") ~ "CHINGOLA",
      (cop22_psnu == "Chipata District" & country == "Zambia") ~ "CHIPATA",
      (cop22_psnu == "Kabwe District" & country == "Zambia") ~ "KABWE",
      (cop22_psnu == "Kafue District" & country == "Zambia") ~ "KAFUE",
      (cop22_psnu == "Kapiri-Mposhi District" & country == "Zambia") ~ "KAPIRI MPOSHI",
      (cop22_psnu == "Kasama District" & country == "Zambia") ~ "KASAMA",
      (cop22_psnu == "Kitwe District" & country == "Zambia") ~ "KITWE",
      (cop22_psnu == "Livingstone District" & country == "Zambia") ~ "LIVINGSTONE",
      (cop22_psnu == "Luanshya District" & country == "Zambia") ~ "LUANSHYA",
      (cop22_psnu == "Lusaka District" & country == "Zambia") ~ "LUSAKA",
      (cop22_psnu == "Mazabuka District" & country == "Zambia") ~ "MAZABUKA",
      (cop22_psnu == "Monze District" & country == "Zambia") ~ "MONZE",
      (cop22_psnu == "Mongu District" & country == "Zambia") ~ "MONGU",
      (cop22_psnu == "Mufulira District" & country == "Zambia") ~ "MUFULIRA",
      (cop22_psnu == "Namwala District" & country == "Zambia") ~ "NAMWALA",
      (cop22_psnu == "Ndola District" & country == "Zambia") ~ "NDOLA",
      (cop22_psnu == "Sesheke District" & country == "Zambia") ~ "SESHEKE",
      (cop22_psnu == "Beitbridge" & country == "Zimbabwe") ~ "BEITBRIDGE",
      (cop22_psnu == "Bubi" & country == "Zimbabwe") ~ "BUBI",
      (cop22_psnu == "Bulawayo" & country == "Zimbabwe") ~ "BULAWAYO",
      (cop22_psnu == "Bulilima" & country == "Zimbabwe") ~ "BULILIMA",
      (cop22_psnu == "Chipinge" & country == "Zimbabwe") ~ "CHIPINGE",
      (cop22_psnu == "Gwanda" & country == "Zimbabwe") ~ "GWANDA",
      (cop22_psnu == "Gweru" & country == "Zimbabwe") ~ "GWERU",
      (cop22_psnu == "Insiza" & country == "Zimbabwe") ~ "INSIZA",
      (cop22_psnu == "Lupane" & country == "Zimbabwe") ~ "LUPANE",
      (cop22_psnu == "Makoni" & country == "Zimbabwe") ~ "MAKONI",
      (cop22_psnu == "Mangwe" & country == "Zimbabwe") ~ "MANGWE",
      (cop22_psnu == "Matobo" & country == "Zimbabwe") ~ "MATOBO",
      (cop22_psnu == "Mazowe" & country == "Zimbabwe") ~ "MAZOWE",
      (cop22_psnu == "Mutare" & country == "Zimbabwe") ~ "MUTARE",
      (cop22_psnu == "Nkayi" & country == "Zimbabwe") ~ "NKAYI",
      (cop22_psnu == "Tsholotsho" & country == "Zimbabwe") ~ "TSHOLOTSHO"
    )
  )


my_data_recent_snu1s <- 
  lapply(choices_recent_countries_all2_recent$path_names, function(the_file) {
    
    print(the_file)
    
    # read the data
    data <- aws.s3::s3read_using(FUN = readr::read_delim, "|", escape_double = FALSE,
                                 trim_ws = TRUE, col_types = readr::cols(.default = readr::col_character()
                                 ), 
                                 bucket = my_bucket,
                                 object = the_file)
    
    res <- data %>% 
      filter(indicator == "AGYW_PREV" & sex == "Female" & (standardizeddisaggregate == "Age/Sex/Time/Complete" |standardizeddisaggregate == "Age/Sex/Time/Complete+")) %>%
      filter(numeratordenom != "D") %>% #New Jan 23, 2024. Was getting some duplication (e.g., Bobirwa 2022, 10-14 population)
      filter(country %in% c("Kenya",
                            "Lesotho")) %>%
      filter(!is.na(qtr4)) %>%
      group_by(country, snu1, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>% 
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (snu1 == "Homa Bay County" & country == "Kenya") ~ "HOMA BAY",
      (snu1 == "Kiambu County" & country == "Kenya") ~ "KIAMBU",
      (snu1 == "Kisumu County" & country == "Kenya") ~ "KISUMU",
      (snu1 == "Migori County" & country == "Kenya") ~ "MIGORI",
      (snu1 == "Mombasa County" & country == "Kenya") ~ "MOMBASA",
      (snu1 == "Nairobi County" & country == "Kenya") ~ "NAIROBI CITY",
      (snu1 == "Siaya County" & country == "Kenya") ~ "SIAYA",
      (snu1 == "Berea" & country == "Lesotho") ~ "BEREA",
      (snu1 == "Mafeteng" & country == "Lesotho") ~ "MAFETENG",
      (snu1 == "Maseru" & country == "Lesotho") ~ "MASERU",
      (snu1 == "Mohale's Hoek" & country == "Lesotho") ~ "MOHALE'S HOEK"
    )
  )

my_data_recent <- rbind(my_data_recent_psnus,
                        my_data_recent_snu1s)



my_data <- rbind(my_data_historic, 
                 my_data_recent)

my_data <- my_data %>%
  mutate(
    AREA_NAME = case_when(
      (is.na(cop22_psnu)) ~ as.character(snu1),
      TRUE ~ as.character(cop22_psnu)
    )
  ) %>%
  rename(psnu = cop22_psnu)


### A few basic data checks, if entries are found with no Q4 data, no cumulative data, but Q2 data...
### or if found with no data at all, pass on to data managers for QC

# noQ4 <- my_data %>% 
#   filter(is.na(qtr4))
# 
# noQ4orCum <- my_data %>% 
#   filter((is.na(qtr4)&is.na(cumulative)))
# 
# Q2only <- my_data %>% 
#   filter(is.na(qtr4)) %>%
#   filter(is.na(cumulative)) %>%
#   filter(!is.na(qtr2))
# 
# NoData <- my_data %>% 
#   filter(is.na(qtr4)) %>%
#   filter(is.na(cumulative)) %>%
#   filter(is.na(qtr2))

# Q2only %>% 
#   write.csv(file = 'q2only.csv')

rm(choices)
rm(cleaned_choices)
rm(my_data_historic)
rm(my_data_recent)
rm(choices_recent_countries_all_historic)
rm(choices_recent_countries_all_recent)
rm(choices_recent_countries_all2_historic)
rm(choices_recent_countries_all2_recent)
rm(my_data_qs_check)

###

# writing --------------
# write to dreams workspace

print("writing to system dreams...")


previous <- s3read_using(FUN = read.csv, #Import the previous version
                          bucket = Sys.getenv("WRITE_S3"),
                          object = "system_dreams_saturation/AGYW_PREVbyCountry.csv")


s3write_using(previous, FUN = write.csv, #Backup the previous version
              bucket = Sys.getenv("WRITE_S3"),
              object = "system_dreams_saturation/AGYW_PREVbyCountry_previous.csv"
)


s3write_using(my_data, FUN = write.csv, #Overwrite the working version
              bucket = Sys.getenv("WRITE_S3"),
              object = "system_dreams_saturation/AGYW_PREVbyCountry.csv"
)


my_data_DSNUs <- read.csv("preprocessing/data/DSNUsfromDATIMDataVisualizer.csv")

my_data_DSNUs2 <- my_data_DSNUs %>%
  rename(`2018` = "X2018",
         `2019` = "X2019",
         `2020` = "X2020",
         `2021` = "X2021",
         `2022` = "X2022",
         `2023` = "X2023") %>%
  pivot_longer(
    cols = `2018`:`2023`,
    names_to = "fiscal_year",
    values_to = "AGYW_PREV"
  )

my_data_DSNUs2$JOIN_NAME <- my_data_DSNUs2$snu1
my_data_DSNUs2$psnu <- "NA"
my_data_DSNUs2$AREA_NAME <- my_data_DSNUs2$snu1
my_data_DSNUs2$fiscal_year <- my_data_DSNUs2$fiscal_year %>%
  as.double()

my_data_DSNUs3 <- my_data_DSNUs2 %>%
  mutate(
    ageasentered = case_when(
    (ageasentered == "10 to 14") ~ "10-14",
    (ageasentered == "15 to 19") ~ "15-19",
    (ageasentered == "20 to 24") ~ "20-24",
    (ageasentered == "25 to 29") ~ "25-29",
    TRUE ~ as.character(ageasentered)))


my_data_test <- rbind(my_data,
                   my_data_DSNUs3)

my_data_test2 <- my_data_test %>%
  mutate(
    JOIN_NAME = case_when(
      (country == "Uganda") ~ as.character(AREA_NAME),
      TRUE ~ as.character(JOIN_NAME)
    )
  )

s3write_using(my_data_test2, FUN = write.csv, #Overwrite the working version
              bucket = Sys.getenv("WRITE_S3"),
              object = "system_dreams_saturation/AGYW_PREVbyCountry_test.csv"
)

# test reading from write bucket --------
# read from dreams workspace

# print("reading dreams data...")
# read_testing <- s3read_using(FUN = read.csv,
#              bucket = Sys.getenv("WRITE_S3"),
#              object = "system_dreams_saturation/AGYW_PREVbyCountry.csv") %>%
#   as.data.frame()

#rm(read_testing)





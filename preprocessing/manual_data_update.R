library(aws.s3)
library(readxl)
library(paws)
library(jsonlite)

# connect ---------------------------------------

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

choices_recent_countries_all2_historic <- subset(choices_recent_countries_all_historic, grepl("Botswana|Cote d'Ivoire|Eswatini|Haiti|Kenya|Lesotho|Malawi|Mozambique|Namibia|Rwanda|South Africa|Tanzania|Uganda|Zambia|Zimbabwe", path_names))

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
      group_by(country, snu1, psnu, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))

  }) %>%
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (psnu == "Bobirwa District" & country == "Botswana") ~ "BOBONONG",
      (psnu == "Mahalapye District" & country == "Botswana") ~ "MAHALAPYE",
      (psnu == "Serowe District" & country == "Botswana") ~ "SEROWE PALAPYE",
      (psnu == "Kgatleng District" & country == "Botswana") ~ "KGATLENG",
      (psnu == "Kweneng East District" & country == "Botswana") ~ "KWENENG EAST",
      (psnu == "North East District" & country == "Botswana") ~ "NORTH EAST",
      (psnu == "Gaborone District" & country == "Botswana") ~ "GABORONE",
      (psnu == "Southern District" & country == "Botswana") ~ "SOUTHERN",
      (psnu == "Abobo-Est" & country == "Cote d'Ivoire") ~ "Abobo-Est",
      (psnu == "Cocody-Bingerville" & country == "Cote d'Ivoire") ~ "Cocody-Bingerville",
      (psnu == "Daloa" & country == "Cote d'Ivoire") ~ "Daloa",
      (psnu == "Man" & country == "Cote d'Ivoire") ~ "Man",
      (psnu == "Dessalines" & country == "Haiti") ~ "Dessalines",
      (psnu == "Saint-Marc" & country == "Haiti") ~ "Saint-Marc",
      (psnu == "Cap-Haïtien" & country == "Haiti") ~ "Cap-Haïtien",
      (psnu == "Port-au-Prince" & country == "Haiti") ~ "Port-au-Prince",
      (psnu == "Blantyre District" & country == "Malawi") ~ "BLANTYRE", #psnu for Malawi
      (psnu == "Machinga District" & country == "Malawi") ~ "MACHINGA", #psnu for Malawi
      (psnu == "Zomba District" & country == "Malawi") ~ "ZOMBA", #psnu for Malawi
      (psnu == "Pemba" & country == "Mozambique") ~ "CIDADE DE PEMBA",
      (psnu == "Chokwe" & country == "Mozambique") ~ "CHÓKWÈ",
      (psnu == "Chonguene" & country == "Mozambique") ~ "CHONGOENE",
      (psnu == "Guija" & country == "Mozambique") ~ "GUIJÁ",
      (psnu == "Limpopo" & country == "Mozambique") ~ "LIMPOPO",
      (psnu == "Xai-Xai" & country == "Mozambique") ~ "CIDADE DE XAI-XAI",
      (psnu == "Maxixe" & country == "Mozambique") ~ "CIDADE DA MAXIXE",
      (psnu == "Chimoio" & country == "Mozambique") ~ "CIDADE DE CHIMOIO",
      (psnu == "Boane" & country == "Mozambique") ~ "BOANE",
      (psnu == "Magude" & country == "Mozambique") ~ "MAGUDE",
      (psnu == "Manhiça" & country == "Mozambique") ~ "MANHIÇA",
      (psnu == "Marracuene" & country == "Mozambique") ~ "MARRACUENE",
      (psnu == "Matola" & country == "Mozambique") ~ "CIDADE DA MATOLA",
      (psnu == "Matutuine" & country == "Mozambique") ~ "MATUTUINE",
      (psnu == "Moamba" & country == "Mozambique") ~ "MOAMBA",
      (psnu == "Namaacha" & country == "Mozambique") ~ "NAMAACHA",
      (psnu == "Erati" & country == "Mozambique") ~ "ERÁTI",
      (psnu == "Nampula" & country == "Mozambique") ~ "CIDADE DE NAMPULA",
      (psnu == "Beira" & country == "Mozambique") ~ "CIDADE DA BEIRA",
      (psnu == "Caia" & country == "Mozambique") ~ "CAIA",
      (psnu == "Gile" & country == "Mozambique") ~ "GILÉ",
      (psnu == "Ile" & country == "Mozambique") ~ "ILE",
      (psnu == "Inhassunge" & country == "Mozambique") ~ "INHASSUNGE",
      (psnu == "Lugela" & country == "Mozambique") ~ "LUGELA",
      (psnu == "Maganja Da Costa" & country == "Mozambique") ~ "MAGANJA DA COSTA",
      (psnu == "Milange" & country == "Mozambique") ~ "MILANGE",
      (psnu == "Mocuba" & country == "Mozambique") ~ "MOCUBA",
      (psnu == "Mocubela" & country == "Mozambique") ~ "MOCUBELA",
      (psnu == "Namacurra" & country == "Mozambique") ~ "NAMACURRA",
      (psnu == "Nicoadala" & country == "Mozambique") ~ "NICOADALA",
      (psnu == "Pebane" & country == "Mozambique") ~ "PEBANE",
      (psnu == "Quelimane" & country == "Mozambique") ~ "QUELIMANE",
      (psnu == "Andara" & country == "Namibia") ~ "MUKWE",
      (psnu == "Nyangana" & country == "Namibia") ~ "NDIYONA",
      (psnu == "Rundu" & country == "Namibia") ~ "RUNDU",
      (psnu == "Windhoek" & country == "Namibia") ~ "WINDHOEK",
      (psnu == "Oshakati" & country == "Namibia") ~ "OSHAKATI",
      (psnu == "Omuthiya" & country == "Namibia") ~ "OMUTHIYAGWIIPUNDI",
      (psnu == "Onandjokwe" & country == "Namibia") ~ "ONIIPA",
      (psnu == "Tsumeb" & country == "Namibia") ~ "TSUMEB",
      (psnu == "Katima Mulilo" & country == "Namibia") ~ "KATIMA MULILO",
      (psnu == "ec Alfred Nzo District Municipality" & country == "South Africa") ~ "ALFRED NZO", #psnu for South Africa
      (psnu == "nw Bojanala Platinum District Municipality" & country == "South Africa") ~ "BOJANALA", #psnu for South Africa
      (psnu == "ec Buffalo City Metropolitan Municipality" & country == "South Africa") ~ "BUFFALO CITY", #psnu for South Africa
      (psnu == "lp Capricorn District Municipality" & country == "South Africa") ~ "CAPRICORN", #psnu for South Africa
      (psnu == "wc City of Cape Town Metropolitan Municipality" & country == "South Africa") ~ "CITY OF CAPE TOWN", #psnu for South Africa
      (psnu == "gp City of Johannesburg Metropolitan Municipality" & country == "South Africa") ~ "CITY OF JOHANNESBURG", #psnu for South Africa
      (psnu == "gp City of Tshwane Metropolitan Municipality" & country == "South Africa") ~ "CITY OF TSHWANE", #psnu for South Africa
      (psnu == "nw Dr Kenneth Kaunda District Municipality" & country == "South Africa") ~ "DOCTOR KENNETH KAUNDA", #psnu for South Africa
      (psnu == "mp Ehlanzeni District Municipality" & country == "South Africa") ~ "EHLANZENI", #psnu for South Africa
      (psnu == "gp Ekurhuleni Metropolitan Municipality" & country == "South Africa") ~ "EKURHULENI", #psnu for South Africa
      (psnu == "kz eThekwini Metropolitan Municipality" & country == "South Africa") ~ "ETHEKWINI", #psnu for South Africa
      (psnu == "mp Gert Sibande District Municipality" & country == "South Africa") ~ "GERT SIBANDE", #psnu for South Africa
      (psnu == "fs Lejweleputswa District Municipality" & country == "South Africa") ~ "LEJWELEPUTSWA", #psnu for South Africa
      (psnu == "lp Mopani District Municipality" & country == "South Africa") ~ "MOPANI", #psnu for South Africa
      (psnu == "nw Ngaka Modiri Molema District Municipality" & country == "South Africa") ~ "NGAKA MODIRI MOLEMA", #psnu for South Africa
      (psnu == "mp Nkangala District Municipality" & country == "South Africa") ~ "NKANGALA", #psnu for South Africa
      (psnu == "ec Oliver Tambo District Municipality" & country == "South Africa") ~ "OR TAMBO", #psnu for South Africa
      (psnu == "gp Sedibeng District Municipality" & country == "South Africa") ~ "SEDIBENG", #psnu for South Africa
      (psnu == "fs Thabo Mofutsanyane District Municipality" & country == "South Africa") ~ "THABO MOFUTSANYANE", #psnu for South Africa
      (psnu == "kz Ugu District Municipality" & country == "South Africa") ~ "UGU", #psnu for South Africa
      (psnu == "kz uMgungundlovu District Municipality" & country == "South Africa") ~ "UMGUNGUNDLOVU", #psnu for South Africa
      (psnu == "kz Uthukela District Municipality" & country == "South Africa") ~ "UTHUKELA", #psnu for South Africa
      (psnu == "kz King Cetshwayo District Municipality" & country == "South Africa") ~ "UTHUNGULU", #psnu for South Africa
      (psnu == "kz Zululand District Municipality" & country == "South Africa") ~ "ZULULAND", #psnu for South Africa
      (psnu == "Temeke MC" & country == "Tanzania") ~ "TEMEKE MUNICIPAL COUNCIL",
      (psnu == "Mufindi DC" & country == "Tanzania") ~ "MUFINDI DISTRICT COUNCIL",
      (psnu == "Muleba DC" & country == "Tanzania") ~ "MULEBA DISTRIC COUNCIL",
      (psnu == "Kyela DC" & country == "Tanzania") ~ "KYELA DISTRICT COUNCIL",
      (psnu == "Mbarali DC" & country == "Tanzania") ~ "MBARALI DISTRICT COUNCIL",
      (psnu == "Mbeya CC" & country == "Tanzania") ~ "MBEYA CITY COUNCIL",
      (psnu == "Nyamagana MC" & country == "Tanzania") ~ "NYAMAGANA MUNICIPAL COUNCIL",
      (psnu == "Kahama TC" & country == "Tanzania") ~ "KAHAMA TOWN COUNCIL",
      (psnu == "Msalala DC" & country == "Tanzania") ~ "MSALALA",
      (psnu == "Shinyanga DC" & country == "Tanzania") ~ "SHINYANGA DISTRICT COUNCIL",
      (psnu == "Shinyanga MC" & country == "Tanzania") ~ "SHINYANGA MUNICIPAL COUNCIL",
      (psnu == "Ushetu DC" & country == "Tanzania") ~ "USHETU DISTRICT COUNCIL",
      (psnu == "Bukomansimbi District" & country == "Uganda") ~ "BUKOMANSIMBI",
      (psnu == "Gomba District" & country == "Uganda") ~ "GOMBA",
      (psnu == "Kalangala District" & country == "Uganda") ~ "KALANGALA",
      (psnu == "Kyotera District" & country == "Uganda") ~ "KYOTERA",
      (psnu == "Lwengo District" & country == "Uganda") ~ "LWENGO",
      (psnu == "Lyantonde District" & country == "Uganda") ~ "LYANTONDE",
      (psnu == "Masaka District" & country == "Uganda") ~ "MASAKA",
      (psnu == "Rakai District" & country == "Uganda") ~ "RAKAI",
      (psnu == "Sembabule District" & country == "Uganda") ~ "SEMBABULE",
      (psnu == "Wakiso District" & country == "Uganda") ~ "WAKISO",
      (psnu == "Kassanda District" & country == "Uganda") ~ "KASANDA",
      (psnu == "Luwero District" & country == "Uganda") ~ "LUWERO",
      (psnu == "Mityana District" & country == "Uganda") ~ "MITYANA",
      (psnu == "Mubende District" & country == "Uganda") ~ "MUBENDE",
      (psnu == "Mukono District" & country == "Uganda") ~ "MUKONO",
      (psnu == "Kampala District" & country == "Uganda") ~ "KAMPALA",
      (psnu == "Agago District" & country == "Uganda") ~ "AGAGO",
      (psnu == "Apac District" & country == "Uganda") ~ "APAC",
      (psnu == "Gulu District" & country == "Uganda") ~ "GULU",
      (psnu == "Kwania District" & country == "Uganda") ~ "KWANIA",
      (psnu == "Lira District" & country == "Uganda") ~ "LIRA",
      (psnu == "Omoro District" & country == "Uganda") ~ "OMORO",
      (psnu == "Oyam District" & country == "Uganda") ~ "OYAM",
      (psnu == "Mbarara District" & country == "Uganda") ~ "MBARARA",
      (psnu == "Kabwe District" & country == "Zambia") ~ "KABWE",
      (psnu == "Kapiri-Mposhi District" & country == "Zambia") ~ "KAPIRI MPOSHI",
      (psnu == "Chingola District" & country == "Zambia") ~ "CHINGOLA",
      (psnu == "Kitwe District" & country == "Zambia") ~ "KITWE",
      (psnu == "Luanshya District" & country == "Zambia") ~ "LUANSHYA",
      (psnu == "Mufulira District" & country == "Zambia") ~ "MUFULIRA",
      (psnu == "Ndola District" & country == "Zambia") ~ "NDOLA",
      (psnu == "Chipata District" & country == "Zambia") ~ "CHIPATA",
      (psnu == "Lusaka District" & country == "Zambia") ~ "LUSAKA",
      (psnu == "Kasama District" & country == "Zambia") ~ "KASAMA",
      (psnu == "Livingstone District" & country == "Zambia") ~ "LIVINGSTONE",
      (psnu == "Mazabuka District" & country == "Zambia") ~ "MAZABUKA",
      (psnu == "Monze District" & country == "Zambia") ~ "MONZE",
      (psnu == "Mongu District" & country == "Zambia") ~ "MONGU",
      (psnu == "Bulawayo" & country == "Zimbabwe") ~ "BULAWAYO",
      (psnu == "Chipinge" & country == "Zimbabwe") ~ "CHIPINGE",
      (psnu == "Makoni" & country == "Zimbabwe") ~ "MAKONI",
      (psnu == "Mutare" & country == "Zimbabwe") ~ "MUTARE",
      (psnu == "Mazowe" & country == "Zimbabwe") ~ "MAZOWE",
      (psnu == "Bubi" & country == "Zimbabwe") ~ "BUBI",
      (psnu == "Lupane" & country == "Zimbabwe") ~ "LUPANE",
      (psnu == "Nkayi" & country == "Zimbabwe") ~ "NKAYI",
      (psnu == "Tsholotsho" & country == "Zimbabwe") ~ "TSHOLOTSHO",
      (psnu == "Beitbridge" & country == "Zimbabwe") ~ "BEITBRIDGE",
      (psnu == "Bulilima" & country == "Zimbabwe") ~ "BULILIMA",
      (psnu == "Gwanda" & country == "Zimbabwe") ~ "GWANDA",
      (psnu == "Insiza" & country == "Zimbabwe") ~ "INSIZA",
      (psnu == "Mangwe" & country == "Zimbabwe") ~ "MANGWE",
      (psnu == "Matobo" & country == "Zimbabwe") ~ "MATOBO",
      (psnu == "Gweru" & country == "Zimbabwe") ~ "GWERU"
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
      filter(country %in% c("Eswatini",
                            "Kenya",
                            "Lesotho",
                            "Rwanda")) %>%
      filter(!is.na(qtr4)) %>%
      group_by(country, snu1, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>%
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (snu1 == "Hhohho" & country == "Eswatini") ~ "Hhohho",
      (snu1 == "Lubombo" & country == "Eswatini") ~ "Lubombo",
      (snu1 == "Manzini" & country == "Eswatini") ~ "Manzini",
      (snu1 == "Shiselweni" & country == "Eswatini") ~ "Shiselweni",
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
      (snu1 == "Mohale's Hoek" & country == "Lesotho") ~ "MOHALE'S HOEK",
      (snu1 == "East" & country == "Rwanda") ~ "EASTERN PROVINCE",
      (snu1 == "Kigali City" & country == "Rwanda") ~ "KIGALI",
      (snu1 == "South" & country == "Rwanda") ~ "SOUTHERN PROVINCE"
    )
  )

my_data_historic <- rbind(my_data_historic_snu1s, 
                 my_data_historic_psnus)

choices_recent_countries_all_recent <- subset(choices, grepl("MER_Structured_Datasets/Current_Frozen/PSNU_Recent/txt/", path_names))

choices_recent_countries_all2_recent <- subset(choices_recent_countries_all_recent, grepl("Botswana|Cote d'Ivoire|Eswatini|Haiti|Kenya|Lesotho|Malawi|Mozambique|Namibia|Rwanda|South Africa|Tanzania|Uganda|Zambia|Zimbabwe", path_names))


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
      group_by(country, snu1, psnu, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>% 
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (psnu == "Bobirwa District" & country == "Botswana") ~ "BOBONONG",
      (psnu == "Mahalapye District" & country == "Botswana") ~ "MAHALAPYE",
      (psnu == "Serowe District" & country == "Botswana") ~ "SEROWE PALAPYE",
      (psnu == "Kgatleng District" & country == "Botswana") ~ "KGATLENG",
      (psnu == "Kweneng East District" & country == "Botswana") ~ "KWENENG EAST",
      (psnu == "North East District" & country == "Botswana") ~ "NORTH EAST",
      (psnu == "Gaborone District" & country == "Botswana") ~ "GABORONE",
      (psnu == "Southern District" & country == "Botswana") ~ "SOUTHERN",
      (psnu == "Abobo-Est" & country == "Cote d'Ivoire") ~ "Abobo-Est",
      (psnu == "Cocody-Bingerville" & country == "Cote d'Ivoire") ~ "Cocody-Bingerville",
      (psnu == "Daloa" & country == "Cote d'Ivoire") ~ "Daloa",
      (psnu == "Man" & country == "Cote d'Ivoire") ~ "Man",
      (psnu == "Dessalines" & country == "Haiti") ~ "Dessalines",
      (psnu == "Saint-Marc" & country == "Haiti") ~ "Saint-Marc",
      (psnu == "Cap-Haïtien" & country == "Haiti") ~ "Cap-Haïtien",
      (psnu == "Port-au-Prince" & country == "Haiti") ~ "Port-au-Prince",
      (psnu == "Blantyre District" & country == "Malawi") ~ "BLANTYRE", #psnu for Malawi
      (psnu == "Machinga District" & country == "Malawi") ~ "MACHINGA", #psnu for Malawi
      (psnu == "Zomba District" & country == "Malawi") ~ "ZOMBA", #psnu for Malawi
      (psnu == "Pemba" & country == "Mozambique") ~ "CIDADE DE PEMBA",
      (psnu == "Chokwe" & country == "Mozambique") ~ "CHÓKWÈ",
      (psnu == "Chonguene" & country == "Mozambique") ~ "CHONGOENE",
      (psnu == "Guija" & country == "Mozambique") ~ "GUIJÁ",
      (psnu == "Limpopo" & country == "Mozambique") ~ "LIMPOPO",
      (psnu == "Xai-Xai" & country == "Mozambique") ~ "CIDADE DE XAI-XAI",
      (psnu == "Maxixe" & country == "Mozambique") ~ "CIDADE DA MAXIXE",
      (psnu == "Chimoio" & country == "Mozambique") ~ "CIDADE DE CHIMOIO",
      (psnu == "Boane" & country == "Mozambique") ~ "BOANE",
      (psnu == "Magude" & country == "Mozambique") ~ "MAGUDE",
      (psnu == "Manhiça" & country == "Mozambique") ~ "MANHIÇA",
      (psnu == "Marracuene" & country == "Mozambique") ~ "MARRACUENE",
      (psnu == "Matola" & country == "Mozambique") ~ "CIDADE DA MATOLA",
      (psnu == "Matutuine" & country == "Mozambique") ~ "MATUTUINE",
      (psnu == "Moamba" & country == "Mozambique") ~ "MOAMBA",
      (psnu == "Namaacha" & country == "Mozambique") ~ "NAMAACHA",
      (psnu == "Erati" & country == "Mozambique") ~ "ERÁTI",
      (psnu == "Nampula" & country == "Mozambique") ~ "CIDADE DE NAMPULA",
      (psnu == "Beira" & country == "Mozambique") ~ "CIDADE DA BEIRA",
      (psnu == "Caia" & country == "Mozambique") ~ "CAIA",
      (psnu == "Gile" & country == "Mozambique") ~ "GILÉ",
      (psnu == "Ile" & country == "Mozambique") ~ "ILE",
      (psnu == "Inhassunge" & country == "Mozambique") ~ "INHASSUNGE",
      (psnu == "Lugela" & country == "Mozambique") ~ "LUGELA",
      (psnu == "Maganja Da Costa" & country == "Mozambique") ~ "MAGANJA DA COSTA",
      (psnu == "Milange" & country == "Mozambique") ~ "MILANGE",
      (psnu == "Mocuba" & country == "Mozambique") ~ "MOCUBA",
      (psnu == "Mocubela" & country == "Mozambique") ~ "MOCUBELA",
      (psnu == "Namacurra" & country == "Mozambique") ~ "NAMACURRA",
      (psnu == "Nicoadala" & country == "Mozambique") ~ "NICOADALA",
      (psnu == "Pebane" & country == "Mozambique") ~ "PEBANE",
      (psnu == "Quelimane" & country == "Mozambique") ~ "QUELIMANE",
      (psnu == "Andara" & country == "Namibia") ~ "MUKWE",
      (psnu == "Nyangana" & country == "Namibia") ~ "NDIYONA",
      (psnu == "Rundu" & country == "Namibia") ~ "RUNDU",
      (psnu == "Windhoek" & country == "Namibia") ~ "WINDHOEK",
      (psnu == "Oshakati" & country == "Namibia") ~ "OSHAKATI",
      (psnu == "Omuthiya" & country == "Namibia") ~ "OMUTHIYAGWIIPUNDI",
      (psnu == "Onandjokwe" & country == "Namibia") ~ "ONIIPA",
      (psnu == "Tsumeb" & country == "Namibia") ~ "TSUMEB",
      (psnu == "Katima Mulilo" & country == "Namibia") ~ "KATIMA MULILO",
      (psnu == "ec Alfred Nzo District Municipality" & country == "South Africa") ~ "ALFRED NZO", #psnu for South Africa
      (psnu == "nw Bojanala Platinum District Municipality" & country == "South Africa") ~ "BOJANALA", #psnu for South Africa
      (psnu == "ec Buffalo City Metropolitan Municipality" & country == "South Africa") ~ "BUFFALO CITY", #psnu for South Africa
      (psnu == "lp Capricorn District Municipality" & country == "South Africa") ~ "CAPRICORN", #psnu for South Africa
      (psnu == "wc City of Cape Town Metropolitan Municipality" & country == "South Africa") ~ "CITY OF CAPE TOWN", #psnu for South Africa
      (psnu == "gp City of Johannesburg Metropolitan Municipality" & country == "South Africa") ~ "CITY OF JOHANNESBURG", #psnu for South Africa
      (psnu == "gp City of Tshwane Metropolitan Municipality" & country == "South Africa") ~ "CITY OF TSHWANE", #psnu for South Africa
      (psnu == "nw Dr Kenneth Kaunda District Municipality" & country == "South Africa") ~ "DOCTOR KENNETH KAUNDA", #psnu for South Africa
      (psnu == "mp Ehlanzeni District Municipality" & country == "South Africa") ~ "EHLANZENI", #psnu for South Africa
      (psnu == "gp Ekurhuleni Metropolitan Municipality" & country == "South Africa") ~ "EKURHULENI", #psnu for South Africa
      (psnu == "kz eThekwini Metropolitan Municipality" & country == "South Africa") ~ "ETHEKWINI", #psnu for South Africa
      (psnu == "mp Gert Sibande District Municipality" & country == "South Africa") ~ "GERT SIBANDE", #psnu for South Africa
      (psnu == "fs Lejweleputswa District Municipality" & country == "South Africa") ~ "LEJWELEPUTSWA", #psnu for South Africa
      (psnu == "lp Mopani District Municipality" & country == "South Africa") ~ "MOPANI", #psnu for South Africa
      (psnu == "nw Ngaka Modiri Molema District Municipality" & country == "South Africa") ~ "NGAKA MODIRI MOLEMA", #psnu for South Africa
      (psnu == "mp Nkangala District Municipality" & country == "South Africa") ~ "NKANGALA", #psnu for South Africa
      (psnu == "ec Oliver Tambo District Municipality" & country == "South Africa") ~ "OR TAMBO", #psnu for South Africa
      (psnu == "gp Sedibeng District Municipality" & country == "South Africa") ~ "SEDIBENG", #psnu for South Africa
      (psnu == "fs Thabo Mofutsanyane District Municipality" & country == "South Africa") ~ "THABO MOFUTSANYANE", #psnu for South Africa
      (psnu == "kz Ugu District Municipality" & country == "South Africa") ~ "UGU", #psnu for South Africa
      (psnu == "kz uMgungundlovu District Municipality" & country == "South Africa") ~ "UMGUNGUNDLOVU", #psnu for South Africa
      (psnu == "kz Uthukela District Municipality" & country == "South Africa") ~ "UTHUKELA", #psnu for South Africa
      (psnu == "kz King Cetshwayo District Municipality" & country == "South Africa") ~ "UTHUNGULU", #psnu for South Africa
      (psnu == "kz Zululand District Municipality" & country == "South Africa") ~ "ZULULAND", #psnu for South Africa
      (psnu == "Temeke MC" & country == "Tanzania") ~ "TEMEKE MUNICIPAL COUNCIL",
      (psnu == "Mufindi DC" & country == "Tanzania") ~ "MUFINDI DISTRICT COUNCIL",
      (psnu == "Muleba DC" & country == "Tanzania") ~ "MULEBA DISTRIC COUNCIL",
      (psnu == "Kyela DC" & country == "Tanzania") ~ "KYELA DISTRICT COUNCIL",
      (psnu == "Mbarali DC" & country == "Tanzania") ~ "MBARALI DISTRICT COUNCIL",
      (psnu == "Mbeya CC" & country == "Tanzania") ~ "MBEYA CITY COUNCIL",
      (psnu == "Nyamagana MC" & country == "Tanzania") ~ "NYAMAGANA MUNICIPAL COUNCIL",
      (psnu == "Kahama TC" & country == "Tanzania") ~ "KAHAMA TOWN COUNCIL",
      (psnu == "Msalala DC" & country == "Tanzania") ~ "MSALALA",
      (psnu == "Shinyanga DC" & country == "Tanzania") ~ "SHINYANGA DISTRICT COUNCIL",
      (psnu == "Shinyanga MC" & country == "Tanzania") ~ "SHINYANGA MUNICIPAL COUNCIL",
      (psnu == "Ushetu DC" & country == "Tanzania") ~ "USHETU DISTRICT COUNCIL",
      (psnu == "Bukomansimbi District" & country == "Uganda") ~ "BUKOMANSIMBI",
      (psnu == "Gomba District" & country == "Uganda") ~ "GOMBA",
      (psnu == "Kalangala District" & country == "Uganda") ~ "KALANGALA",
      (psnu == "Kyotera District" & country == "Uganda") ~ "KYOTERA",
      (psnu == "Lwengo District" & country == "Uganda") ~ "LWENGO",
      (psnu == "Lyantonde District" & country == "Uganda") ~ "LYANTONDE",
      (psnu == "Masaka District" & country == "Uganda") ~ "MASAKA",
      (psnu == "Rakai District" & country == "Uganda") ~ "RAKAI",
      (psnu == "Sembabule District" & country == "Uganda") ~ "SEMBABULE",
      (psnu == "Wakiso District" & country == "Uganda") ~ "WAKISO",
      (psnu == "Kassanda District" & country == "Uganda") ~ "KASANDA",
      (psnu == "Luwero District" & country == "Uganda") ~ "LUWERO",
      (psnu == "Mityana District" & country == "Uganda") ~ "MITYANA",
      (psnu == "Mubende District" & country == "Uganda") ~ "MUBENDE",
      (psnu == "Mukono District" & country == "Uganda") ~ "MUKONO",
      (psnu == "Kampala District" & country == "Uganda") ~ "KAMPALA",
      (psnu == "Agago District" & country == "Uganda") ~ "AGAGO",
      (psnu == "Apac District" & country == "Uganda") ~ "APAC",
      (psnu == "Gulu District" & country == "Uganda") ~ "GULU",
      (psnu == "Kwania District" & country == "Uganda") ~ "KWANIA",
      (psnu == "Lira District" & country == "Uganda") ~ "LIRA",
      (psnu == "Omoro District" & country == "Uganda") ~ "OMORO",
      (psnu == "Oyam District" & country == "Uganda") ~ "OYAM",
      (psnu == "Mbarara District" & country == "Uganda") ~ "MBARARA",
      (psnu == "Kabwe District" & country == "Zambia") ~ "KABWE",
      (psnu == "Kapiri-Mposhi District" & country == "Zambia") ~ "KAPIRI MPOSHI",
      (psnu == "Chingola District" & country == "Zambia") ~ "CHINGOLA",
      (psnu == "Kitwe District" & country == "Zambia") ~ "KITWE",
      (psnu == "Luanshya District" & country == "Zambia") ~ "LUANSHYA",
      (psnu == "Mufulira District" & country == "Zambia") ~ "MUFULIRA",
      (psnu == "Ndola District" & country == "Zambia") ~ "NDOLA",
      (psnu == "Chipata District" & country == "Zambia") ~ "CHIPATA",
      (psnu == "Lusaka District" & country == "Zambia") ~ "LUSAKA",
      (psnu == "Kasama District" & country == "Zambia") ~ "KASAMA",
      (psnu == "Livingstone District" & country == "Zambia") ~ "LIVINGSTONE",
      (psnu == "Mazabuka District" & country == "Zambia") ~ "MAZABUKA",
      (psnu == "Monze District" & country == "Zambia") ~ "MONZE",
      (psnu == "Mongu District" & country == "Zambia") ~ "MONGU",
      (psnu == "Bulawayo" & country == "Zimbabwe") ~ "BULAWAYO",
      (psnu == "Chipinge" & country == "Zimbabwe") ~ "CHIPINGE",
      (psnu == "Makoni" & country == "Zimbabwe") ~ "MAKONI",
      (psnu == "Mutare" & country == "Zimbabwe") ~ "MUTARE",
      (psnu == "Mazowe" & country == "Zimbabwe") ~ "MAZOWE",
      (psnu == "Bubi" & country == "Zimbabwe") ~ "BUBI",
      (psnu == "Lupane" & country == "Zimbabwe") ~ "LUPANE",
      (psnu == "Nkayi" & country == "Zimbabwe") ~ "NKAYI",
      (psnu == "Tsholotsho" & country == "Zimbabwe") ~ "TSHOLOTSHO",
      (psnu == "Beitbridge" & country == "Zimbabwe") ~ "BEITBRIDGE",
      (psnu == "Bulilima" & country == "Zimbabwe") ~ "BULILIMA",
      (psnu == "Gwanda" & country == "Zimbabwe") ~ "GWANDA",
      (psnu == "Insiza" & country == "Zimbabwe") ~ "INSIZA",
      (psnu == "Mangwe" & country == "Zimbabwe") ~ "MANGWE",
      (psnu == "Matobo" & country == "Zimbabwe") ~ "MATOBO",
      (psnu == "Gweru" & country == "Zimbabwe") ~ "GWERU"
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
      filter(country %in% c("Eswatini",
                            "Kenya",
                            "Lesotho",
                            "Rwanda")) %>%
      filter(!is.na(qtr4)) %>%
      group_by(country, snu1, ageasentered, fiscal_year) %>%
      summarize(qtr4 = sum(as.numeric(qtr4)))
    
  }) %>% 
  bind_rows() %>%
  rename(AGYW_PREV = qtr4) %>%
  mutate(
    JOIN_NAME = case_when(
      (snu1 == "Hhohho" & country == "Eswatini") ~ "Hhohho",
      (snu1 == "Lubombo" & country == "Eswatini") ~ "Lubombo",
      (snu1 == "Manzini" & country == "Eswatini") ~ "Manzini",
      (snu1 == "Shiselweni" & country == "Eswatini") ~ "Shiselweni",
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
      (snu1 == "Mohale's Hoek" & country == "Lesotho") ~ "MOHALE'S HOEK",
      (snu1 == "East" & country == "Rwanda") ~ "EASTERN PROVINCE",
      (snu1 == "Kigali City" & country == "Rwanda") ~ "KIGALI",
      (snu1 == "South" & country == "Rwanda") ~ "SOUTHERN PROVINCE"
    )
  )

my_data_recent <- rbind(my_data_recent_psnus,
                        my_data_recent_snu1s)



my_data <- rbind(my_data_historic, 
                 my_data_recent)

my_data <- my_data %>%
  mutate(
    AREA_NAME = case_when(
      (is.na(psnu)) ~ as.character(snu1),
      TRUE ~ as.character(psnu)
    )
  )

### A few basic data checks, if entries are found with no Q4 data, no cumulative data, but Q2 data...
### or if found with no data at all, pass on tp data managers for QC

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
s3write_using(my_data, FUN = write.csv,
              bucket = Sys.getenv("TEST_BUCKET_WRITE"),
              object = "system_dreams_saturation/AGYW_PREVbyCountry.csv"
)


# test reading from write bucket --------
# read from dreams workspace

# print("reading dreams data...")
# read_testing <- s3read_using(FUN = read.csv,
#              bucket = Sys.getenv("TEST_BUCKET_WRITE"),
#              object = "system_dreams_saturation/AGYW_PREVbyCountry.csv") %>%
#   as.data.frame()

#rm(read_testing)


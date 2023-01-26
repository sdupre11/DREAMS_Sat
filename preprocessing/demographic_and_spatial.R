library(readxl)
library(tidyverse)
library(sf)
library(spdep)
library(leaflet)
library(raster)
library(exactextractr)

# CONVENIENCE FUNCTION(S) ----
sf_check <- function(x) {
  x %>%
    as.data.frame() %>%
     dplyr::select(-c(geometry)) %>%
    View()
}

poly_check <- function(ADM2,ADM1) {
  ggplot() +
    geom_sf(data = ADM2, 
            colour = "black", 
            fill = NA) +
    geom_sf(data = ADM1, 
            colour = "red", 
            fill = NA) + 
    coord_sf()
}

pivot_step1 <- function(x) {
  x %>%
    pivot_longer(
      cols = F1014_2018:F2529_2023,
      names_to = c("ageasentered", "fiscal_year"), 
      names_sep = "_",
      names_prefix = "F",
      values_to = "Denominator"
    )
}

pivot_step2 <- function(x, y) {
  if(y == "Tanzania") {
  x %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  NSO_NAME),
      names_from = fiscal_year, 
      values_from = "Denominator"
    )
  } else {
    x %>%
      pivot_wider(
        id_cols = c(AREA_NAME,
                    ageasentered),
        names_from = fiscal_year, 
        values_from = "Denominator"
      )
  }
}

adjust_ages <- function(x) {
  x %>% 
    mutate(
      ageasentered = case_when(
        (ageasentered == 1014) ~ "10-14",
        (ageasentered == 1519) ~ "15-19",
        (ageasentered == 2024) ~ "20-24", 
        (ageasentered == 2529) ~ "25-29",
        TRUE ~ as.character(ageasentered)
      ))
}

add_join_names <- function(x) {
  a <- x %>%
    mutate(
      JOIN_NAME = case_when(
        (AREA_NAME == "BOBONONG" & CNTRY_NAME == "BOTSWANA") ~ "Bobirwa District",
        (AREA_NAME == "MAHALAPYE" & CNTRY_NAME == "BOTSWANA") ~ "Mahalapye District",
        (AREA_NAME == "SEROWE PALAPYE" & CNTRY_NAME == "BOTSWANA") ~ "Serowe District",
        (AREA_NAME == "KGATLENG" & CNTRY_NAME == "BOTSWANA") ~ "Kgatleng District",
        (AREA_NAME == "KWENENG EAST" & CNTRY_NAME == "BOTSWANA") ~ "Kweneng East District",
        (AREA_NAME == "NORTH EAST" & CNTRY_NAME == "BOTSWANA") ~ "North East District",
        (AREA_NAME == "GABORONE" & CNTRY_NAME == "BOTSWANA") ~ "Gaborone District",
        (AREA_NAME == "SOUTHERN" & CNTRY_NAME == "BOTSWANA") ~ "Southern District",
        # (AREA_NAME == "Abobo-Est" & CNTRY_NAME == "Cote d'Ivoire") ~ "Abobo-Est",
        # (AREA_NAME == "Cocody-Bingerville" & CNTRY_NAME == "Cote d'Ivoire") ~ "Cocody-Bingerville",
        # (AREA_NAME == "Daloa" & CNTRY_NAME == "Cote d'Ivoire") ~ "Daloa",
        # (AREA_NAME == "Man" & CNTRY_NAME == "Cote d'Ivoire") ~ "Man",
        # (AREA_NAME == "Dessalines" & CNTRY_NAME == "Haiti") ~ "Dessalines",
        # (AREA_NAME == "Saint-Marc" & CNTRY_NAME == "Haiti") ~ "Saint-Marc",
        # (AREA_NAME == "Cap-Haïtien" & CNTRY_NAME == "Haiti") ~ "Cap-Haïtien",
        # (AREA_NAME == "Port-au-Prince" & CNTRY_NAME == "Haiti") ~ "Port-au-Prince",
        (AREA_NAME == "BLANTYRE" & CNTRY_NAME == "MALAWI") ~ "Blantyre District", #AREA_NAME for Malawi
        (AREA_NAME == "MACHINGA" & CNTRY_NAME == "MALAWI") ~ "Machinga District", #AREA_NAME for Malawi
        (AREA_NAME == "ZOMBA" & CNTRY_NAME == "MALAWI") ~ "Zomba District", #AREA_NAME for Malawi
        (AREA_NAME == "CIDADE DE PEMBA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Pemba",
        (AREA_NAME == "CHÓKWÈ" & CNTRY_NAME == "MOZAMBIQUE") ~ "Chokwe",
        (AREA_NAME == "CHONGOENE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Chonguene",
        (AREA_NAME == "GUIJÁ" & CNTRY_NAME == "MOZAMBIQUE") ~ "Guija",
        (AREA_NAME == "LIMPOPO" & CNTRY_NAME == "MOZAMBIQUE") ~ "Limpopo",
        (AREA_NAME == "CIDADE DE XAI-XAI" & CNTRY_NAME == "MOZAMBIQUE") ~ "Xai-Xai",
        (AREA_NAME == "CIDADE DA MAXIXE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Maxixe",
        (AREA_NAME == "CIDADE DE CHIMOIO" & CNTRY_NAME == "MOZAMBIQUE") ~ "Chimoio",
        (AREA_NAME == "BOANE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Boane",
        (AREA_NAME == "MAGUDE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Magude",
        (AREA_NAME == "MANHIÇA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Manhiça",
        (AREA_NAME == "MARRACUENE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Marracuene",
        (AREA_NAME == "CIDADE DA MATOLA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Matola",
        (AREA_NAME == "MATUTUINE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Matutuine",
        (AREA_NAME == "MOAMBA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Moamba",
        (AREA_NAME == "NAMAACHA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Namaacha",
        (AREA_NAME == "ERÁTI" & CNTRY_NAME == "MOZAMBIQUE") ~ "Erati",
        (AREA_NAME == "CIDADE DE NAMPULA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Nampula",
        (AREA_NAME == "CIDADE DA BEIRA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Beira",
        (AREA_NAME == "CAIA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Caia",
        (AREA_NAME == "GILÉ" & CNTRY_NAME == "MOZAMBIQUE") ~ "Gile",
        (AREA_NAME == "ILE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Ile",
        (AREA_NAME == "INHASSUNGE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Inhassunge",
        (AREA_NAME == "LUGELA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Lugela",
        (AREA_NAME == "MAGANJA DA COSTA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Maganja Da Costa",
        (AREA_NAME == "MILANGE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Milange",
        (AREA_NAME == "MOCUBA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Mocuba",
        (AREA_NAME == "MOCUBELA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Mocubela",
        (AREA_NAME == "NAMACURRA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Namacurra",
        (AREA_NAME == "NICOADALA" & CNTRY_NAME == "MOZAMBIQUE") ~ "Nicoadala",
        (AREA_NAME == "PEBANE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Pebane",
        (AREA_NAME == "QUELIMANE" & CNTRY_NAME == "MOZAMBIQUE") ~ "Quelimane",
        (AREA_NAME == "MUKWE" & CNTRY_NAME == "NAMIBIA") ~ "Andara",
        (AREA_NAME == "NDIYONA" & CNTRY_NAME == "NAMIBIA") ~ "Nyangana",
        (AREA_NAME == "RUNDU" & CNTRY_NAME == "NAMIBIA") ~ "Rundu",
        (AREA_NAME == "WINDHOEK" & CNTRY_NAME == "NAMIBIA") ~ "Windhoek",
        (AREA_NAME == "OSHAKATI" & CNTRY_NAME == "NAMIBIA") ~ "Oshakati",
        (AREA_NAME == "OMUTHIYAGWIIPUNDI" & CNTRY_NAME == "NAMIBIA") ~ "Omuthiya",
        (AREA_NAME == "ONIIPA" & CNTRY_NAME == "NAMIBIA") ~ "Onandjokwe",
        (AREA_NAME == "TSUMEB" & CNTRY_NAME == "NAMIBIA") ~ "Tsumeb",
        (AREA_NAME == "KATIMA MULILO" & CNTRY_NAME == "NAMIBIA") ~ "Katima Mulilo",
        (AREA_NAME == "ALFRED NZO" & CNTRY_NAME == "SOUTH AFRICA") ~ "ec Alfred Nzo District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "BOJANALA" & CNTRY_NAME == "SOUTH AFRICA") ~ "nw Bojanala Platinum District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "BUFFALO CITY" & CNTRY_NAME == "SOUTH AFRICA") ~ "ec Buffalo City Metropolitan Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "CAPRICORN" & CNTRY_NAME == "SOUTH AFRICA") ~ "lp Capricorn District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "CITY OF CAPE TOWN" & CNTRY_NAME == "SOUTH AFRICA") ~ "wc City of Cape Town Metropolitan Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "CITY OF JOHANNESBURG" & CNTRY_NAME == "SOUTH AFRICA") ~ "gp City of Johannesburg Metropolitan Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "CITY OF TSHWANE" & CNTRY_NAME == "SOUTH AFRICA") ~ "gp City of Tshwane Metropolitan Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "DOCTOR KENNETH KAUNDA" & CNTRY_NAME == "SOUTH AFRICA") ~ "nw Dr Kenneth Kaunda District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "EHLANZENI" & CNTRY_NAME == "SOUTH AFRICA") ~ "mp Ehlanzeni District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "EKURHULENI" & CNTRY_NAME == "SOUTH AFRICA") ~ "gp Ekurhuleni Metropolitan Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "ETHEKWINI" & CNTRY_NAME == "SOUTH AFRICA") ~ "kz eThekwini Metropolitan Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "GERT SIBANDE" & CNTRY_NAME == "SOUTH AFRICA") ~ "mp Gert Sibande District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "LEJWELEPUTSWA" & CNTRY_NAME == "SOUTH AFRICA") ~ "fs Lejweleputswa District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "MOPANI" & CNTRY_NAME == "SOUTH AFRICA") ~ "lp Mopani District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "NGAKA MODIRI MOLEMA" & CNTRY_NAME == "SOUTH AFRICA") ~ "nw Ngaka Modiri Molema District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "NKANGALA" & CNTRY_NAME == "SOUTH AFRICA") ~ "mp Nkangala District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "OR TAMBO" & CNTRY_NAME == "SOUTH AFRICA") ~ "ec Oliver Tambo District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "SEDIBENG" & CNTRY_NAME == "SOUTH AFRICA") ~ "gp Sedibeng District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "THABO MOFUTSANYANE" & CNTRY_NAME == "SOUTH AFRICA") ~ "fs Thabo Mofutsanyane District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "UGU" & CNTRY_NAME == "SOUTH AFRICA") ~ "kz Ugu District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "UMGUNGUNDLOVU" & CNTRY_NAME == "SOUTH AFRICA") ~ "kz uMgungundlovu District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "UTHUKELA" & CNTRY_NAME == "SOUTH AFRICA") ~ "kz Uthukela District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "UTHUNGULU" & CNTRY_NAME == "SOUTH AFRICA") ~ "kz King Cetshwayo District Municipality", #AREA_NAME for SOUTH AFRICA
        (AREA_NAME == "ZULULAND" & CNTRY_NAME == "SOUTH AFRICA") ~ "kz Zululand District Municipality", #AREA_NAME for South Africa
        (AREA_NAME == "TEMEKE MUNICIPAL COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Temeke MC",
        (AREA_NAME == "MUFINDI DISTRICT COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Mufindi DC",
        (AREA_NAME == "MULEBA DISTRIC COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Muleba DC",
        (AREA_NAME == "KYELA DISTRICT COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Kyela DC",
        (AREA_NAME == "MBARALI DISTRICT COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Mbarali DC",
        (AREA_NAME == "MBEYA CITY COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Mbeya CC",
        (AREA_NAME == "NYAMAGANA MUNICIPAL COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Nyamagana MC",
        (AREA_NAME == "KAHAMA TOWN COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Kahama TC",
        (AREA_NAME == "MSALALA" & CNTRY_NAME == "TANZANIA") ~ "Msalala DC",
        (AREA_NAME == "SHINYANGA DISTRICT COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Shinyanga DC",
        (AREA_NAME == "SHINYANGA MUNICIPAL COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Shinyanga MC",
        (AREA_NAME == "USHETU DISTRICT COUNCIL" & CNTRY_NAME == "TANZANIA") ~ "Ushetu DC",
        (AREA_NAME == "BUKOMANSIMBI" & CNTRY_NAME == "UGANDA") ~ "Bukomansimbi District",
        (AREA_NAME == "GOMBA" & CNTRY_NAME == "UGANDA") ~ "Gomba District",
        (AREA_NAME == "KALANGALA" & CNTRY_NAME == "UGANDA") ~ "Kalangala District",
        (AREA_NAME == "KYOTERA" & CNTRY_NAME == "UGANDA") ~ "Kyotera District",
        (AREA_NAME == "LWENGO" & CNTRY_NAME == "UGANDA") ~ "Lwengo District",
        (AREA_NAME == "LYANTONDE" & CNTRY_NAME == "UGANDA") ~ "Lyantonde District",
        (AREA_NAME == "MASAKA" & CNTRY_NAME == "UGANDA") ~ "Masaka District",
        (AREA_NAME == "RAKAI" & CNTRY_NAME == "UGANDA") ~ "Rakai District",
        (AREA_NAME == "SEMBABULE" & CNTRY_NAME == "UGANDA") ~ "Sembabule District",
        (AREA_NAME == "WAKISO" & CNTRY_NAME == "UGANDA") ~ "Wakiso District",
        (AREA_NAME == "KASANDA" & CNTRY_NAME == "UGANDA") ~ "Kassanda District",
        (AREA_NAME == "LUWERO" & CNTRY_NAME == "UGANDA") ~ "Luwero District",
        (AREA_NAME == "MITYANA" & CNTRY_NAME == "UGANDA") ~ "Mityana District",
        (AREA_NAME == "MUBENDE" & CNTRY_NAME == "UGANDA") ~ "Mubende District",
        (AREA_NAME == "MUKONO" & CNTRY_NAME == "UGANDA") ~ "Mukono District",
        (AREA_NAME == "KAMPALA" & CNTRY_NAME == "UGANDA") ~ "Kampala District",
        (AREA_NAME == "AGAGO" & CNTRY_NAME == "UGANDA") ~ "Agago District",
        (AREA_NAME == "APAC" & CNTRY_NAME == "UGANDA") ~ "Apac District",
        (AREA_NAME == "GULU" & CNTRY_NAME == "UGANDA") ~ "Gulu District",
        (AREA_NAME == "KWANIA" & CNTRY_NAME == "UGANDA") ~ "Kwania District",
        (AREA_NAME == "LIRA" & CNTRY_NAME == "UGANDA") ~ "Lira District",
        (AREA_NAME == "OMORO" & CNTRY_NAME == "UGANDA") ~ "Omoro District",
        (AREA_NAME == "OYAM" & CNTRY_NAME == "UGANDA") ~ "Oyam District",
        (AREA_NAME == "MBARARA" & CNTRY_NAME == "UGANDA") ~ "Mbarara District",
        (AREA_NAME == "KABWE" & CNTRY_NAME == "ZAMBIA") ~ "Kabwe District",
        (AREA_NAME == "KAPIRI MPOSHI" & CNTRY_NAME == "ZAMBIA") ~ "Kapiri-Mposhi District",
        (AREA_NAME == "CHINGOLA" & CNTRY_NAME == "ZAMBIA") ~ "Chingola District",
        (AREA_NAME == "KITWE" & CNTRY_NAME == "ZAMBIA") ~ "Kitwe District",
        (AREA_NAME == "LUANSHYA" & CNTRY_NAME == "ZAMBIA") ~ "Luanshya District",
        (AREA_NAME == "MUFULIRA" & CNTRY_NAME == "ZAMBIA") ~ "Mufulira District",
        (AREA_NAME == "NDOLA" & CNTRY_NAME == "ZAMBIA") ~ "Ndola District",
        (AREA_NAME == "CHIPATA" & CNTRY_NAME == "ZAMBIA") ~ "Chipata District",
        (AREA_NAME == "LUSAKA" & CNTRY_NAME == "ZAMBIA") ~ "Lusaka District",
        (AREA_NAME == "KASAMA" & CNTRY_NAME == "ZAMBIA") ~ "Kasama District",
        (AREA_NAME == "LIVINGSTONE" & CNTRY_NAME == "ZAMBIA") ~ "Livingstone District",
        (AREA_NAME == "MAZABUKA" & CNTRY_NAME == "ZAMBIA") ~ "Mazabuka District",
        (AREA_NAME == "MONZE" & CNTRY_NAME == "ZAMBIA") ~ "Monze District",
        (AREA_NAME == "MONGU" & CNTRY_NAME == "ZAMBIA") ~ "Mongu District",
        (AREA_NAME == "BULAWAYO" & CNTRY_NAME == "ZIMBABWE") ~ "Bulawayo",
        (AREA_NAME == "CHIPINGE" & CNTRY_NAME == "ZIMBABWE") ~ "Chipinge",
        (AREA_NAME == "MAKONI" & CNTRY_NAME == "ZIMBABWE") ~ "Makoni",
        (AREA_NAME == "MUTARE" & CNTRY_NAME == "ZIMBABWE") ~ "Mutare",
        (AREA_NAME == "MAZOWE" & CNTRY_NAME == "ZIMBABWE") ~ "Mazowe",
        (AREA_NAME == "BUBI" & CNTRY_NAME == "ZIMBABWE") ~ "Bubi",
        (AREA_NAME == "LUPANE" & CNTRY_NAME == "ZIMBABWE") ~ "Lupane",
        (AREA_NAME == "NKAYI" & CNTRY_NAME == "ZIMBABWE") ~ "Nkayi",
        (AREA_NAME == "TSHOLOTSHO" & CNTRY_NAME == "ZIMBABWE") ~ "Tsholotsho",
        (AREA_NAME == "BEITBRIDGE" & CNTRY_NAME == "ZIMBABWE") ~ "Beitbridge",
        (AREA_NAME == "BULILIMA" & CNTRY_NAME == "ZIMBABWE") ~ "Bulilima",
        (AREA_NAME == "GWANDA" & CNTRY_NAME == "ZIMBABWE") ~ "Gwanda",
        (AREA_NAME == "INSIZA" & CNTRY_NAME == "ZIMBABWE") ~ "Insiza",
        (AREA_NAME == "MANGWE" & CNTRY_NAME == "ZIMBABWE") ~ "Mangwe",
        (AREA_NAME == "MATOBO" & CNTRY_NAME == "ZIMBABWE") ~ "Matobo",
        (AREA_NAME == "GWERU" & CNTRY_NAME == "ZIMBABWE") ~ "Gweru",
        # (AREA_NAME == "Hhohho" & CNTRY_NAME == "Eswatini") ~ "Hhohho",
        # (AREA_NAME == "Lubombo" & CNTRY_NAME == "Eswatini") ~ "Lubombo",
        # (AREA_NAME == "Manzini" & CNTRY_NAME == "Eswatini") ~ "Manzini",
        # (AREA_NAME == "Shiselweni" & CNTRY_NAME == "Eswatini") ~ "Shiselweni",
        (AREA_NAME == "HOMA BAY" & CNTRY_NAME == "KENYA") ~ "Homa Bay County",
        (AREA_NAME == "KIAMBU" & CNTRY_NAME == "KENYA") ~ "Kiambu County",
        (AREA_NAME == "KISUMU" & CNTRY_NAME == "KENYA") ~ "Kisumu County",
        (AREA_NAME == "MIGORI" & CNTRY_NAME == "KENYA") ~ "Migori County",
        (AREA_NAME == "MOMBASA" & CNTRY_NAME == "KENYA") ~ "Mombasa County",
        (AREA_NAME == "NAIROBI CITY" & CNTRY_NAME == "KENYA") ~ "Nairobi County",
        (AREA_NAME == "SIAYA" & CNTRY_NAME == "KENYA") ~ "Siaya County",
        (AREA_NAME == "BEREA" & CNTRY_NAME == "LESOTHO") ~ "Berea",
        (AREA_NAME == "MAFETENG" & CNTRY_NAME == "LESOTHO") ~ "Mafeteng",
        (AREA_NAME == "MASERU" & CNTRY_NAME == "LESOTHO") ~ "Maseru",
        (AREA_NAME == "MOHALE'S HOEK" & CNTRY_NAME == "LESOTHO") ~ "Mohale's Hoek",
        (AREA_NAME == "EASTERN PROVINCE" & CNTRY_NAME == "RWANDA") ~ "East",
        (AREA_NAME == "KIGALI" & CNTRY_NAME == "RWANDA") ~ "Kigali City",
        (AREA_NAME == "SOUTHERN PROVINCE" & CNTRY_NAME == "RWANDA") ~ "South",
        TRUE ~ as.character(AREA_NAME)
      )
    )
  
}


## STARTING WITH USCB-SOURCED SHAPEFILES AND ESTIMATES

# BOTSWANA ----
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM2.1.Bot.sf <- st_read('preprocessing/data/Botswana_adm2_uscb_2019.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  add_join_names()


saveRDS(ADM2.1.Bot.sf, file = "data/BotswanaADM2.RDS")

DREAMS_Districts_Botswana_USCB <- c("BOBONONG",
                                    "MAHALAPYE", 
                                    "SEROWE PALAPYE",
                                    "KGATLENG", 
                                    "KWENENG EAST", 
                                    "NORTH EAST", 
                                    "GABORONE", 
                                    "SOUTHERN")
                               

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Botswana_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM2_NAME",
           "ADM_LEVEL", 
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.2.Bot.sf <- ADM2.1.Bot.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng",
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM1_NAME",
            "OBJECTID"))

merged.00 <- merge(ADM2.2.Bot.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM2_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM2_NAME=ADM2_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Botswana_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Botswana")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "BotswanaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b, file = "preprocessing/data/BotswanaOutput_Denominators.RDS")

# KENYA ----
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Ken.sf <- st_read('preprocessing/data/Kenya_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  add_join_names()

saveRDS(ADM1.1.Ken.sf, file = "data/KenyaADM1.RDS")

DREAMS_Districts_Kenya_USCB <- c("HOMA BAY",
                                 "KIAMBU", 
                                 "KISUMU", 
                                 "MIGORI", 
                                 "MOMBASA", 
                                 "NAIROBI CITY", 
                                 "SIAYA")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Kenya_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 1) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM1.2.Ken.sf <- ADM1.1.Ken.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("USCBCMNT",
            "Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM1_NAME"))

merged.00 <- merge(ADM1.2.Ken.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Kenya_USCB)

merged.1d <- merged.1c %>%
  filter(AREA_NAME %in% DREAMS_Districts_Kenya_USCB)

merged.3b <-pivot_step1(merged.1d)

merged.4b <-pivot_step2(merged.3b, 
                        "Kenya")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "KenyaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/KenyaOutput_Denominators.RDS")


# LESOTHO ----
###DENOMINATOR: Small country, ADM1 only
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Les.sf <- st_read('preprocessing/data/Lesotho_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)%>%
  add_join_names()

saveRDS(ADM1.1.Les.sf, file = "data/LesothoADM1.RDS")

DREAMS_Districts_Lesotho_USCB <- c("BEREA", 
                              "MAFETENG",
                              "MASERU",
                              "MOHALE'S HOEK")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Lesotho_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 1) %>%
   dplyr::select(-c("ADM_LEVEL"))

ADM1.2.Les.sf <- ADM1.1.Les.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng",
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "GENC_CODE",
            "FIPS_CODE",
            "NSO_CODE"))

merged.00 <- merge(ADM1.2.Les.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM1_NAME.x",
            "ADM1_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Lesotho_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Lesotho")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "LesothoOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/LesothoOutput_Denominators.RDS")

# MALAWI ----
###DENOMINATOR: Smaller country, ADM1s dropped, ADM2s treated as ADM1s, Lesotho treatment
##Start with District cohort pop by year

ADM1.0.Mal.sf <- st_read('preprocessing/data/Malawi_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326) 

#merge rural/urban pieces

OtherPolys <- ADM1.0.Mal.sf %>%
  dplyr::filter(!(AREA_NAME %in% c("BLANTYRE", "BLANTYRE CITY", "ZOMBA", "ZOMBA CITY")))

BlantyrePolys <- ADM1.0.Mal.sf %>%
  dplyr::filter(AREA_NAME %in% c("BLANTYRE", "BLANTYRE CITY")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

ZombaPolys <- ADM1.0.Mal.sf %>%
  dplyr::filter(AREA_NAME %in% c("ZOMBA", "ZOMBA CITY")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

BlantyrePolys$ADM2_NAME <- "BLANTYRE"
BlantyrePolys$AREA_NAME <- "BLANTYRE"
ZombaPolys$ADM2_NAME <- "ZOMBA"
ZombaPolys$AREA_NAME <- "ZOMBA"

Polys <- bind_rows(OtherPolys,
                   BlantyrePolys,
                   ZombaPolys)

Polys$ADM1_NAME <- Polys$ADM2_NAME

ADM1.1.Mal.sf <- Polys  %>%
  add_join_names()

saveRDS(ADM1.1.Mal.sf, file = "data/MalawiADM1.RDS")

DREAMS_Districts_Malawi_USCB <- c("BLANTYRE", 
                             "MACHINGA",
                             "ZOMBA")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Malawi_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.2.Other <- Demographic.2 %>%
  dplyr::filter(!(AREA_NAME %in% c("BLANTYRE", "BLANTYRE CITY", "ZOMBA", "ZOMBA CITY")))

Demographic.2.Blantyre <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("BLANTYRE", "BLANTYRE CITY")) %>%
  mutate(AREA_NAME = "BLANTYRE") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM1_NAME = "BLANTYRE",
         ADM_LEVEL = 1)


Demographic.2.Zomba <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("ZOMBA", "ZOMBA CITY")) %>%
  mutate(AREA_NAME = "ZOMBA") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM1_NAME = "ZOMBA",
         ADM_LEVEL = 1)

Demographic.3 <- bind_rows(Demographic.2.Other, 
                           Demographic.2.Blantyre, 
                           Demographic.2.Zomba)

Demographic.4 <- Demographic.3 %>%
  filter(ADM_LEVEL == 1) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM1.2.Mal.sf <- ADM1.1.Mal.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng", 
            "Shape_Area",
            "GENC_CODE",
            "FIPS_CODE",
            "ADM_LEVEL",
            "ADM2_NAME",
            #"NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry"))

merged.00 <- merge(ADM1.2.Mal.sf, 
                   Demographic.4, 
                   by.x = "AREA_NAME", 
                   by.y = "AREA_NAME", 
                   all.x = TRUE) %>%
  dplyr::filter(AREA_NAME != "LAKE MALAWI") 

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("ADM1_NAME.x",
            "ADM1_NAME.y",
            "GEO_MATCH.x",
            "GEO_MATCH.y")) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Malawi_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Malawi")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "LesothoOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/MalawiOutput_Denominators.RDS")

# MOZAMBIQUE ----
###DENOMINATOR: Small country, ADM1 only
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM2.1.Moz.sf <- st_read('preprocessing/data/Mozambique_adm2_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)  %>%
  add_join_names()

saveRDS(ADM2.1.Moz.sf, file = "data/MozambiqueADM2.RDS")

DREAMS_Districts_Mozambique_USCB <- c("CIDADE DE PEMBA",
                                 "CHÓKWÈ",
                                 "CHONGOENE",
                                 "GUIJÁ",
                                 "LIMPOPO",
                                 "CIDADE DE XAI-XAI",
                                 "CIDADE DA MAXIXE",
                                 "CIDADE DE CHIMOIO",
                                 "BOANE",
                                 "MAGUDE",
                                 "MANHIÇA",
                                 "MARRACUENE",
                                 "CIDADE DA MATOLA",
                                 "MATUTUINE",
                                 "MOAMBA",
                                 "NAMAACHA",
                                 "ERÁTI",
                                 "CIDADE DE NAMPULA",
                                 "CIDADE DA BEIRA",
                                 "CAIA",
                                 "GILÉ",
                                 "ILE",
                                 "INHASSUNGE",
                                 "LUGELA",
                                 "MAGANJA DA COSTA",
                                 "MILANGE",
                                 "MOCUBA",
                                 "MOCUBELA",
                                 "NAMACURRA",
                                 "NICOADALA",
                                 "PEBANE",
                                 "QUELIMANE")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Mozambique_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM2_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

ADM2.2.Moz.sf <- ADM2.1.Moz.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng",
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "NSO_CODE",
            "USCBCMNT",
            "ADM1_NAME"))

merged.00 <- merge(ADM2.2.Moz.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM2_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM2_NAME=ADM2_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Mozambique_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Mozambique")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "MozambiqueOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/MozambiqueOutput_Denominators.RDS")
# NAMIBIA ----
###DENOMINATOR

ADM2.0.Nam.sf <- st_read('preprocessing/data/Namibia_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326) 

#merge rural/urban pieces

OtherPolys <- ADM2.0.Nam.sf %>%
  dplyr::filter(!(AREA_NAME %in% c("RUNDU RURAL WEST", 
                                   "RUNDU URBAN", 
                                   "RUNDU RURAL EAST", 
                                   "WINDHOEK EAST",
                                   "WINDHOEK RURAL", 
                                   "WINDHOEK WEST", 
                                   "OSHAKATI EAST", 
                                   "OSHAKATI WEST",
                                   "KATIMA MULILO RURAL", 
                                   "KATIMA MULILO URBAN")))

RunduPolys <- ADM2.0.Nam.sf %>%
  dplyr::filter(AREA_NAME %in% c("RUNDU RURAL WEST", 
                                 "RUNDU URBAN", 
                                 "RUNDU RURAL EAST")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

WindhoekPolys <- ADM2.0.Nam.sf %>%
  dplyr::filter(AREA_NAME %in% c("WINDHOEK EAST",
                                 "WINDHOEK RURAL", 
                                 "WINDHOEK WEST")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

OshakatiPolys <- ADM2.0.Nam.sf %>%
  dplyr::filter(AREA_NAME %in% c("OSHAKATI EAST", 
                                 "OSHAKATI WEST")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

KatimaMuliloPolys <- ADM2.0.Nam.sf %>%
  dplyr::filter(AREA_NAME %in% c("KATIMA MULILO RURAL", 
                                 "KATIMA MULILO URBAN")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

RunduPolys$ADM2_NAME <- "RUNDU"
RunduPolys$AREA_NAME <- "RUNDU"
WindhoekPolys$ADM2_NAME <- "WINDHOEK"
WindhoekPolys$AREA_NAME <- "WINDHOEK"
OshakatiPolys$ADM2_NAME <- "OSHAKATI"
OshakatiPolys$AREA_NAME <- "OSHAKATI"
KatimaMuliloPolys$ADM2_NAME <- "KATIMA MULILO"
KatimaMuliloPolys$AREA_NAME <- "KATIMA MULILO"

Polys <- bind_rows(OtherPolys,
                   RunduPolys,
                   WindhoekPolys,
                   OshakatiPolys,
                   KatimaMuliloPolys)

ADM2.1.Nam.sf <- Polys  %>%
  add_join_names()

saveRDS(ADM2.1.Nam.sf, file = "data/NamibiaADM2.RDS")

DREAMS_Districts_Namibia_USCB <- c("MUKWE", 
                             "NDIYONA",
                             "RUNDU",
                             "WINDHOEK",
                             "OSHAKATI",
                             "OMUTHIYAGWIIPUNDI",
                             "ONIIPA",
                             "TSUMEB",
                             "KATIMA MULILO")

# Village in MUKWE, PEPFAR "Andara" seems to be Mukwe
# Village in NDIYONA
# RUNDU RURAL WEST, RUNDU URBAN, RUNDU RURAL EAST
# WINDHOEK EAST, WINDHOEK RURAL, WINDHOEK WEST
# OSHAKATI EAST, OSHAKATI WEST
# OMUTHIYAGWIIPUNDI
# Located within ONIIPA
# TSUMEB
# KATIMA MULILO RURAL, KATIMA MULILO URBAN


##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Namibia_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM2_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.2.Other <- Demographic.2 %>%
  dplyr::filter(!(AREA_NAME %in% c("RUNDU RURAL WEST", 
                                   "RUNDU URBAN", 
                                   "RUNDU RURAL EAST", 
                                   "WINDHOEK EAST",
                                   "WINDHOEK RURAL", 
                                   "WINDHOEK WEST", 
                                   "OSHAKATI EAST", 
                                   "OSHAKATI WEST",
                                   "KATIMA MULILO RURAL", 
                                   "KATIMA MULILO URBAN")))

Demographic.2.Rundu <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("RUNDU RURAL WEST", 
                                 "RUNDU URBAN", 
                                 "RUNDU RURAL EAST")) %>%
  mutate(AREA_NAME = "RUNDU") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM2_NAME = "RUNDU",
         ADM_LEVEL = 2)


Demographic.2.Windhoek <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("WINDHOEK EAST",
                                 "WINDHOEK RURAL", 
                                 "WINDHOEK WEST")) %>%
  mutate(AREA_NAME = "WINDHOEK") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM1_NAME = "WINDHOEK",
         ADM_LEVEL = 2)

Demographic.2.Oshakati <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("OSHAKATI EAST", 
                                 "OSHAKATI WEST")) %>%
  mutate(AREA_NAME = "OSHAKATI") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM1_NAME = "OSHAKATI",
         ADM_LEVEL = 2)

Demographic.2.KatimaMulilo <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("KATIMA MULILO RURAL", 
                                 "KATIMA MULILO URBAN")) %>%
  mutate(AREA_NAME = "KATIMA MULILO") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM1_NAME = "KATIMA MULILO",
         ADM_LEVEL = 2)

Demographic.3 <- bind_rows(Demographic.2.Other, 
                           Demographic.2.Rundu, 
                           Demographic.2.Windhoek,
                           Demographic.2.Oshakati,
                           Demographic.2.KatimaMulilo)

Demographic.4 <- Demographic.3 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.2.Nam.sf <- ADM2.1.Nam.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "ADM1_NAME",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "NSO_CODE",
            "USCBCMNT"))

merged.00 <- merge(ADM2.2.Nam.sf, 
                   Demographic.4, 
                   by.x = "AREA_NAME", 
                   by.y = "AREA_NAME", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("ADM1_NAME",
            "ADM2_NAME.y",
            "GEO_MATCH.x",
            "GEO_MATCH.y")) %>%
  rename(ADM2_NAME = ADM2_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Namibia_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Namibia")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "NamibiaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/NamibiaOutput_Denominators.RDS")

# RWANDA ----
###DENOMINATOR: ADM1s for PEPFAR = SNUs and PSNUs both
ADM1.1.Rwa.sf <- st_read('preprocessing/data/Rwanda_adm1_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  add_join_names()

saveRDS(ADM1.1.Rwa.sf, file = "data/RwandaADM1.RDS")

DREAMS_Districts_Rwanda_USCB <- c("EASTERN PROVINCE", 
                              "KIGALI",
                              "SOUTHERN PROVINCE")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Rwanda_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 1) %>%
   dplyr::select(-c("ADM_LEVEL"))

ADM1.2.Rwa.sf <- ADM1.1.Rwa.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng",
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "GENC_CODE",
            "FIPS_CODE",
            "NSO_CODE",
            "USCBCMNT")) %>%
  filter(AREA_NAME != "LAKE KIVU")

merged.00 <- merge(ADM1.2.Rwa.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM1_NAME.x",
            "ADM1_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Rwanda_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Rwanda")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "RwandaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/RwandaOutput_Denominators.RDS")

# SOUTH AFRICA ----
###DENOMINATOR: Huge country, ADM1s dropped, ADM2s treated as ADM1s, ADM3s treated as ADM2s
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.SAf.sf <- st_read('preprocessing/data/South_Africa_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)  %>%
  add_join_names()

ADM1.1.SAf.sf.export <- ADM1.1.SAf.sf %>%
   dplyr::select(-c("ADM1_NAME")) %>%
  rename("ADM1_NAME" = "ADM2_NAME")

saveRDS(ADM1.1.SAf.sf.export, file = "data/SAfricaADM1.RDS")

###NOTE: UTHUNGULU is PEPFAR kz King Cetshwayo District Municipality

DREAMS_Districts_SouthAfrica_USCB <- c("ALFRED NZO",
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

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/SouthAfrica_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM1.2.SAf.sf <- ADM1.1.SAf.sf %>% 
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM2_NAME"))

merged.00 <- merge(ADM1.2.SAf.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM1_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM1_NAME=ADM1_NAME.x) %>%
  filter(ADM2_NAME %in% DREAMS_Districts_SouthAfrica_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "South Africa")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "SouthAfricaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b, file = "preprocessing/data/SouthAfricaOutput_Denominators.RDS")
# TANZANIA ----
###DENOMINATOR: Huge country, ADM1s dropped, ADM2s treated as ADM1s, ADM3s treated as ADM2s
##Start with District cohort pop by year
ADM2.1.Tan.sf <- st_read('preprocessing/data/Tanzania_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)  %>%
  add_join_names()

saveRDS(ADM2.1.Tan.sf, file = "data/TanzaniaADM2.RDS")

### KEEP AND USE NSO CODE FOR MATCHING FOR TANZANIA, THEY MATCH PEPFAR CODES (except for Mbeya)

DREAMS_Districts_Tanzania_USCB <- c("Kahama TC",
                               "Kyela DC",
                               "Mbarali DC",
                               "Mbeya City Council",
                               "Msalala DC",
                               "Mufindi DC",
                               "Muleba DC",
                               "Nyamagana MC",
                               "Shinyanga DC",
                               "Shinyanga MC",
                               "Temeke MC",
                               "Ushetu DC")


##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Tanzania_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "NSO_NAME",
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.2.Tan.sf <- ADM2.1.Tan.sf %>% 
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM1_NAME"))

merged.00 <- merge(ADM2.2.Tan.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM2_NAME.y",
            "GEO_MATCH",
            "NSO_NAME.y",
            "USCBCMNT")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM2_NAME=ADM2_NAME.x) %>%
  rename(NSO_NAME=NSO_NAME.x) %>%
  filter(NSO_NAME %in% DREAMS_Districts_Tanzania_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Tanzania")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "TanzaniaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b, file = "preprocessing/data/TanzaniaOutput_Denominators.RDS")

# UGANDA ----
ADM1.1.Uga.sf <- st_read('preprocessing/data/Uganda_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)  %>%
  add_join_names()

saveRDS(ADM1.1.Uga.sf, file = "data/UgandaADM1.RDS")

DREAMS_Districts_Uganda_USCB <- c("BUKOMANSIMBI",
                             "GOMBA",
                             "KALANGALA",
                             "KYOTERA",
                             "LWENGO",
                             "LYANTONDE",
                             "MASAKA",
                             "RAKAI",
                             "SEMBABULE",
                             "WAKISO",
                             "KASANDA",
                             "LUWERO",
                             "MITYANA",
                             "MUBENDE",
                             "MUKONO",
                             "KAMPALA",
                             "AGAGO",
                             "APAC",
                             "GULU",
                             "KWANIA",
                             "LIRA",
                             "OMORO",
                             "OYAM",
                             "MBARARA")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Uganda_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 1) %>%
   dplyr::select(-c("ADM_LEVEL"))

ADM1.2.Uga.sf <- ADM1.1.Uga.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng",
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "GENC_CODE",
            "FIPS_CODE",
            "USCBCMNT"))

merged.00 <- merge(ADM1.2.Uga.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM1_NAME.x",
            "ADM1_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Uganda_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Uganda")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "UgandaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/UgandaOutput_Denominators.RDS")
# ZAMBIA ----
###DENOMINATOR ADM2s = PSNUs
ADM2.1.Zam.sf <- st_read('preprocessing/data/Zambia_adm2_uscb_2021.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)  %>%
  add_join_names()

saveRDS(ADM2.1.Zam.sf, file = "data/ZambiaADM2.RDS")

DREAMS_Districts_Zambia_USCB <- c("CHINGOLA",
                             "CHIPATA",
                             "KABWE", 
                             "KAPIRI MPOSHI",
                             "KASAMA",
                             "KITWE",
                             "LIVINGSTONE",
                             "LUANSHYA",
                             "LUSAKA",
                             "MAZABUKA",
                             "MONZE",
                             "MONGU",
                             "MUFULIRA",
                             "NDOLA",
                             "SOUTHERN PROVINCE")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Zambia_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM2_NAME", 
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

ADM2.2.Zam.sf <- ADM2.1.Zam.sf %>%
  as.data.frame() %>%
   dplyr::select(-c("Shape_Leng",
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "USCBCMNT"))

merged.00 <- merge(ADM2.2.Zam.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("AREA_NAME.y",
            "ADM2_NAME.x",
            "ADM2_NAME.y",
            "GEO_MATCH")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  filter(AREA_NAME %in% DREAMS_Districts_Zambia_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Zambia")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "ZambiaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/ZambiaOutput_Denominators.RDS")

# ZIMBABWE ----
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year

ADM2.0.Zim.sf <- st_read('preprocessing/data/Zimbabwe_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

#merge rural/urban pieces

OtherPolys <- ADM2.0.Zim.sf %>%
  dplyr::filter(!(AREA_NAME %in% c("CHIPINGE URBAN", 
                                   "CHIPINGE RURAL", 
                                   "MUTARE URBAN", 
                                   "MUTARE RURAL",
                                   "BEITBRIDGE URBAN", 
                                   "BEITBRIDGE RURAL",
                                   "GWANDA URBAN", 
                                   "GWANDA",
                                   "GWERU", 
                                   "GWERU RURAL")))

ChipingePolys <- ADM2.0.Zim.sf %>%
  dplyr::filter(AREA_NAME %in% c("CHIPINGE URBAN", 
                                 "CHIPINGE RURAL")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

MutarePolys <- ADM2.0.Zim.sf %>%
  dplyr::filter(AREA_NAME %in% c("MUTARE URBAN", 
                                 "MUTARE RURAL")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

BeitbridgePolys <- ADM2.0.Zim.sf %>%
  dplyr::filter(AREA_NAME %in% c("BEITBRIDGE URBAN", 
                                 "BEITBRIDGE RURAL")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

GwandaPolys <- ADM2.0.Zim.sf %>%
  dplyr::filter(AREA_NAME %in% c("GWANDA URBAN", 
                                 "GWANDA")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

GweruPolys <- ADM2.0.Zim.sf %>%
  dplyr::filter(AREA_NAME %in% c("GWERU", 
                                 "GWERU RURAL")) %>%
  group_by(CNTRY_NAME) %>%
  summarize()

ChipingePolys$ADM2_NAME <- "CHIPINGE"
ChipingePolys$AREA_NAME <- "CHIPINGE"
MutarePolys$ADM2_NAME <- "MUTARE"
MutarePolys$AREA_NAME <- "MUTARE"
BeitbridgePolys$ADM2_NAME <- "BEITBRIDGE"
BeitbridgePolys$AREA_NAME <- "BEITBRIDGE"
GwandaPolys$ADM2_NAME <- "GWANDA"
GwandaPolys$AREA_NAME <- "GWANDA"
GweruPolys$ADM2_NAME <- "GWERU"
GweruPolys$AREA_NAME <- "GWERU"


Polys <- bind_rows(OtherPolys,
                   ChipingePolys,
                   MutarePolys,
                   BeitbridgePolys,
                   GwandaPolys,
                   GweruPolys)

Polys$ADM2_NAME <- Polys$ADM2_NAME

ADM2.1.Zim.sf <- Polys  %>%
  add_join_names()

saveRDS(ADM2.1.Zim.sf, file = "data/ZimbabweADM2.RDS")

DREAMS_Districts_Zimbabwe_USCB <- c("BEITBRIDGE",
                               "BUBI",
                               "BULAWAYO",
                               "BULILIMA",
                               "CHIPINGE",
                               "GWANDA",
                               "GWERU",
                               "INSIZA",
                               "LUPANE",
                               "MAKONI",
                               "MANGWE",
                               "MATOBO",
                               "MAZOWE",
                               "MUTARE",
                               "NKAYI",
                               "TSHOLOTSHO")

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Zimbabwe_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
   dplyr::select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM_LEVEL",
           "F1014_2018",
           "F1519_2018",
           "F2024_2018",
           "F2529_2018",
           "F1014_2019",
           "F1519_2019",
           "F2024_2019",
           "F2529_2019",
           "F1014_2020",
           "F1519_2020",
           "F2024_2020",
           "F2529_2020",
           "F1014_2021",
           "F1519_2021",
           "F2024_2021",
           "F2529_2021",
           "F1014_2022",
           "F1519_2022",
           "F2024_2022",
           "F2529_2022",
           "F1014_2023",
           "F1519_2023",
           "F2024_2023",
           "F2529_2023"))

Demographic.2.Other <- Demographic.2 %>%
  dplyr::filter(!(AREA_NAME %in% c("CHIPINGE URBAN", 
                                   "CHIPINGE RURAL", 
                                   "MUTARE URBAN", 
                                   "MUTARE RURAL",
                                   "BEITBRIDGE URBAN", 
                                   "BEITBRIDGE RURAL",
                                   "GWANDA URBAN", 
                                   "GWANDA",
                                   "GWERU", 
                                   "GWERU RURAL")))

Demographic.2.Chipinge <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("CHIPINGE URBAN", 
                                 "CHIPINGE RURAL")) %>%
  mutate(AREA_NAME = "CHIPINGE") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM2_NAME = "CHIPINGE",
         ADM_LEVEL = 2)

Demographic.2.Mutare <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("MUTARE URBAN", 
                                 "MUTARE RURAL")) %>%
  mutate(AREA_NAME = "MUTARE") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM2_NAME = "MUTARE",
         ADM_LEVEL = 2)

Demographic.2.Beitbridge <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("BEITBRIDGE URBAN", 
                                 "BEITBRIDGE RURAL")) %>%
  mutate(AREA_NAME = "BEITBRIDGE") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM2_NAME = "BEITBRIDGE",
         ADM_LEVEL = 2)

Demographic.2.Gwanda <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("GWANDA URBAN", 
                                 "GWANDA")) %>%
  mutate(AREA_NAME = "GWANDA") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM2_NAME = "GWANDA",
         ADM_LEVEL = 2)

Demographic.2.Gweru <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("GWERU", 
                                 "GWERU RURAL")) %>%
  mutate(AREA_NAME = "GWERU") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2018 = sum(F1014_2018),
            F1519_2018 = sum(F1519_2018),
            F2024_2018 = sum(F2024_2018),
            F2529_2018 = sum(F2529_2018),
            F1014_2019 = sum(F1014_2019),
            F1519_2019 = sum(F1519_2019),
            F2024_2019 = sum(F2024_2019),
            F2529_2019 = sum(F2529_2019),
            F1014_2020 = sum(F1014_2020),
            F1519_2020 = sum(F1519_2020),
            F2024_2020 = sum(F2024_2020),
            F2529_2020 = sum(F2529_2020),
            F1014_2021 = sum(F1014_2021),
            F1519_2021 = sum(F1519_2021),
            F2024_2021 = sum(F2024_2021),
            F2529_2021 = sum(F2529_2021),
            F1014_2022 = sum(F1014_2022),
            F1519_2022 = sum(F1519_2022),
            F2024_2022 = sum(F2024_2022),
            F2529_2022 = sum(F2529_2022),
            F1014_2023 = sum(F1014_2023),
            F1519_2023 = sum(F1519_2023),
            F2024_2023 = sum(F2024_2023),
            F2529_2023 = sum(F2529_2023)
  ) %>%
  mutate(ADM2_NAME = "GWERU",
         ADM_LEVEL = 2)

Demographic.3 <- bind_rows(Demographic.2.Other,
                           Demographic.2.Chipinge,
                           Demographic.2.Mutare,
                           Demographic.2.Beitbridge,
                           Demographic.2.Gwanda, 
                           Demographic.2.Gweru)

Demographic.4 <- Demographic.3 %>%
  filter(ADM_LEVEL == 2) %>%
   dplyr::select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.2.Zim.sf <- ADM2.1.Zim.sf %>% 
  as.data.frame() %>%
   dplyr::select(-c("USCBCMNT",
            "Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "NSO_NAME",
            "ADM1_NAME"))

merged.00 <- merge(ADM2.2.Zim.sf, 
                   Demographic.4, 
                   by.x = "AREA_NAME", 
                   by.y = "AREA_NAME", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

merged.1c <- merged.00 %>%
   dplyr::select(-c("ADM1_NAME",
            "ADM2_NAME.y",
            "GEO_MATCH.y",
            "GEO_MATCH.x")) %>%
  rename(ADM2_NAME=ADM2_NAME.x) %>%
  filter(ADM2_NAME %in% DREAMS_Districts_Zimbabwe_USCB)

merged.3b <-pivot_step1(merged.1c)

merged.4b <-pivot_step2(merged.3b, 
                        "Zimbabwe")

merged.5b <- adjust_ages(merged.4b)

##Export Results
#For any manual checking
#write_excel_csv(merged.5b, file = "ZimbabweOutput_Denominators.csv")  #purely for external inspection
#For import to app
saveRDS(merged.5b,file = "preprocessing/data/ZimbabweOutput_Denominators.RDS")







## NEXT PEPFAR SHAPEFILES (HTI, ESW, CDI)
PEPFARAll <- st_read('preprocessing/data/VcPepfarPolygons.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

# COTE D'IVOIRE ----
ADM0.1.CDI.sf <- PEPFARAll %>%
  filter(uid == "ds0ADyc9UCU") %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

AboboEstCDI <- PEPFARAll %>%
  filter(uid == "m4hsNX3VTKP")

CocodyBingervilleCDI <- PEPFARAll %>%
  filter(uid == "uum3VZlFaKv")

DaloaCDI <- PEPFARAll %>%
  filter(uid == "Afx3d2MkpEG")

ManCDI <- PEPFARAll %>%
  filter(uid == "GeLP2HBm78g")

ADM2.1.CDI.sf <- rbind(AboboEstCDI,
                       CocodyBingervilleCDI,
                       DaloaCDI,
                       ManCDI) %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

ADM2.2.CDI.sf <- rbind(ADM2.1.CDI.sf,
                       ADM0.1.CDI.sf)

# saveRDS(ADM0.1.CDI.sf, file = "data/CDIADM0.RDS")
saveRDS(ADM2.2.CDI.sf, file = "data/CDIADM2.RDS")

# ESWATINI ----
ADM0.1.Esw.sf <- PEPFARAll %>%
  filter(uid == "V0qMZH29CtN") %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

HhohhoEswatini <- PEPFARAll %>%
  filter(uid == "qYzGABaWyCf")

LubomboEswatini <- PEPFARAll %>%
  filter(uid == "nxGb6sd7p7D")

ManziniEswatini <- PEPFARAll %>%
  filter(uid == "Z3IDOaDDkwG")

ShiselweniEswatini <- PEPFARAll %>%
  filter(uid == "qRppsyyTP4A")

ADM1.1.Esw.sf <- rbind(HhohhoEswatini,
                       LubomboEswatini,
                       ManziniEswatini,
                       ShiselweniEswatini) %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

ADM1.2.Esw.sf <- rbind(ADM1.1.Esw.sf,
                       ADM0.1.Esw.sf)
  

#saveRDS(ADM0.1.Esw.sf, file = "data/EswADM0.RDS")
saveRDS(ADM1.2.Esw.sf, file = "data/EswADM1.RDS")

# HAITI ----
ADM0.1.Hai.sf <- PEPFARAll %>%
  filter(uid == "JTypsdEUNPw") %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

DessalinesHaiti <- PEPFARAll %>%
  filter(uid == "lDgHhlAPrbI")

SaintMarcHaiti <- PEPFARAll %>%
  filter(uid == "A4RKAXHmMJz")

##Cap-Haïtien
CapHaitienHaiti <- PEPFARAll %>%
  filter(uid == "JVXPyu8T2fO")

PortauPrinceHaiti <- PEPFARAll %>%
  filter(uid == "C4PnwquCK8U")

ADM2.1.Hai.sf <- rbind(DessalinesHaiti,
                       SaintMarcHaiti,
                       CapHaitienHaiti,
                       PortauPrinceHaiti) %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  ) 

ADM2.2.Hai.sf <- rbind(ADM2.1.Hai.sf,
                       ADM0.1.Hai.sf)

# saveRDS(ADM0.1.Hai.sf, file = "data/HaiADM0.RDS")
saveRDS(ADM2.2.Hai.sf, file = "data/HaiADM2.RDS")

rm(PEPFARAll)

## NEXT WORLDPOP ESTIMATES  (HTI, ESW, CDI)

# COTE D'IVOIRE ----
CDI_F10_14_2018 <- raster("preprocessing/data/WorldPop/civ_f_10_2018.tif")
CDI_F15_19_2018 <- raster("preprocessing/data/WorldPop/civ_f_15_2018.tif")
CDI_F20_24_2018 <- raster("preprocessing/data/WorldPop/civ_f_20_2018.tif")
CDI_F25_29_2018 <- raster("preprocessing/data/WorldPop/civ_f_25_2018.tif")
CDI_F10_14_2019 <- raster("preprocessing/data/WorldPop/civ_f_10_2019.tif")
CDI_F15_19_2019 <- raster("preprocessing/data/WorldPop/civ_f_15_2019.tif")
CDI_F20_24_2019 <- raster("preprocessing/data/WorldPop/civ_f_20_2019.tif")
CDI_F25_29_2019 <- raster("preprocessing/data/WorldPop/civ_f_25_2019.tif")
CDI_F10_14_2020 <- raster("preprocessing/data/WorldPop/civ_f_10_2020.tif")
CDI_F15_19_2020 <- raster("preprocessing/data/WorldPop/civ_f_15_2020.tif")
CDI_F20_24_2020 <- raster("preprocessing/data/WorldPop/civ_f_20_2020.tif")
CDI_F25_29_2020 <- raster("preprocessing/data/WorldPop/civ_f_25_2020.tif")

CDI_SF_Temp_10 <- ADM2.1.CDI.sf
CDI_SF_Temp_15 <- ADM2.1.CDI.sf
CDI_SF_Temp_20 <- ADM2.1.CDI.sf
CDI_SF_Temp_25 <- ADM2.1.CDI.sf

CDI_SF_Temp_10$`2018` <- exact_extract(CDI_F10_14_2018, CDI_SF_Temp_10, "sum") %>%
  round(0)
CDI_SF_Temp_10$`2019` <- exact_extract(CDI_F10_14_2019, CDI_SF_Temp_10, "sum") %>%
  round(0)
CDI_SF_Temp_10$`2020` <- exact_extract(CDI_F10_14_2020, CDI_SF_Temp_10, "sum") %>%
  round(0)
CDI_SF_Temp_10$`2021` <- CDI_SF_Temp_10$`2020`
CDI_SF_Temp_10$`2022` <- CDI_SF_Temp_10$`2020`
CDI_SF_Temp_10$`2023` <- CDI_SF_Temp_10$`2020`
CDI_SF_Temp_10$ageasentered <- "10-14"

CDI_SF_Temp_15$`2018` <- exact_extract(CDI_F15_19_2018, CDI_SF_Temp_15, "sum") %>%
  round(0)
CDI_SF_Temp_15$`2019` <- exact_extract(CDI_F15_19_2019, CDI_SF_Temp_15, "sum") %>%
  round(0)
CDI_SF_Temp_15$`2020` <- exact_extract(CDI_F15_19_2020, CDI_SF_Temp_15, "sum") %>%
  round(0)
CDI_SF_Temp_15$`2021` <- CDI_SF_Temp_15$`2020`
CDI_SF_Temp_15$`2022` <- CDI_SF_Temp_15$`2020`
CDI_SF_Temp_15$`2023` <- CDI_SF_Temp_15$`2020`
CDI_SF_Temp_15$ageasentered <- "15-19"

CDI_SF_Temp_20$`2018` <- exact_extract(CDI_F20_24_2018, CDI_SF_Temp_20, "sum") %>%
  round(0)
CDI_SF_Temp_20$`2019` <- exact_extract(CDI_F20_24_2019, CDI_SF_Temp_20, "sum") %>%
  round(0)
CDI_SF_Temp_20$`2020` <- exact_extract(CDI_F20_24_2020, CDI_SF_Temp_20, "sum") %>%
  round(0)
CDI_SF_Temp_20$`2021` <- CDI_SF_Temp_20$`2020`
CDI_SF_Temp_20$`2022` <- CDI_SF_Temp_20$`2020`
CDI_SF_Temp_20$`2023` <- CDI_SF_Temp_20$`2020`
CDI_SF_Temp_20$ageasentered <- "20-24"

CDI_SF_Temp_25$`2018` <- exact_extract(CDI_F25_29_2018, CDI_SF_Temp_25, "sum") %>%
  round(0)
CDI_SF_Temp_25$`2019` <- exact_extract(CDI_F25_29_2019, CDI_SF_Temp_25, "sum") %>%
  round(0)
CDI_SF_Temp_25$`2020` <- exact_extract(CDI_F25_29_2020, CDI_SF_Temp_25, "sum") %>%
  round(0)
CDI_SF_Temp_25$`2021` <- CDI_SF_Temp_25$`2020`
CDI_SF_Temp_25$`2022` <- CDI_SF_Temp_25$`2020`
CDI_SF_Temp_25$`2023` <- CDI_SF_Temp_25$`2020`
CDI_SF_Temp_25$ageasentered <- "25-29"

CDI_SF_Temp <- rbind(CDI_SF_Temp_10,
                     CDI_SF_Temp_15,
                     CDI_SF_Temp_20,
                     CDI_SF_Temp_25)

CDI_SF_Temp$country <- "Cote d'Ivoire"
CDI_SF_Temp$NSO_NAME <- ""
CDI_SF_Temp$AREA_NAME <- as.character("")
CDI_SF_Temp <- CDI_SF_Temp %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

CDI_WorldPop <- CDI_SF_Temp %>%
  as.data.frame() %>%
  dplyr::select(-c("geometry"))

saveRDS(CDI_WorldPop, file = "preprocessing/data/CotedIvoireOutput_Denominators.RDS")

# ESWATINI ----
Esw_F10_14_2018 <- raster("preprocessing/data/WorldPop/swz_f_10_2018.tif")
Esw_F15_19_2018 <- raster("preprocessing/data/WorldPop/swz_f_15_2018.tif")
Esw_F20_24_2018 <- raster("preprocessing/data/WorldPop/swz_f_20_2018.tif")
Esw_F25_29_2018 <- raster("preprocessing/data/WorldPop/swz_f_25_2018.tif")
Esw_F10_14_2019 <- raster("preprocessing/data/WorldPop/swz_f_10_2019.tif")
Esw_F15_19_2019 <- raster("preprocessing/data/WorldPop/swz_f_15_2019.tif")
Esw_F20_24_2019 <- raster("preprocessing/data/WorldPop/swz_f_20_2019.tif")
Esw_F25_29_2019 <- raster("preprocessing/data/WorldPop/swz_f_25_2019.tif")
Esw_F10_14_2020 <- raster("preprocessing/data/WorldPop/swz_f_10_2020.tif")
Esw_F15_19_2020 <- raster("preprocessing/data/WorldPop/swz_f_15_2020.tif")
Esw_F20_24_2020 <- raster("preprocessing/data/WorldPop/swz_f_20_2020.tif")
Esw_F25_29_2020 <- raster("preprocessing/data/WorldPop/swz_f_25_2020.tif")

Esw_SF_Temp_10 <- ADM1.1.Esw.sf
Esw_SF_Temp_15 <- ADM1.1.Esw.sf
Esw_SF_Temp_20 <- ADM1.1.Esw.sf
Esw_SF_Temp_25 <- ADM1.1.Esw.sf

Esw_SF_Temp_10$`2018` <- exact_extract(Esw_F10_14_2018, Esw_SF_Temp_10, "sum") %>%
  round(0)
Esw_SF_Temp_10$`2019` <- exact_extract(Esw_F10_14_2019, Esw_SF_Temp_10, "sum") %>%
  round(0)
Esw_SF_Temp_10$`2020` <- exact_extract(Esw_F10_14_2020, Esw_SF_Temp_10, "sum") %>%
  round(0)
Esw_SF_Temp_10$`2021` <- Esw_SF_Temp_10$`2020`
Esw_SF_Temp_10$`2022` <- Esw_SF_Temp_10$`2020`
Esw_SF_Temp_10$`2023` <- Esw_SF_Temp_10$`2020`
Esw_SF_Temp_10$ageasentered <- "10-14"

Esw_SF_Temp_15$`2018` <- exact_extract(Esw_F15_19_2018, Esw_SF_Temp_15, "sum") %>%
  round(0)
Esw_SF_Temp_15$`2019` <- exact_extract(Esw_F15_19_2019, Esw_SF_Temp_15, "sum") %>%
  round(0)
Esw_SF_Temp_15$`2020` <- exact_extract(Esw_F15_19_2020, Esw_SF_Temp_15, "sum") %>%
  round(0)
Esw_SF_Temp_15$`2021` <- Esw_SF_Temp_15$`2020`
Esw_SF_Temp_15$`2022` <- Esw_SF_Temp_15$`2020`
Esw_SF_Temp_15$`2023` <- Esw_SF_Temp_15$`2020`
Esw_SF_Temp_15$ageasentered <- "15-19"

Esw_SF_Temp_20$`2018` <- exact_extract(Esw_F20_24_2018, Esw_SF_Temp_20, "sum") %>%
  round(0)
Esw_SF_Temp_20$`2019` <- exact_extract(Esw_F20_24_2019, Esw_SF_Temp_20, "sum") %>%
  round(0)
Esw_SF_Temp_20$`2020` <- exact_extract(Esw_F20_24_2020, Esw_SF_Temp_20, "sum") %>%
  round(0)
Esw_SF_Temp_20$`2021` <- Esw_SF_Temp_20$`2020`
Esw_SF_Temp_20$`2022` <- Esw_SF_Temp_20$`2020`
Esw_SF_Temp_20$`2023` <- Esw_SF_Temp_20$`2020`
Esw_SF_Temp_20$ageasentered <- "20-24"

Esw_SF_Temp_25$`2018` <- exact_extract(Esw_F25_29_2018, Esw_SF_Temp_25, "sum") %>%
  round(0)
Esw_SF_Temp_25$`2019` <- exact_extract(Esw_F25_29_2019, Esw_SF_Temp_25, "sum") %>%
  round(0)
Esw_SF_Temp_25$`2020` <- exact_extract(Esw_F25_29_2020, Esw_SF_Temp_25, "sum") %>%
  round(0)
Esw_SF_Temp_25$`2021` <- Esw_SF_Temp_25$`2020`
Esw_SF_Temp_25$`2022` <- Esw_SF_Temp_25$`2020`
Esw_SF_Temp_25$`2023` <- Esw_SF_Temp_25$`2020`
Esw_SF_Temp_25$ageasentered <- "25-29"

Esw_SF_Temp <- rbind(Esw_SF_Temp_10,
                     Esw_SF_Temp_15,
                     Esw_SF_Temp_20,
                     Esw_SF_Temp_25)

Esw_SF_Temp$country <- "Eswatini"
Esw_SF_Temp$NSO_NAME <- ""
Esw_SF_Temp$AREA_NAME <- as.character("")
Esw_SF_Temp <- Esw_SF_Temp %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("bobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

Esw_WorldPop <- Esw_SF_Temp %>%
  as.data.frame() %>%
  dplyr::select(-c("geometry"))
  

saveRDS(Esw_WorldPop, file = "preprocessing/data/EswatiniOutput_Denominators.RDS")

# HAITI ----
Hai_F10_14_2018 <- raster("preprocessing/data/WorldPop/hti_f_10_2018.tif")
Hai_F15_19_2018 <- raster("preprocessing/data/WorldPop/hti_f_15_2018.tif")
Hai_F20_24_2018 <- raster("preprocessing/data/WorldPop/hti_f_20_2018.tif")
Hai_F25_29_2018 <- raster("preprocessing/data/WorldPop/hti_f_25_2018.tif")
Hai_F10_14_2019 <- raster("preprocessing/data/WorldPop/hti_f_10_2019.tif")
Hai_F15_19_2019 <- raster("preprocessing/data/WorldPop/hti_f_15_2019.tif")
Hai_F20_24_2019 <- raster("preprocessing/data/WorldPop/hti_f_20_2019.tif")
Hai_F25_29_2019 <- raster("preprocessing/data/WorldPop/hti_f_25_2019.tif")
Hai_F10_14_2020 <- raster("preprocessing/data/WorldPop/hti_f_10_2020.tif")
Hai_F15_19_2020 <- raster("preprocessing/data/WorldPop/hti_f_15_2020.tif")
Hai_F20_24_2020 <- raster("preprocessing/data/WorldPop/hti_f_20_2020.tif")
Hai_F25_29_2020 <- raster("preprocessing/data/WorldPop/hti_f_25_2020.tif")

Hai_SF_Temp_10 <- ADM2.1.Hai.sf
Hai_SF_Temp_15 <- ADM2.1.Hai.sf
Hai_SF_Temp_20 <- ADM2.1.Hai.sf
Hai_SF_Temp_25 <- ADM2.1.Hai.sf

Hai_SF_Temp_10$`2018` <- exact_extract(Hai_F10_14_2018, Hai_SF_Temp_10, "sum") %>%
  round(0)
Hai_SF_Temp_10$`2019` <- exact_extract(Hai_F10_14_2019, Hai_SF_Temp_10, "sum") %>%
  round(0)
Hai_SF_Temp_10$`2020` <- exact_extract(Hai_F10_14_2020, Hai_SF_Temp_10, "sum") %>%
  round(0)
Hai_SF_Temp_10$`2021` <- Hai_SF_Temp_10$`2020`
Hai_SF_Temp_10$`2022` <- Hai_SF_Temp_10$`2020`
Hai_SF_Temp_10$`2023` <- Hai_SF_Temp_10$`2020`
Hai_SF_Temp_10$ageasentered <- "10-14"

Hai_SF_Temp_15$`2018` <- exact_extract(Hai_F15_19_2018, Hai_SF_Temp_15, "sum") %>%
  round(0)
Hai_SF_Temp_15$`2019` <- exact_extract(Hai_F15_19_2019, Hai_SF_Temp_15, "sum") %>%
  round(0)
Hai_SF_Temp_15$`2020` <- exact_extract(Hai_F15_19_2020, Hai_SF_Temp_15, "sum") %>%
  round(0)
Hai_SF_Temp_15$`2021` <- Hai_SF_Temp_15$`2020`
Hai_SF_Temp_15$`2022` <- Hai_SF_Temp_15$`2020`
Hai_SF_Temp_15$`2023` <- Hai_SF_Temp_15$`2020`
Hai_SF_Temp_15$ageasentered <- "15-19"

Hai_SF_Temp_20$`2018` <- exact_extract(Hai_F20_24_2018, Hai_SF_Temp_20, "sum") %>%
  round(0)
Hai_SF_Temp_20$`2019` <- exact_extract(Hai_F20_24_2019, Hai_SF_Temp_20, "sum") %>%
  round(0)
Hai_SF_Temp_20$`2020` <- exact_extract(Hai_F20_24_2020, Hai_SF_Temp_20, "sum") %>%
  round(0)
Hai_SF_Temp_20$`2021` <- Hai_SF_Temp_20$`2020`
Hai_SF_Temp_20$`2022` <- Hai_SF_Temp_20$`2020`
Hai_SF_Temp_20$`2023` <- Hai_SF_Temp_20$`2020`
Hai_SF_Temp_20$ageasentered <- "20-24"

Hai_SF_Temp_25$`2018` <- exact_extract(Hai_F25_29_2018, Hai_SF_Temp_25, "sum") %>%
  round(0)
Hai_SF_Temp_25$`2019` <- exact_extract(Hai_F25_29_2019, Hai_SF_Temp_25, "sum") %>%
  round(0)
Hai_SF_Temp_25$`2020` <- exact_extract(Hai_F25_29_2020, Hai_SF_Temp_25, "sum") %>%
  round(0)
Hai_SF_Temp_25$`2021` <- Hai_SF_Temp_25$`2020`
Hai_SF_Temp_25$`2022` <- Hai_SF_Temp_25$`2020`
Hai_SF_Temp_25$`2023` <- Hai_SF_Temp_25$`2020`
Hai_SF_Temp_25$ageasentered <- "25-29"

Hai_SF_Temp <- rbind(Hai_SF_Temp_10,
                     Hai_SF_Temp_15,
                     Hai_SF_Temp_20,
                     Hai_SF_Temp_25)

Hai_SF_Temp$country <- "Haiti"
Hai_SF_Temp$NSO_NAME <- ""
Hai_SF_Temp$AREA_NAME <- as.character("")
Hai_SF_Temp <- Hai_SF_Temp %>%
  mutate(
    AREA_NAME = case_when(
      (uid == "JTypsdEUNPw") ~ as.character("Haiti"),
      (uid == "lDgHhlAPrbI") ~ as.character("Dessalines"),
      (uid == "A4RKAXHmMJz") ~ as.character("Saint-Marc"),
      (uid == "JVXPyu8T2fO") ~ as.character("Cap-Haïtien"),
      (uid == "C4PnwquCK8U") ~ as.character("Port-au-Prince"),
      (uid == "V0qMZH29CtN") ~ as.character("Eswatini"),
      (uid == "qYzGABaWyCf") ~ as.character("Hhohho"),
      (uid == "nxGb6sd7p7D") ~ as.character("Lubombo"),
      (uid == "Z3IDOaDDkwG") ~ as.character("Manzini"),
      (uid == "qRppsyyTP4A") ~ as.character("Shiselweni"),
      (uid == "ds0ADyc9UCU") ~ as.character("Cote d'Ivoire"),
      (uid == "m4hsNX3VTKP") ~ as.character("Abobo-Est"),
      (uid == "uum3VZlFaKv") ~ as.character("Cocody-Bingerville"),
      (uid == "Afx3d2MkpEG") ~ as.character("Daloa"),
      (uid == "GeLP2HBm78g") ~ as.character("Man"),
      TRUE ~ as.character("ERROR")
    )
  )

Hai_WorldPop <- Hai_SF_Temp %>%
  as.data.frame() %>%
  dplyr::select(-c("geometry"))

saveRDS(Hai_WorldPop, file = "preprocessing/data/HaitiOutput_Denominators.RDS")

# COMBINE AND PREPROCESS ----
Bot_Data <- readRDS('preprocessing/data/BotswanaOutput_Denominators.RDS') %>%
  mutate(country = "Botswana") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
CDI_Data <- readRDS('preprocessing/data/CotedIvoireOutput_Denominators.RDS')
Esw_Data <- readRDS('preprocessing/data/EswatiniOutput_Denominators.RDS')
Hai_Data <- readRDS('preprocessing/data/HaitiOutput_Denominators.RDS')
Ken_Data <- readRDS('preprocessing/data/KenyaOutput_Denominators.RDS') %>%
  mutate(country = "Kenya") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Les_Data <- readRDS('preprocessing/data/LesothoOutput_Denominators.RDS') %>%
  mutate(country = "Lesotho") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Mal_Data <- readRDS('preprocessing/data/MalawiOutput_Denominators.RDS') %>%
  mutate(country = "Malawi") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Moz_Data <- readRDS('preprocessing/data/MozambiqueOutput_Denominators.RDS') %>%
  mutate(country = "Mozambique") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Nam_Data <- readRDS('preprocessing/data/NamibiaOutput_Denominators.RDS') %>%
  mutate(country = "Namibia") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Rwa_Data <- readRDS('preprocessing/data/RwandaOutput_Denominators.RDS') %>%
  mutate(country = "Rwanda") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
SAf_Data <- readRDS('preprocessing/data/SouthAfricaOutput_Denominators.RDS') %>%
  mutate(country = "South Africa") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Tan_Data <- readRDS('preprocessing/data/TanzaniaOutput_Denominators.RDS') %>%
  mutate(country = "Tanzania")  %>%
  mutate(uid="")
Uga_Data <- readRDS('preprocessing/data/UgandaOutput_Denominators.RDS') %>%
  mutate(country = "Uganda") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Zam_Data <- readRDS('preprocessing/data/ZambiaOutput_Denominators.RDS') %>%
  mutate(country = "Zambia") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")
Zim_Data <- readRDS('preprocessing/data/ZimbabweOutput_Denominators.RDS') %>%
  mutate(country = "Zimbabwe") %>%
  mutate(NSO_NAME="") %>%
  mutate(uid="")

countryData <- rbind(Bot_Data,
                     CDI_Data,
                     Esw_Data,
                     Hai_Data,
                     Ken_Data,
                     Les_Data,
                     Mal_Data,
                     Moz_Data,
                     Nam_Data,
                     Rwa_Data,
                     SAf_Data,
                     Tan_Data,
                     Uga_Data,
                     Zam_Data,
                     Zim_Data) 

countryData <- countryData %>%
  pivot_longer(
    cols = `2018`:`2023`,
    names_to = c("fiscal_year"), 
    names_prefix = "F",
    values_to = "population"
  ) %>%
  rename(JOIN_NAME = AREA_NAME)

countryData$fiscal_year <- countryData$fiscal_year %>%
  as.numeric()

saveRDS(countryData, file = "data/countryData.RDS")


library(readxl)
library(tidyverse)
library(sf)
library(spdep)
library(leaflet)

# CONVENIENCE FUNCTION(S) ----
sf_check <- function(x) {
  x %>%
    as.data.frame() %>%
    select(-c(geometry)) %>%
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
      cols = F1014_2019:F2529_2022,
      names_to = c("ageasentered", "fiscal_year"), 
      names_sep = "_",
      names_prefix = "F",
      values_to = "Denominator"
    )
}

pivot_step2 <- function(x) {
  x %>%
    pivot_wider(
      id_cols = c(AREA_NAME,
                  ageasentered,
                  populationtx),
      names_from = fiscal_year, 
      values_from = "Denominator"
    )
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



####COTE D'IVOIRE [Not currently in subnat estimates] [Will need to adjust for fact that ADM2s are the PEPFAR units]
####ESWATINI [Not currently in subnat estimates]
####HAITI
####MOZAMBIQUE
####NAMIBIA
####RWANDA
####UGANDA [Will need to adjust for fact that ADM2s are the PEPFAR units]
####ZAMBIA



# BOTSWANA ----
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Bot.sf <- st_read('preprocessing/data/Botswana_adm1_uscb_2019.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Bot.sf <- st_read('preprocessing/data/Botswana_adm2_uscb_2019.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Bot.sf, file = "data/BotswanaADM1.RDS")
saveRDS(ADM2.1.Bot.sf, file = "data/BotswanaADM2.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)

DREAMS_Districts_Botswana <- c("CENTRAL", 
                               "KGATLENG", 
                               "KWENENG", 
                               "NORTH EAST", 
                               "SOUTH EAST", 
                               "SOUTHERN")

ADM1.DREAMS.Bot.sf.1 <- ADM1.1.Bot.sf  %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Botswana)

ADM1.DREAMS.Bot.sf.2.Central <- ADM1.DREAMS.Bot.sf.1 %>%
  filter(AREA_NAME == "CENTRAL")

ADM1.DREAMS.Bot.sf.2.Kgatleng <- ADM1.DREAMS.Bot.sf.1 %>%
  filter(AREA_NAME == "KGATLENG")

ADM1.DREAMS.Bot.sf.2.Kweneng <- ADM1.DREAMS.Bot.sf.1 %>%
  filter(AREA_NAME == "KWENENG")

ADM1.DREAMS.Bot.sf.2.NorthEast <- ADM1.DREAMS.Bot.sf.1 %>%
  filter(AREA_NAME == "NORTH EAST")

ADM1.DREAMS.Bot.sf.2.SouthEast <- ADM1.DREAMS.Bot.sf.1 %>%
  filter(AREA_NAME == "SOUTH EAST")

ADM1.DREAMS.Bot.sf.2.Southern <- ADM1.DREAMS.Bot.sf.1 %>%
  filter(AREA_NAME == "SOUTHERN")

# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.Central)
# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.Kgatleng)
# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.Kweneng)
# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.NorthEast)
# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.SouthEast)
# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.Southern)
# sf_check(ADM1.1.Bot.sf)


##Create neighbors list, does not make something it's own neighbor.
ADM2.1.Bot.sf$CentralNeighbor <- ifelse((sf::st_intersects(ADM2.1.Bot.sf, 
                                                           ADM1.DREAMS.Bot.sf.2.Central,
                                                           sparse = F) & !(ADM2.1.Bot.sf$ADM1_NAME %in% DREAMS_Districts_Botswana)),
                                        1,
                                        0)

ADM2.1.Bot.sf$KgatlengNeighbor <- ifelse((sf::st_intersects(ADM2.1.Bot.sf, 
                                                            ADM1.DREAMS.Bot.sf.2.Kgatleng,
                                                            sparse = F) & !(ADM2.1.Bot.sf$ADM1_NAME %in% DREAMS_Districts_Botswana)),
                                         1,
                                         0)

ADM2.1.Bot.sf$KwenengNeighbor <- ifelse((sf::st_intersects(ADM2.1.Bot.sf, 
                                                           ADM1.DREAMS.Bot.sf.2.Kweneng,
                                                           sparse = F) & !(ADM2.1.Bot.sf$ADM1_NAME %in% DREAMS_Districts_Botswana)),
                                        1,
                                        0)


ADM2.1.Bot.sf$NorthEastNeighbor <- ifelse((sf::st_intersects(ADM2.1.Bot.sf, 
                                                             ADM1.DREAMS.Bot.sf.2.NorthEast,
                                                             sparse = F) & !(ADM2.1.Bot.sf$ADM1_NAME %in% DREAMS_Districts_Botswana)),
                                          1,
                                          0)

ADM2.1.Bot.sf$SouthEastNeighbor <- ifelse((sf::st_intersects(ADM2.1.Bot.sf, 
                                                             ADM1.DREAMS.Bot.sf.2.SouthEast,
                                                             sparse = F) & !(ADM2.1.Bot.sf$ADM1_NAME %in% DREAMS_Districts_Botswana)),
                                          1,
                                          0)

ADM2.1.Bot.sf$SouthernNeighbor <- ifelse((sf::st_intersects(ADM2.1.Bot.sf, 
                                                            ADM1.DREAMS.Bot.sf.2.Southern,
                                                            sparse = F) & !(ADM2.1.Bot.sf$ADM1_NAME %in% DREAMS_Districts_Botswana)),
                                         1,
                                         0)



# sf_check(ADM2.1.Bot.sf)
# 
# poly_check(ADM1.1.Bot.sf, ADM1.DREAMS.Bot.sf.2.Southern)
# 
# test1 <- ADM2.1.Bot.sf %>%
#   filter(SouthernNeighbor == 1)
# 
# poly_check(ADM1.1.Bot.sf, test1)
# 
# poly_check(ADM1.DREAMS.Bot.sf.2.Southern, test1)


#Re-adds ones from the district
ADM2.2.Bot.sf <- ADM2.1.Bot.sf

ADM2.2.Bot.sf$CentralNeighbor <- ifelse((ADM2.2.Bot.sf$ADM1_NAME == "CENTRAL" | ADM2.2.Bot.sf$CentralNeighbor == 1),
                                        1,
                                        0)

ADM2.2.Bot.sf$KgatlengNeighbor <- ifelse((ADM2.2.Bot.sf$ADM1_NAME == "KGATLENG" | ADM2.2.Bot.sf$KgatlengNeighbor == 1),
                                         1,
                                         0)

ADM2.2.Bot.sf$KwenengNeighbor <- ifelse((ADM2.2.Bot.sf$ADM1_NAME == "KWENENG" | ADM2.2.Bot.sf$KwenengNeighbor == 1),
                                        1,
                                        0)

ADM2.2.Bot.sf$NorthEastNeighbor <- ifelse((ADM2.2.Bot.sf$ADM1_NAME == "NORTH EAST" | ADM2.2.Bot.sf$NorthEastNeighbor == 1),
                                          1,
                                          0)

ADM2.2.Bot.sf$SouthEastNeighbor <- ifelse((ADM2.2.Bot.sf$ADM1_NAME == "SOUTH EAST" | ADM2.2.Bot.sf$SouthEastNeighbor == 1),
                                          1,
                                          0)

ADM2.2.Bot.sf$SouthernNeighbor <- ifelse((ADM2.2.Bot.sf$ADM1_NAME == "SOUTHERN" | ADM2.2.Bot.sf$SouthernNeighbor == 1),
                                         1,
                                         0)

# sf_check(ADM2.2.Bot.sf)
# 
# test1 <- ADM2.2.Bot.sf %>%
#   filter(SouthernNeighbor == 1)
# 
# poly_check(ADM1.1.Bot.sf, test1)
# 
# poly_check(ADM1.DREAMS.Bot.sf.2.Southern, test1)

ADM2.2.Bot.sf <- ADM2.2.Bot.sf %>%
  as.data.frame()

neighborsLookupBotswana <- ADM2.2.Bot.sf

ADM2.2.Bot.sf$CentralNeighbor <- c(ADM2.2.Bot.sf$CentralNeighbor)
ADM2.2.Bot.sf$KgatlengNeighbor <- c(ADM2.2.Bot.sf$KgatlengNeighbor)
ADM2.2.Bot.sf$KwenengNeighbor <- c(ADM2.2.Bot.sf$KwenengNeighbor)
ADM2.2.Bot.sf$NorthEastNeighbor <- c(ADM2.2.Bot.sf$NorthEastNeighbor)
ADM2.2.Bot.sf$SouthEastNeighbor <- c(ADM2.2.Bot.sf$SouthEastNeighbor)
ADM2.2.Bot.sf$SouthernNeighbor <- c(ADM2.2.Bot.sf$SouthernNeighbor)

#sf_check(ADM2.2.Zim.sf)

ADM2.2.Bot.sf$DREAMSneighbors <- ADM2.2.Bot.sf$CentralNeighbor + ADM2.2.Bot.sf$KgatlengNeighbor + ADM2.2.Bot.sf$KwenengNeighbor + ADM2.2.Bot.sf$NorthEastNeighbor + ADM2.2.Bot.sf$SouthEastNeighbor + ADM2.2.Bot.sf$SouthernNeighbor

ADM2.2.Bot.sf <- ADM2.2.Bot.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM1_NAME %in% DREAMS_Districts_Botswana) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )

#sf_check(ADM2.2.Zim.sf)

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Botswana_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
  select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.3.Bot.sf <- ADM2.2.Bot.sf %>% 
  select(-c("Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM2_NAME",
            "OBJECTID"))

merged.00 <- merge(ADM2.3.Bot.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
  select(-c("AREA_NAME.y",
            "CentralNeighbor",
            "KgatlengNeighbor",
            "KwenengNeighbor",
            "NorthEastNeighbor",
            "SouthEastNeighbor",
            "SouthernNeighbor",
            "ADM1_NAME.y",
            "ADM2_NAME",
            "GEO_MATCH",
            "DREAMSneighbors")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM1_NAME=ADM1_NAME.x) %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Botswana)


##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

##Create new pop total for each
#Take those that border X (inc. X itself and exc. other DREAMS districts)
### XXX WHY !is.na(F1014_2019)?
merged.1.Central <- merged.1 %>%
  filter(CentralNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("KGATLENG", 
                                                                           "KWENENG", 
                                                                           "NORTH EAST", 
                                                                           "SOUTH EAST", 
                                                                           "SOUTHERN")))) %>%
  group_by(CentralNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Kgatleng <- merged.1 %>%
  filter(KgatlengNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("CENTRAL", 
                                                                            "KWENENG", 
                                                                            "NORTH EAST", 
                                                                            "SOUTH EAST", 
                                                                            "SOUTHERN")))) %>%
  group_by(KgatlengNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Kweneng <- merged.1 %>%
  filter(KwenengNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("CENTRAL", 
                                                                           "KGATLENG", 
                                                                           "NORTH EAST", 
                                                                           "SOUTH EAST", 
                                                                           "SOUTHERN")))) %>%
  group_by(KwenengNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.NorthEast <- merged.1 %>%
  filter(NorthEastNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("CENTRAL", 
                                                                             "KGATLENG", 
                                                                             "KWENENG", 
                                                                             "SOUTH EAST", 
                                                                             "SOUTHERN")))) %>%
  group_by(NorthEastNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.SouthEast <- merged.1 %>%
  filter(SouthEastNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("CENTRAL", 
                                                                             "KGATLENG", 
                                                                             "KWENENG", 
                                                                             "NORTH EAST", 
                                                                             "SOUTHERN")))) %>%
  group_by(SouthEastNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Southern <- merged.1 %>%
  filter(SouthernNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("CENTRAL", 
                                                                            "KGATLENG", 
                                                                            "KWENENG", 
                                                                            "NORTH EAST", 
                                                                            "SOUTH EAST")))) %>%
  group_by(SouthernNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Central$AREA_NAME <- "CENTRAL"
merged.1.Kgatleng$AREA_NAME <- "KGATLENG"
merged.1.Kweneng$AREA_NAME <- "KWENENG"
merged.1.NorthEast$AREA_NAME <- "NORTH EAST"
merged.1.SouthEast$AREA_NAME <- "SOUTH EAST"
merged.1.Southern$AREA_NAME <- "SOUTHERN"

merged.2a <- rbind(merged.1.Central,
                   merged.1.Kgatleng,
                   merged.1.Kweneng,
                   merged.1.NorthEast,
                   merged.1.SouthEast,
                   merged.1.Southern)

merged.1d <- merged.1c %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Botswana) %>%
  group_by(ADM1_NAME) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = ADM1_NAME)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1d)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

##Export Results
#For any manual checking
#write_excel_csv(merged.6, file = "BotswanaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.6, file = "preprocessing/data/BotswanaOutput_Denominators.RDS")



# KENYA ----
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Ken.sf <- st_read('preprocessing/data/Kenya_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Ken.sf <- st_read('preprocessing/data/Kenya_adm2_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Ken.sf, file = "data/KenyaADM1.RDS")
saveRDS(ADM2.1.Ken.sf, file = "data/KenyaADM2.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)

DREAMS_Districts_Kenya <- c("HOMA BAY", 
                            "KIAMBU", 
                            "KISUMU", 
                            "MIGORI", 
                            "MOMBASA", 
                            "NAIROBI CITY", 
                            "SIAYA")

ADM1.DREAMS.Ken.sf.1 <- ADM1.1.Ken.sf  %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Kenya)

ADM1.DREAMS.Ken.sf.2.Homa <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "HOMA BAY")

ADM1.DREAMS.Ken.sf.2.Kiambu <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "KIAMBU")

ADM1.DREAMS.Ken.sf.2.Kisumu <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "KISUMU")

ADM1.DREAMS.Ken.sf.2.Migori <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "MIGORI")

ADM1.DREAMS.Ken.sf.2.Mombasa <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "MOMBASA")

ADM1.DREAMS.Ken.sf.2.Nairobi <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "NAIROBI CITY")

ADM1.DREAMS.Ken.sf.2.Siaya <- ADM1.DREAMS.Ken.sf.1 %>%
  filter(AREA_NAME == "SIAYA")
# 
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Homa)
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Kiambu)
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Kisumu)
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Migori)
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Mombasa)
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Nairobi)
# poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Siaya)
# sf_check(ADM1.1.Ken.sf)


##Create neighbors list, does not make something it's own neighbor.
ADM2.1.Ken.sf$HomaNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                        ADM1.DREAMS.Ken.sf.2.Homa,
                                                        sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                     1,
                                     0)

ADM2.1.Ken.sf$KiambuNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                          ADM1.DREAMS.Ken.sf.2.Kiambu,
                                                          sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                       1,
                                       0)

ADM2.1.Ken.sf$KisumuNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                          ADM1.DREAMS.Ken.sf.2.Kisumu,
                                                          sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                       1,
                                       0)


ADM2.1.Ken.sf$MigoriNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                          ADM1.DREAMS.Ken.sf.2.Migori,
                                                          sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                       1,
                                       0)

ADM2.1.Ken.sf$MombasaNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                           ADM1.DREAMS.Ken.sf.2.Mombasa,
                                                           sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                        1,
                                        0)

ADM2.1.Ken.sf$NairobiNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                           ADM1.DREAMS.Ken.sf.2.Nairobi,
                                                           sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                        1,
                                        0)

ADM2.1.Ken.sf$SiayaNeighbor <- ifelse((sf::st_intersects(ADM2.1.Ken.sf, 
                                                         ADM1.DREAMS.Ken.sf.2.Siaya,
                                                         sparse = F) & !(ADM2.1.Ken.sf$ADM1_NAME %in% DREAMS_Districts_Kenya)),
                                      1,
                                      0)

# sf_check(ADM2.1.Ken.sf)
# 
#poly_check(ADM1.1.Ken.sf, ADM1.DREAMS.Ken.sf.2.Siaya)

# test1 <- ADM2.1.Ken.sf %>%
#   filter(SiayaNeighbor == 1)
# 
# poly_check(ADM1.1.Ken.sf, test1)
# 
# poly_check(ADM1.DREAMS.Ken.sf.2.Siaya, test1)


#Re-adds ones from the district
ADM2.2.Ken.sf <- ADM2.1.Ken.sf

ADM2.2.Ken.sf$HomaNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "HOMA BAY" | ADM2.2.Ken.sf$HomaNeighbor == 1),
                                     1,
                                     0)

ADM2.2.Ken.sf$KiambuNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "KIAMBU" | ADM2.2.Ken.sf$KiambuNeighbor == 1),
                                       1,
                                       0)

ADM2.2.Ken.sf$KisumuNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "KISUMU" | ADM2.2.Ken.sf$KisumuNeighbor == 1),
                                       1,
                                       0)

ADM2.2.Ken.sf$MigoriNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "MIGORI" | ADM2.2.Ken.sf$MigoriNeighbor == 1),
                                       1,
                                       0)

ADM2.2.Ken.sf$MombasaNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "MOMBASA" | ADM2.2.Ken.sf$MombasaNeighbor == 1),
                                        1,
                                        0)

ADM2.2.Ken.sf$NairobiNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "NAIROBI CITY" | ADM2.2.Ken.sf$NairobiNeighbor == 1),
                                        1,
                                        0)

ADM2.2.Ken.sf$SiayaNeighbor <- ifelse((ADM2.2.Ken.sf$ADM1_NAME == "SIAYA" | ADM2.2.Ken.sf$SiayaNeighbor == 1),
                                      1,
                                      0)

# sf_check(ADM2.2.Ken.sf)
# 
# test1 <- ADM2.2.Ken.sf %>%
#   filter(SiayaNeighbor == 1)
# 
# poly_check(ADM1.1.Ken.sf, test1)
# 
# poly_check(ADM1.DREAMS.Ken.sf.2.Siaya, test1)

ADM2.2.Ken.sf <- ADM2.2.Ken.sf %>%
  as.data.frame()

neighborsLookupKenya <- ADM2.2.Ken.sf

ADM2.2.Ken.sf$HomaNeighbor <- c(ADM2.2.Ken.sf$HomaNeighbor)
ADM2.2.Ken.sf$KiambuNeighbor <- c(ADM2.2.Ken.sf$KiambuNeighbor)
ADM2.2.Ken.sf$KisumuNeighbor <- c(ADM2.2.Ken.sf$KisumuNeighbor)
ADM2.2.Ken.sf$MigoriNeighbor <- c(ADM2.2.Ken.sf$MigoriNeighbor)
ADM2.2.Ken.sf$MombasaNeighbor <- c(ADM2.2.Ken.sf$MombasaNeighbor)
ADM2.2.Ken.sf$NairobiNeighbor <- c(ADM2.2.Ken.sf$NairobiNeighbor)
ADM2.2.Ken.sf$SiayaNeighbor <- c(ADM2.2.Ken.sf$SiayaNeighbor)

#sf_check(ADM2.2.Zim.sf)

ADM2.2.Ken.sf$DREAMSneighbors <- ADM2.2.Ken.sf$HomaNeighbor + ADM2.2.Ken.sf$KiambuNeighbor + ADM2.2.Ken.sf$KisumuNeighbor + ADM2.2.Ken.sf$MigoriNeighbor + ADM2.2.Ken.sf$MombasaNeighbor + ADM2.2.Ken.sf$NairobiNeighbor + ADM2.2.Ken.sf$SiayaNeighbor

ADM2.2.Ken.sf <- ADM2.2.Ken.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM1_NAME %in% DREAMS_Districts_Kenya) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )

#sf_check(ADM2.2.Zim.sf)

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Kenya_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
  select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.3.Ken.sf <- ADM2.2.Ken.sf %>% 
  select(-c("USCBCMNT",
            "Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM2_NAME"))

merged.00 <- merge(ADM2.3.Ken.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
  select(-c("AREA_NAME.y",
            "HomaNeighbor",
            "KiambuNeighbor",
            "KisumuNeighbor",
            "MigoriNeighbor",
            "MombasaNeighbor",
            "NairobiNeighbor",
            "SiayaNeighbor",
            "ADM1_NAME.y",
            "ADM2_NAME",
            "GEO_MATCH",
            "DREAMSneighbors")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM1_NAME=ADM1_NAME.x) %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Kenya)


##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

##Create new pop total for each
#Take those that border X (inc. X itself and exc. other DREAMS districts)
### XXX WHY !is.na(F1014_2019)?
merged.1.Homa <- merged.1 %>%
  filter(HomaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("KIAMBU", 
                                                                        "KISUMU", 
                                                                        "MIGORI", 
                                                                        "MOMBASA", 
                                                                        "NAIROBI CITY", 
                                                                        "SIAYA")))) %>%
  group_by(HomaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Kiambu <- merged.1 %>%
  filter(KiambuNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("HOMA BAY", 
                                                                          "KISUMU", 
                                                                          "MIGORI", 
                                                                          "MOMBASA", 
                                                                          "NAIROBI CITY", 
                                                                          "SIAYA")))) %>%
  group_by(KiambuNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Kisumu <- merged.1 %>%
  filter(KisumuNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("HOMA BAY", 
                                                                          "KIAMBU", 
                                                                          "MIGORI", 
                                                                          "MOMBASA", 
                                                                          "NAIROBI CITY", 
                                                                          "SIAYA")))) %>%
  group_by(KisumuNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Migori <- merged.1 %>%
  filter(MigoriNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("HOMA BAY",
                                                                          "KIAMBU", 
                                                                          "KISUMU", 
                                                                          "MOMBASA", 
                                                                          "NAIROBI CITY", 
                                                                          "SIAYA")))) %>%
  group_by(MigoriNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Mombasa <- merged.1 %>%
  filter(MombasaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("HOMA BAY",
                                                                           "KIAMBU", 
                                                                           "KISUMU", 
                                                                           "MIGORI",
                                                                           "NAIROBI CITY", 
                                                                           "SIAYA")))) %>%
  group_by(MombasaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Nairobi <- merged.1 %>%
  filter(NairobiNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("HOMA BAY",
                                                                           "KIAMBU", 
                                                                           "KISUMU", 
                                                                           "MIGORI", 
                                                                           "MOMBASA", 
                                                                           "SIAYA")))) %>%
  group_by(NairobiNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Siaya <- merged.1 %>%
  filter(SiayaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("HOMA BAY",
                                                                         "KIAMBU", 
                                                                         "KISUMU", 
                                                                         "MIGORI", 
                                                                         "MOMBASA", 
                                                                         "NAIROBI CITY")))) %>%
  group_by(SiayaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Homa$AREA_NAME <- "HOMA BAY"
merged.1.Kiambu$AREA_NAME <- "KIAMBU"
merged.1.Kisumu$AREA_NAME <- "KISUMU"
merged.1.Migori$AREA_NAME <- "MIGORI"
merged.1.Mombasa$AREA_NAME <- "MOMBASA"
merged.1.Nairobi$AREA_NAME <- "NAIROBI CITY"
merged.1.Siaya$AREA_NAME <- "SIAYA"

merged.2a <- rbind(merged.1.Homa,
                   merged.1.Kiambu,
                   merged.1.Kisumu,
                   merged.1.Migori,
                   merged.1.Mombasa,
                   merged.1.Nairobi,
                   merged.1.Siaya)

merged.1d <- merged.1c %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Kenya) %>%
  group_by(ADM1_NAME) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = ADM1_NAME)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1d)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

##Export Results
#For any manual checking
#write_excel_csv(merged.6, file = "KenyaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.6,file = "preprocessing/data/KenyaOutput_Denominators.RDS")


# LESOTHO ----
###DENOMINATOR: Small country, ADM1 only
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Les.sf <- st_read('preprocessing/data/Lesotho_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Les.sf, file = "data/LesothoADM1.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)

DREAMS_Districts_Lesotho <- c("BEREA", 
                              "MAFETENG",
                              "MASERU",
                              "MOHALE'S HOEK")

ADM1.DREAMS.Les.sf.1 <- ADM1.1.Les.sf %>%
  filter(AREA_NAME %in% DREAMS_Districts_Lesotho)

ADM1.DREAMS.Les.sf.2.Berea <- ADM1.DREAMS.Les.sf.1 %>%
  filter(AREA_NAME == "BEREA")

ADM1.DREAMS.Les.sf.2.Mafeteng <- ADM1.DREAMS.Les.sf.1 %>%
  filter(AREA_NAME == "MAFETENG")

ADM1.DREAMS.Les.sf.2.Maseru <- ADM1.DREAMS.Les.sf.1 %>%
  filter(AREA_NAME == "MASERU")

ADM1.DREAMS.Les.sf.2.MHoek <- ADM1.DREAMS.Les.sf.1 %>%
  filter(AREA_NAME == "MOHALE'S HOEK")

# poly_check(ADM1.1.Les.sf, ADM1.DREAMS.Les.sf.2.Berea)
# poly_check(ADM1.1.Les.sf, ADM1.DREAMS.Les.sf.2.Mafeteng)
# poly_check(ADM1.1.Les.sf, ADM1.DREAMS.Les.sf.2.Maseru)
# poly_check(ADM1.1.Les.sf, ADM1.DREAMS.Les.sf.2.MHoek)
# sf_check(ADM1.1.Les.sf)

##Create neighbors list
ADM1.1.Les.sf$BereaNeighbor <- ifelse((sf::st_intersects(ADM1.1.Les.sf,
                                                         ADM1.DREAMS.Les.sf.2.Berea,
                                                         sparse = F)),
                                      1,
                                      0)

ADM1.1.Les.sf$MafetengNeighbor <- ifelse((sf::st_intersects(ADM1.1.Les.sf, 
                                                            ADM1.DREAMS.Les.sf.2.Mafeteng,
                                                            sparse = F)),
                                         1,
                                         0)

ADM1.1.Les.sf$MaseruNeighbor <- ifelse((sf::st_intersects(ADM1.1.Les.sf, 
                                                          ADM1.DREAMS.Les.sf.2.Maseru,
                                                          sparse = F)),
                                       1,
                                       0)


ADM1.1.Les.sf$MHoekNeighbor <- ifelse((sf::st_intersects(ADM1.1.Les.sf,
                                                         ADM1.DREAMS.Les.sf.2.MHoek,
                                                         sparse = F)),
                                      1,
                                      0)
#sf_check(ADM1.1.Les.sf)

ADM1.1.Les.sf <- ADM1.1.Les.sf %>%
  as.data.frame()

ADM1.1.Les.sf$BereaNeighbor <- c(ADM1.1.Les.sf$BereaNeighbor)
ADM1.1.Les.sf$MafetengNeighbor <- c(ADM1.1.Les.sf$MafetengNeighbor)
ADM1.1.Les.sf$MaseruNeighbor <- c(ADM1.1.Les.sf$MaseruNeighbor)
ADM1.1.Les.sf$MHoekNeighbor <- c(ADM1.1.Les.sf$MHoekNeighbor)

#Setup lookup table df to be used in filtering app map
neighborsLookupLesotho <- ADM1.1.Les.sf
  

ADM1.1.Les.sf$DREAMSneighbors <- ADM1.1.Les.sf$BereaNeighbor + ADM1.1.Les.sf$MafetengNeighbor + ADM1.1.Les.sf$MaseruNeighbor + ADM1.1.Les.sf$MHoekNeighbor

#Set DREAMSneighbors to zero for DREAMS ADM1s, this flags them for non-treatment later
ADM1.1.Les.sf <- ADM1.1.Les.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM1_NAME %in% DREAMS_Districts_Lesotho) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )

#sf_check(ADM1.1.Les.sf)

#Strip out remaining spatial parameters and unnecessary fields
ADM1.2.Les.sf <- ADM1.1.Les.sf %>% 
  select(-c("Shape_Leng", 
            "Shape_Area",
            "GENC_CODE",
            "FIPS_CODE",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry"))

#sf_check(ADM1.2.Les.sf)

# test1 <- ADM1.1.Les.sf %>%
#   filter(MafetengNeighbor == 1) 
# poly_check(test1, ADM1.DREAMS.Les.sf.2.Mafeteng)

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Lesotho_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME", 
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 1) %>%
  select(-c("ADM_LEVEL"))

merged.00 <- merge(ADM1.2.Les.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Check for any mismatches between paired X and Y dataset shared fields

##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

merged.1c <- merged.1 %>%
  select(-c("AREA_NAME.y",
            "BereaNeighbor",
            "MafetengNeighbor",
            "MaseruNeighbor",
            "MHoekNeighbor",
            "ADM1_NAME.x",
            "ADM1_NAME.y",
            "GEO_MATCH",
            "DREAMSneighbors")) %>%
  rename(AREA_NAME=AREA_NAME.x)

##Create new pop total for each
#Take those that border Berea (inc. Berea itself and exc. other DREAMS districts)
### XXX WHY !is.na(F1014_2019)?
merged.1.Berea <- merged.1 %>%
  filter(BereaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("MASERU",
                                                                         "MAFETENG",
                                                                         "MOHALE'S HOEK")))) %>%
  group_by(BereaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Mafeteng <- merged.1 %>%
  filter(MafetengNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("MASERU",
                                                                            "BEREA",
                                                                            "MOHALE'S HOEK")))) %>%
  group_by(MafetengNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Maseru <- merged.1 %>%
  filter(MaseruNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BEREA",
                                                                          "MAFETENG",
                                                                          "MOHALE'S HOEK")))) %>%
  group_by(MaseruNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.MHoek <- merged.1 %>%
  filter(MHoekNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("MASERU",
                                                                         "MAFETENG",
                                                                         "BEREA")))) %>%
  group_by(MHoekNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Berea$AREA_NAME <- "BEREA"
merged.1.Mafeteng$AREA_NAME <- "MAFETENG"
merged.1.Maseru$AREA_NAME <- "MASERU"
merged.1.MHoek$AREA_NAME <- "MOHALE'S HOEK"

merged.2a <- rbind(merged.1.Berea,
                   merged.1.Mafeteng,
                   merged.1.Maseru,
                   merged.1.MHoek)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1c)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

merged.7 <- merged.6 %>%
  filter(AREA_NAME %in% c("BEREA",
                          "MAFETENG",
                          "MASERU",
                          "MOHALE'S HOEK"))
                                                

##Export Results
#For any manual checking
#write_excel_csv(merged.7, file = "LesothoOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.7,file = "preprocessing/data/LesothoOutput_Denominators.RDS")

# MALAWI ----
###DENOMINATOR: Smaller country, ADM1s dropped, ADM2s treated as ADM1s, Lesotho treatment
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year

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

ADM1.1.Mal.sf <- Polys

saveRDS(ADM1.1.Mal.sf, file = "data/MalawiADM1.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)


DREAMS_Districts_Malawi <- c("BLANTYRE", 
                             "MACHINGA",
                             "ZOMBA")

ADM1.DREAMS.Mal.sf.1 <- ADM1.1.Mal.sf %>%
  filter(AREA_NAME %in% DREAMS_Districts_Malawi)

ADM1.DREAMS.Mal.sf.2.Blantyre <- ADM1.DREAMS.Mal.sf.1 %>%
  filter(AREA_NAME == "BLANTYRE")

ADM1.DREAMS.Mal.sf.2.Machinga <- ADM1.DREAMS.Mal.sf.1 %>%
  filter(AREA_NAME == "MACHINGA")

ADM1.DREAMS.Mal.sf.2.Zomba <- ADM1.DREAMS.Mal.sf.1 %>%
  filter(AREA_NAME == "ZOMBA")


# poly_check(ADM1.1.Mal.sf, ADM1.DREAMS.Mal.sf.2.Blantyre)
# poly_check(ADM1.1.Mal.sf, ADM1.DREAMS.Mal.sf.2.Machinga)
# poly_check(ADM1.1.Mal.sf, ADM1.DREAMS.Mal.sf.2.Zomba)

##Create neighbors list
ADM1.1.Mal.sf$BlantyreNeighbor <- ifelse((sf::st_intersects(ADM1.1.Mal.sf,
                                                            ADM1.DREAMS.Mal.sf.2.Blantyre,
                                                            sparse = F)),
                                         1,
                                         0)

ADM1.1.Mal.sf$MachingaNeighbor <- ifelse((sf::st_intersects(ADM1.1.Mal.sf, 
                                                            ADM1.DREAMS.Mal.sf.2.Machinga,
                                                            sparse = F)),
                                         1,
                                         0)

ADM1.1.Mal.sf$ZombaNeighbor <- ifelse((sf::st_intersects(ADM1.1.Mal.sf, 
                                                         ADM1.DREAMS.Mal.sf.2.Zomba,
                                                         sparse = F)),
                                      1,
                                      0)


#sf_check(ADM1.1.Mal.sf)

ADM1.1.Mal.sf <- ADM1.1.Mal.sf %>%
  as.data.frame()

ADM1.1.Mal.sf$BlantyreNeighbor <- c(ADM1.1.Mal.sf$BlantyreNeighbor)
ADM1.1.Mal.sf$MachingaNeighbor <- c(ADM1.1.Mal.sf$MachingaNeighbor)
ADM1.1.Mal.sf$ZombaNeighbor <- c(ADM1.1.Mal.sf$ZombaNeighbor)


#Setup lookup table df to be used in filtering app map
neighborsLookupMalawi <- ADM1.1.Mal.sf


ADM1.1.Mal.sf$DREAMSneighbors <- ADM1.1.Mal.sf$BlantyreNeighbor + ADM1.1.Mal.sf$MachingaNeighbor + ADM1.1.Mal.sf$ZombaNeighbor

#Set DREAMSneighbors to zero for DREAMS ADM1s, this flags them for non-treatment later
ADM1.1.Mal.sf <- ADM1.1.Mal.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM1_NAME %in% DREAMS_Districts_Malawi) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )

#sf_check(ADM1.1.Mal.sf)

#Strip out remaining spatial parameters and unnecessary fields
ADM1.2.Mal.sf <- ADM1.1.Mal.sf %>% 
  select(-c("Shape_Leng", 
            "Shape_Area",
            "GENC_CODE",
            "FIPS_CODE",
            "ADM_LEVEL",
            "ADM2_NAME",
            #"NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry"))

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Malawi_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME", 
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.2.Other <- Demographic.2 %>%
  dplyr::filter(!(AREA_NAME %in% c("BLANTYRE", "BLANTYRE CITY", "ZOMBA", "ZOMBA CITY")))

Demographic.2.Blantyre <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("BLANTYRE", "BLANTYRE CITY")) %>%
  mutate(AREA_NAME = "BLANTYRE") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2019 = sum(F1014_2019),
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
            F2529_2022 = sum(F2529_2022)
  ) %>%
  mutate(ADM1_NAME = "BLANTYRE",
         ADM_LEVEL = 1)


Demographic.2.Zomba <- Demographic.2 %>%
  dplyr::filter(AREA_NAME %in% c("ZOMBA", "ZOMBA CITY")) %>%
  mutate(AREA_NAME = "ZOMBA") %>%
  group_by(AREA_NAME) %>%
  summarize(F1014_2019 = sum(F1014_2019),
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
            F2529_2022 = sum(F2529_2022)
  ) %>%
  mutate(ADM1_NAME = "ZOMBA",
         ADM_LEVEL = 1)

Demographic.3 <- bind_rows(Demographic.2.Other, 
                           Demographic.2.Blantyre, 
                           Demographic.2.Zomba)

Demographic.4 <- Demographic.3 %>%
  filter(ADM_LEVEL == 1) %>%
  select(-c("ADM_LEVEL"))

merged.00 <- merge(ADM1.2.Mal.sf, 
                   Demographic.4, 
                   by.x = "AREA_NAME", 
                   by.y = "AREA_NAME", 
                   all.x = TRUE) %>%
  dplyr::filter(AREA_NAME != "LAKE MALAWI") 

#Check for any mismatches between paired X and Y dataset shared fields

##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

merged.1c <- merged.1 %>%
  select(-c("BlantyreNeighbor",
            "MachingaNeighbor",
            "ZombaNeighbor",
            "ADM1_NAME.x",
            "ADM1_NAME.y",
            "GEO_MATCH.x",
            "GEO_MATCH.y",
            "DREAMSneighbors"))

##Create new pop total for each
#Take those that border Berea (inc. Berea itself and exc. other DREAMS districts)
### XXX WHY !is.na(F1014_2019)?
merged.1.Blantyre <- merged.1 %>%
  filter(BlantyreNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME %in% c("MACHINGA",
                                                                          "ZOMBA")))) %>%
  group_by(BlantyreNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Machinga <- merged.1 %>%
  filter(MachingaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME %in% c("BLANTYRE",
                                                                          "ZOMBA")))) %>%
  group_by(MachingaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Zomba <- merged.1 %>%
  filter(ZombaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME %in% c("BLANTYRE",
                                                                       "MACHINGA")))) %>%
  group_by(ZombaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Blantyre$AREA_NAME <- "BLANTYRE"
merged.1.Machinga$AREA_NAME <- "MACHINGA"
merged.1.Zomba$AREA_NAME <- "ZOMBA"


merged.2a <- rbind(merged.1.Blantyre,
                   merged.1.Machinga,
                   merged.1.Zomba)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1c)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

merged.7 <- merged.6 %>%
  filter(AREA_NAME %in% c("BLANTYRE",
                          "MACHINGA",
                          "ZOMBA"))


##Export Results
#For any manual checking
#write_excel_csv(merged.7, file = "LesothoOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.7,file = "preprocessing/data/MalawiOutput_Denominators.RDS")


# SOUTH AFRICA ----
###DENOMINATOR: Huge country, ADM1s dropped, ADM2s treated as ADM1s, ADM3s treated as ADM2s
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.SAf.sf <- st_read('preprocessing/data/South_Africa_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.SAf.sf <- st_read('preprocessing/data/South_Africa_adm3_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.SAf.sf, file = "data/SAfricaADM1.RDS")
saveRDS(ADM2.1.SAf.sf, file = "data/SAfricaADM2.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)

###NOTE: UTHUNGULU is PEPFAR kz King Cetshwayo District Municipality

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

ADM1.DREAMS.SAf.sf.1 <- ADM1.1.SAf.sf  %>%
  filter(ADM2_NAME %in% DREAMS_Districts_SouthAfrica)

ADM1.DREAMS.SAf.sf.2.ALFRED_NZO <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "ALFRED NZO")

ADM1.DREAMS.SAf.sf.2.BOJANALA <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "BOJANALA")

ADM1.DREAMS.SAf.sf.2.BUFFALO_CITY <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "BUFFALO CITY")

ADM1.DREAMS.SAf.sf.2.CAPRICORN <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "CAPRICORN")

ADM1.DREAMS.SAf.sf.2.CITY_OF_CAPE_TOWN <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "CITY OF CAPE TOWN")

ADM1.DREAMS.SAf.sf.2.CITY_OF_JOHANNESBURG <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "CITY OF JOHANNESBURG")

ADM1.DREAMS.SAf.sf.2.CITY_OF_TSHWANE <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "CITY OF TSHWANE")

ADM1.DREAMS.SAf.sf.2.DOCTOR_KENNETH_KAUNDA <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "DOCTOR KENNETH KAUNDA")

ADM1.DREAMS.SAf.sf.2.EHLANZENI <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "EHLANZENI")

ADM1.DREAMS.SAf.sf.2.EKURHULENI <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "EKURHULENI")

ADM1.DREAMS.SAf.sf.2.ETHEKWINI <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "ETHEKWINI")

ADM1.DREAMS.SAf.sf.2.GERT_SIBANDE <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "GERT SIBANDE")

ADM1.DREAMS.SAf.sf.2.LEJWELEPUTSWA <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "LEJWELEPUTSWA")

ADM1.DREAMS.SAf.sf.2.MOPANI <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "MOPANI")

ADM1.DREAMS.SAf.sf.2.NGAKA_MODIRI_MOLEMA <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "NGAKA MODIRI MOLEMA")

ADM1.DREAMS.SAf.sf.2.NKANGALA <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "NKANGALA")

ADM1.DREAMS.SAf.sf.2.OR_TAMBO <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "O.R. TAMBO")

ADM1.DREAMS.SAf.sf.2.SEDIBENG <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "SEDIBENG")

ADM1.DREAMS.SAf.sf.2.THABO_MOFUTSANYANE <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "THABO MOFUTSANYANE")

ADM1.DREAMS.SAf.sf.2.UGU <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "UGU")

ADM1.DREAMS.SAf.sf.2.UMGUNGUNDLOVU <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "UMGUNGUNDLOVU")

ADM1.DREAMS.SAf.sf.2.UTHUKELA <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "UTHUKELA")

ADM1.DREAMS.SAf.sf.2.UTHUNGULU <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "UTHUNGULU")

ADM1.DREAMS.SAf.sf.2.ZULULAND <- ADM1.DREAMS.SAf.sf.1 %>%
  filter(ADM2_NAME == "ZULULAND")

##Create neighbors list, does not make something it's own neighbor.
sf::sf_use_s2(FALSE)

ADM2.1.SAf.sf$ALFRED_NZONeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                              ADM1.DREAMS.SAf.sf.2.ALFRED_NZO,
                                                              sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                           1,
                                           0)

ADM2.1.SAf.sf$BOJANALANeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                            ADM1.DREAMS.SAf.sf.2.BOJANALA,
                                                            sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                         1,
                                         0)

ADM2.1.SAf.sf$BUFFALO_CITYNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                ADM1.DREAMS.SAf.sf.2.BUFFALO_CITY,
                                                                sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                             1,
                                             0)


ADM2.1.SAf.sf$CAPRICORNNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                             ADM1.DREAMS.SAf.sf.2.CAPRICORN,
                                                             sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                          1,
                                          0)

ADM2.1.SAf.sf$CITY_OF_CAPE_TOWNNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                     ADM1.DREAMS.SAf.sf.2.CITY_OF_CAPE_TOWN,
                                                                     sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                                  1,
                                                  0)

ADM2.1.SAf.sf$CITY_OF_JOHANNESBURGNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                        ADM1.DREAMS.SAf.sf.2.CITY_OF_JOHANNESBURG,
                                                                        sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                                     1,
                                                     0)

ADM2.1.SAf.sf$CITY_OF_TSHWANENeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                   ADM1.DREAMS.SAf.sf.2.CITY_OF_TSHWANE,
                                                                   sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                                1,
                                                0)

ADM2.1.SAf.sf$DOCTOR_KENNETH_KAUNDANeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                         ADM1.DREAMS.SAf.sf.2.DOCTOR_KENNETH_KAUNDA,
                                                                         sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                                      1,
                                                      0)

ADM2.1.SAf.sf$EHLANZENINeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                             ADM1.DREAMS.SAf.sf.2.EHLANZENI,
                                                             sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                          1,
                                          0)

ADM2.1.SAf.sf$EKURHULENINeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                              ADM1.DREAMS.SAf.sf.2.EKURHULENI,
                                                              sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                           1,
                                           0)

ADM2.1.SAf.sf$ETHEKWININeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                             ADM1.DREAMS.SAf.sf.2.ETHEKWINI,
                                                             sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                          1,
                                          0)

ADM2.1.SAf.sf$GERT_SIBANDENeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                ADM1.DREAMS.SAf.sf.2.GERT_SIBANDE,
                                                                sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                             1,
                                             0)

ADM2.1.SAf.sf$LEJWELEPUTSWANeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                 ADM1.DREAMS.SAf.sf.2.LEJWELEPUTSWA,
                                                                 sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                              1,
                                              0)

ADM2.1.SAf.sf$MOPANINeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                          ADM1.DREAMS.SAf.sf.2.MOPANI,
                                                          sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                       1,
                                       0)

ADM2.1.SAf.sf$NGAKA_MODIRI_MOLEMANeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                       ADM1.DREAMS.SAf.sf.2.NGAKA_MODIRI_MOLEMA,
                                                                       sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                                    1,
                                                    0)


ADM2.1.SAf.sf$NKANGALANeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                            ADM1.DREAMS.SAf.sf.2.NKANGALA,
                                                            sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                         1,
                                         0)

ADM2.1.SAf.sf$OR_TAMBONeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                            ADM1.DREAMS.SAf.sf.2.OR_TAMBO,
                                                            sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                         1,
                                         0)

ADM2.1.SAf.sf$SEDIBENGNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                            ADM1.DREAMS.SAf.sf.2.SEDIBENG,
                                                            sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                         1,
                                         0)

ADM2.1.SAf.sf$THABO_MOFUTSANYANENeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                      ADM1.DREAMS.SAf.sf.2.THABO_MOFUTSANYANE,
                                                                      sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                                   1,
                                                   0)

ADM2.1.SAf.sf$UGUNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                       ADM1.DREAMS.SAf.sf.2.UGU,
                                                       sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                    1,
                                    0)

ADM2.1.SAf.sf$UMGUNGUNDLOVUNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                                 ADM1.DREAMS.SAf.sf.2.UMGUNGUNDLOVU,
                                                                 sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                              1,
                                              0)


ADM2.1.SAf.sf$UTHUKELANeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                            ADM1.DREAMS.SAf.sf.2.UTHUKELA,
                                                            sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                         1,
                                         0)

ADM2.1.SAf.sf$UTHUNGULUNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                             ADM1.DREAMS.SAf.sf.2.UTHUNGULU,
                                                             sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                          1,
                                          0)

ADM2.1.SAf.sf$ZULULANDNeighbor <- ifelse((sf::st_intersects(ADM2.1.SAf.sf, 
                                                            ADM1.DREAMS.SAf.sf.2.ZULULAND,
                                                            sparse = F) & !(ADM2.1.SAf.sf$ADM2_NAME %in% DREAMS_Districts_SouthAfrica)),
                                         1,
                                         0)

sf::sf_use_s2(TRUE)
# 
# sf_check(ADM2.1.SAf.sf)
# # 
# poly_check(ADM2.1.SAf.sf, ADM1.DREAMS.SAf.sf.2.EKURHULENI)


#Re-adds ones from the district
ADM2.2.SAf.sf <- ADM2.1.SAf.sf

ADM2.2.SAf.sf$ALFRED_NZONeighbor  <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "ALFRED NZO" | ADM2.2.SAf.sf$ALFRED_NZONeighbor  == 1),
                                            1,
                                            0)

ADM2.2.SAf.sf$BOJANALANeighbor  <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "BOJANALA" | ADM2.2.SAf.sf$BOJANALANeighbor == 1),
                                          1,
                                          0)

ADM2.2.SAf.sf$BUFFALO_CITYNeighbor  <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "BUFFALO CITY" | ADM2.2.SAf.sf$BUFFALO_CITYNeighbor == 1),
                                              1,
                                              0)

ADM2.2.SAf.sf$CAPRICORNNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "CAPRICORN" | ADM2.2.SAf.sf$CAPRICORNNeighbor == 1),
                                          1,
                                          0)

ADM2.2.SAf.sf$CITY_OF_CAPE_TOWNNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "CITY OF CAPE TOWN" | ADM2.2.SAf.sf$CITY_OF_CAPE_TOWNNeighbor == 1),
                                                  1,
                                                  0)

ADM2.2.SAf.sf$CITY_OF_JOHANNESBURGNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "CITY OF JOHANNESBURG" | ADM2.2.SAf.sf$CITY_OF_JOHANNESBURGNeighbor == 1),
                                                     1,
                                                     0)

ADM2.2.SAf.sf$CITY_OF_TSHWANENeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "CITY OF TSHWANE" | ADM2.2.SAf.sf$CITY_OF_TSHWANENeighbor  == 1),
                                                1,
                                                0)

ADM2.2.SAf.sf$DOCTOR_KENNETH_KAUNDANeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "DOCTOR KENNETH KAUNDA" | ADM2.2.SAf.sf$DOCTOR_KENNETH_KAUNDANeighbor == 1),
                                                      1,
                                                      0)

ADM2.2.SAf.sf$EHLANZENINeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "EHLANZENI" | ADM2.2.SAf.sf$EHLANZENINeighbor == 1),
                                          1,
                                          0)

ADM2.2.SAf.sf$EKURHULENINeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "EKURHULENI" | ADM2.2.SAf.sf$EKURHULENINeighbor == 1),
                                           1,
                                           0)

ADM2.2.SAf.sf$ETHEKWININeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "ETHEKWINI" | ADM2.2.SAf.sf$ETHEKWININeighbor == 1),
                                          1,
                                          0)

ADM2.2.SAf.sf$GERT_SIBANDENeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "GERT SIBANDE" | ADM2.2.SAf.sf$GERT_SIBANDENeighbor == 1),
                                             1,
                                             0)

ADM2.2.SAf.sf$LEJWELEPUTSWANeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "LEJWELEPUTSWA" | ADM2.2.SAf.sf$LEJWELEPUTSWANeighbor  == 1),
                                              1,
                                              0)

ADM2.2.SAf.sf$MOPANINeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "MOPANI" | ADM2.2.SAf.sf$MOPANINeighbor == 1),
                                       1,
                                       0)

ADM2.2.SAf.sf$NGAKA_MODIRI_MOLEMANeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "NGAKA MODIRI MOLEMA" | ADM2.2.SAf.sf$NGAKA_MODIRI_MOLEMANeighbor == 1),
                                                    1,
                                                    0)

ADM2.2.SAf.sf$NKANGALANeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "NKANGALA" | ADM2.2.SAf.sf$NKANGALANeighbor == 1),
                                         1,
                                         0)

ADM2.2.SAf.sf$OR_TAMBONeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "O.R. TAMBO" | ADM2.2.SAf.sf$OR_TAMBONeighbor == 1),
                                         1,
                                         0)

ADM2.2.SAf.sf$SEDIBENGNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "SEDIBENG" | ADM2.2.SAf.sf$SEDIBENGNeighbor == 1),
                                         1,
                                         0)

ADM2.2.SAf.sf$THABO_MOFUTSANYANENeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "THABO MOFUTSANYANE" | ADM2.2.SAf.sf$THABO_MOFUTSANYANENeighbor  == 1),
                                                   1,
                                                   0)

ADM2.2.SAf.sf$UGUNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "UGU" | ADM2.2.SAf.sf$UGUNeighbor == 1),
                                    1,
                                    0)

ADM2.2.SAf.sf$UMGUNGUNDLOVUNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "UMGUNGUNDLOVU" | ADM2.2.SAf.sf$UMGUNGUNDLOVUNeighbor == 1),
                                              1,
                                              0)

ADM2.2.SAf.sf$UTHUKELANeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "UTHUKELA" | ADM2.2.SAf.sf$UTHUKELANeighbor == 1),
                                         1,
                                         0)

ADM2.2.SAf.sf$UTHUNGULUNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "UTHUNGULU" | ADM2.2.SAf.sf$UTHUNGULUNeighbor == 1),
                                          1,
                                          0)

ADM2.2.SAf.sf$ZULULANDNeighbor <- ifelse((ADM2.2.SAf.sf$ADM2_NAME == "ZULULAND" | ADM2.2.SAf.sf$ZULULANDNeighbor == 1),
                                         1,
                                         0)


ADM2.2.SAf.sf <- ADM2.2.SAf.sf %>%
  as.data.frame()

neighborsLookupSouthAfrica <- ADM2.2.SAf.sf

ADM2.2.SAf.sf$ALFRED_NZONeighbor <- c(ADM2.2.SAf.sf$ALFRED_NZONeighbor)
ADM2.2.SAf.sf$BOJANALANeighbor <- c(ADM2.2.SAf.sf$BOJANALANeighbor)
ADM2.2.SAf.sf$BUFFALO_CITYNeighbor <- c(ADM2.2.SAf.sf$BUFFALO_CITYNeighbor)
ADM2.2.SAf.sf$CAPRICORNNeighbor <- c(ADM2.2.SAf.sf$CAPRICORNNeighbor)
ADM2.2.SAf.sf$CITY_OF_CAPE_TOWNNeighbor <- c(ADM2.2.SAf.sf$CITY_OF_CAPE_TOWNNeighbor)
ADM2.2.SAf.sf$CITY_OF_JOHANNESBURGNeighbor <- c(ADM2.2.SAf.sf$CITY_OF_JOHANNESBURGNeighbor)
ADM2.2.SAf.sf$CITY_OF_TSHWANENeighbor <- c(ADM2.2.SAf.sf$CITY_OF_TSHWANENeighbor)
ADM2.2.SAf.sf$DOCTOR_KENNETH_KAUNDANeighbor <- c(ADM2.2.SAf.sf$DOCTOR_KENNETH_KAUNDANeighbor)
ADM2.2.SAf.sf$EHLANZENINeighbor <- c(ADM2.2.SAf.sf$EHLANZENINeighbor)
ADM2.2.SAf.sf$EKURHULENINeighbor <- c(ADM2.2.SAf.sf$EKURHULENINeighbor)
ADM2.2.SAf.sf$ETHEKWININeighbor <- c(ADM2.2.SAf.sf$ETHEKWININeighbor)
ADM2.2.SAf.sf$GERT_SIBANDENeighbor <- c(ADM2.2.SAf.sf$GERT_SIBANDENeighbor)
ADM2.2.SAf.sf$LEJWELEPUTSWANeighbor <- c(ADM2.2.SAf.sf$LEJWELEPUTSWANeighbor)
ADM2.2.SAf.sf$MOPANINeighbor <- c(ADM2.2.SAf.sf$MOPANINeighbor)
ADM2.2.SAf.sf$NGAKA_MODIRI_MOLEMANeighbor <- c(ADM2.2.SAf.sf$NGAKA_MODIRI_MOLEMANeighbor)
ADM2.2.SAf.sf$NKANGALANeighbor <- c(ADM2.2.SAf.sf$NKANGALANeighbor)
ADM2.2.SAf.sf$OR_TAMBONeighbor <- c(ADM2.2.SAf.sf$OR_TAMBONeighbor)
ADM2.2.SAf.sf$SEDIBENGNeighbor <- c(ADM2.2.SAf.sf$SEDIBENGNeighbor)
ADM2.2.SAf.sf$THABO_MOFUTSANYANENeighbor <- c(ADM2.2.SAf.sf$THABO_MOFUTSANYANENeighbor)
ADM2.2.SAf.sf$UGUNeighbor <- c(ADM2.2.SAf.sf$UGUNeighbor)
ADM2.2.SAf.sf$UMGUNGUNDLOVUNeighbor <- c(ADM2.2.SAf.sf$UMGUNGUNDLOVUNeighbor)
ADM2.2.SAf.sf$UTHUKELANeighbor <- c(ADM2.2.SAf.sf$UTHUKELANeighbor)
ADM2.2.SAf.sf$UTHUNGULUNeighbor <- c(ADM2.2.SAf.sf$UTHUNGULUNeighbor)
ADM2.2.SAf.sf$ZULULANDNeighbor <- c(ADM2.2.SAf.sf$ZULULANDNeighbor)

#sf_check(ADM2.2.Zim.sf)

ADM2.2.SAf.sf$DREAMSneighbors <- ADM2.2.SAf.sf$ALFRED_NZONeighbor +
  ADM2.2.SAf.sf$BOJANALANeighbor +
  ADM2.2.SAf.sf$BUFFALO_CITYNeighbor +
  ADM2.2.SAf.sf$CAPRICORNNeighbor +
  ADM2.2.SAf.sf$CITY_OF_CAPE_TOWNNeighbor +
  ADM2.2.SAf.sf$CITY_OF_JOHANNESBURGNeighbor +
  ADM2.2.SAf.sf$CITY_OF_TSHWANENeighbor +
  ADM2.2.SAf.sf$DOCTOR_KENNETH_KAUNDANeighbor +
  ADM2.2.SAf.sf$EHLANZENINeighbor +
  ADM2.2.SAf.sf$EKURHULENINeighbor +
  ADM2.2.SAf.sf$ETHEKWININeighbor +
  ADM2.2.SAf.sf$GERT_SIBANDENeighbor +
  ADM2.2.SAf.sf$LEJWELEPUTSWANeighbor +
  ADM2.2.SAf.sf$MOPANINeighbor +
  ADM2.2.SAf.sf$NGAKA_MODIRI_MOLEMANeighbor +
  ADM2.2.SAf.sf$NKANGALANeighbor +
  ADM2.2.SAf.sf$OR_TAMBONeighbor +
  ADM2.2.SAf.sf$SEDIBENGNeighbor +
  ADM2.2.SAf.sf$THABO_MOFUTSANYANENeighbor +
  ADM2.2.SAf.sf$UGUNeighbor +
  ADM2.2.SAf.sf$UMGUNGUNDLOVUNeighbor +
  ADM2.2.SAf.sf$UTHUKELANeighbor +
  ADM2.2.SAf.sf$UTHUNGULUNeighbor +
  ADM2.2.SAf.sf$ZULULANDNeighbor

ADM2.2.SAf.sf <- ADM2.2.SAf.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM2_NAME %in% DREAMS_Districts_SouthAfrica) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )

#sf_check(ADM2.2.Zim.sf)

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/SouthAfrica_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM3_NAME",
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 3) %>%
  select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.3.SAf.sf <- ADM2.2.SAf.sf %>% 
  select(-c("Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM2_NAME"))

merged.00 <- merge(ADM2.3.SAf.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
  select(-c("AREA_NAME.y",
            "ALFRED_NZONeighbor",
            "BOJANALANeighbor",
            "BUFFALO_CITYNeighbor",
            "CAPRICORNNeighbor",
            "CITY_OF_CAPE_TOWNNeighbor",
            "CITY_OF_JOHANNESBURGNeighbor",
            "CITY_OF_TSHWANENeighbor",
            "DOCTOR_KENNETH_KAUNDANeighbor",
            "EHLANZENINeighbor",
            "EKURHULENINeighbor",
            "ETHEKWININeighbor",
            "GERT_SIBANDENeighbor",
            "LEJWELEPUTSWANeighbor",
            "MOPANINeighbor",
            "NGAKA_MODIRI_MOLEMANeighbor",
            "NKANGALANeighbor",
            "OR_TAMBONeighbor",
            "SEDIBENGNeighbor",
            "THABO_MOFUTSANYANENeighbor",
            "UGUNeighbor",
            "UMGUNGUNDLOVUNeighbor",
            "UTHUKELANeighbor",
            "UTHUNGULUNeighbor",
            "ZULULANDNeighbor",
            "ADM1_NAME.y",
            "ADM3_NAME.y",
            "GEO_MATCH",
            "DREAMSneighbors")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM1_NAME=ADM1_NAME.x) %>%
  rename(ADM3_NAME=ADM3_NAME.x) %>%
  filter(ADM2_NAME %in% DREAMS_Districts_SouthAfrica)


##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

##Create new pop total for each
#Take those that border X (inc. X itself and exc. other DREAMS districts)
merged.1.ALFRED_NZO <- merged.1 %>%
  filter(ALFRED_NZONeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BOJANALA",
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
                                                                              "ZULULAND")))) %>%
  group_by(ALFRED_NZONeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.BOJANALA <- merged.1 %>%
  filter(BOJANALANeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                            "ZULULAND")))) %>%
  group_by(BOJANALANeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.BUFFALO_CITY <- merged.1 %>%
  filter(BUFFALO_CITYNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                                "BOJANALA",
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
                                                                                "ZULULAND")))) %>%
  group_by(BUFFALO_CITYNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.CAPRICORN <- merged.1 %>%
  filter(CAPRICORNNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                             "BOJANALA",
                                                                             "BUFFALO CITY",
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
                                                                             "ZULULAND")))) %>%
  group_by(CAPRICORNNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.CITY_OF_CAPE_TOWN <- merged.1 %>%
  filter(CITY_OF_CAPE_TOWNNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                                     "BOJANALA",
                                                                                     "BUFFALO CITY",
                                                                                     "CAPRICORN",
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
                                                                                     "ZULULAND")))) %>%
  group_by(CITY_OF_CAPE_TOWNNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.CITY_OF_JOHANNESBURG <- merged.1 %>%
  filter(CITY_OF_JOHANNESBURGNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                                        "BOJANALA",
                                                                                        "BUFFALO CITY",
                                                                                        "CAPRICORN",
                                                                                        "CITY OF CAPE TOWN",
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
                                                                                        "ZULULAND")))) %>%
  group_by(CITY_OF_JOHANNESBURGNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.CITY_OF_TSHWANE <- merged.1 %>%
  filter(CITY_OF_TSHWANENeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                                   "BOJANALA",
                                                                                   "BUFFALO CITY",
                                                                                   "CAPRICORN",
                                                                                   "CITY OF CAPE TOWN",
                                                                                   "CITY OF JOHANNESBURG",
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
                                                                                   "ZULULAND")))) %>%
  group_by(CITY_OF_TSHWANENeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.DOCTOR_KENNETH_KAUNDA <- merged.1 %>%
  filter(DOCTOR_KENNETH_KAUNDANeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                                         "BOJANALA",
                                                                                         "BUFFALO CITY",
                                                                                         "CAPRICORN",
                                                                                         "CITY OF CAPE TOWN",
                                                                                         "CITY OF JOHANNESBURG",
                                                                                         "CITY OF TSHWANE",
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
                                                                                         "ZULULAND")))) %>%
  group_by(DOCTOR_KENNETH_KAUNDANeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.EHLANZENI <- merged.1 %>%
  filter(EHLANZENINeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                             "BOJANALA",
                                                                             "BUFFALO CITY",
                                                                             "CAPRICORN",
                                                                             "CITY OF CAPE TOWN",
                                                                             "CITY OF JOHANNESBURG",
                                                                             "CITY OF TSHWANE",
                                                                             "DOCTOR KENNETH KAUNDA",
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
                                                                             "ZULULAND")))) %>%
  group_by(EHLANZENINeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.EKURHULENI <- merged.1 %>%
  filter(EKURHULENINeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                              "BOJANALA",
                                                                              "BUFFALO CITY",
                                                                              "CAPRICORN",
                                                                              "CITY OF CAPE TOWN",
                                                                              "CITY OF JOHANNESBURG",
                                                                              "CITY OF TSHWANE",
                                                                              "DOCTOR KENNETH KAUNDA",
                                                                              "EHLANZENI",
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
                                                                              "ZULULAND")))) %>%
  group_by(EKURHULENINeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.ETHEKWINI <- merged.1 %>%
  filter(ETHEKWININeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
                                                                             "BOJANALA",
                                                                             "BUFFALO CITY",
                                                                             "CAPRICORN",
                                                                             "CITY OF CAPE TOWN",
                                                                             "CITY OF JOHANNESBURG",
                                                                             "CITY OF TSHWANE",
                                                                             "DOCTOR KENNETH KAUNDA",
                                                                             "EHLANZENI",
                                                                             "EKURHULENI",
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
                                                                             "ZULULAND")))) %>%
  group_by(ETHEKWININeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.GERT_SIBANDE <- merged.1 %>%
  filter(GERT_SIBANDENeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                                "ZULULAND")))) %>%
  group_by(GERT_SIBANDENeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.LEJWELEPUTSWA <- merged.1 %>%
  filter(LEJWELEPUTSWANeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                                 "ZULULAND")))) %>%
  group_by(LEJWELEPUTSWANeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.MOPANI <- merged.1 %>%
  filter(MOPANINeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                          "NGAKA MODIRI MOLEMA",
                                                                          "NKANGALA",
                                                                          "O.R. TAMBO",
                                                                          "SEDIBENG",
                                                                          "THABO MOFUTSANYANE",
                                                                          "UGU",
                                                                          "UMGUNGUNDLOVU",
                                                                          "UTHUKELA",
                                                                          "UTHUNGULU",
                                                                          "ZULULAND")))) %>%
  group_by(MOPANINeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.NGAKA_MODIRI_MOLEMA <- merged.1 %>%
  filter(NGAKA_MODIRI_MOLEMANeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                                       "NKANGALA",
                                                                                       "O.R. TAMBO",
                                                                                       "SEDIBENG",
                                                                                       "THABO MOFUTSANYANE",
                                                                                       "UGU",
                                                                                       "UMGUNGUNDLOVU",
                                                                                       "UTHUKELA",
                                                                                       "UTHUNGULU",
                                                                                       "ZULULAND")))) %>%
  group_by(NGAKA_MODIRI_MOLEMANeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.NKANGALA <- merged.1 %>%
  filter(NKANGALANeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                            "O.R. TAMBO",
                                                                            "SEDIBENG",
                                                                            "THABO MOFUTSANYANE",
                                                                            "UGU",
                                                                            "UMGUNGUNDLOVU",
                                                                            "UTHUKELA",
                                                                            "UTHUNGULU",
                                                                            "ZULULAND")))) %>%
  group_by(NKANGALANeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.OR_TAMBO <- merged.1 %>%
  filter(OR_TAMBONeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                            "SEDIBENG",
                                                                            "THABO MOFUTSANYANE",
                                                                            "UGU",
                                                                            "UMGUNGUNDLOVU",
                                                                            "UTHUKELA",
                                                                            "UTHUNGULU",
                                                                            "ZULULAND")))) %>%
  group_by(OR_TAMBONeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.SEDIBENG <- merged.1 %>%
  filter(SEDIBENGNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                            "THABO MOFUTSANYANE",
                                                                            "UGU",
                                                                            "UMGUNGUNDLOVU",
                                                                            "UTHUKELA",
                                                                            "UTHUNGULU",
                                                                            "ZULULAND")))) %>%
  group_by(SEDIBENGNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.THABO_MOFUTSANYANE <- merged.1 %>%
  filter(THABO_MOFUTSANYANENeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                                      "UGU",
                                                                                      "UMGUNGUNDLOVU",
                                                                                      "UTHUKELA",
                                                                                      "UTHUNGULU",
                                                                                      "ZULULAND")))) %>%
  group_by(THABO_MOFUTSANYANENeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.UGU <- merged.1 %>%
  filter(UGUNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                       "UMGUNGUNDLOVU",
                                                                       "UTHUKELA",
                                                                       "UTHUNGULU",
                                                                       "ZULULAND")))) %>%
  group_by(UGUNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.UMGUNGUNDLOVU <- merged.1 %>%
  filter(UMGUNGUNDLOVUNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                                 "UTHUKELA",
                                                                                 "UTHUNGULU",
                                                                                 "ZULULAND")))) %>%
  group_by(UMGUNGUNDLOVUNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.UTHUKELA <- merged.1 %>%
  filter(UTHUKELANeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                            "UTHUNGULU",
                                                                            "ZULULAND")))) %>%
  group_by(UTHUKELANeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.UTHUNGULU <- merged.1 %>%
  filter(UTHUNGULUNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                             "ZULULAND")))) %>%
  group_by(UTHUNGULUNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.ZULULAND <- merged.1 %>%
  filter(ZULULANDNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("ALFRED NZO",
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
                                                                            "UTHUNGULU")))) %>%
  group_by(ZULULANDNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.ALFRED_NZO$AREA_NAME <- "ALFRED NZO"
merged.1.BOJANALA$AREA_NAME <- "BOJANALA"
merged.1.BUFFALO_CITY$AREA_NAME <- "BUFFALO CITY"
merged.1.CAPRICORN$AREA_NAME <- "CAPRICORN"
merged.1.CITY_OF_CAPE_TOWN$AREA_NAME <- "CITY OF CAPE TOWN"
merged.1.CITY_OF_JOHANNESBURG$AREA_NAME <- "CITY OF JOHANNESBURG"
merged.1.CITY_OF_TSHWANE$AREA_NAME <- "CITY OF TSHWANE"
merged.1.DOCTOR_KENNETH_KAUNDA$AREA_NAME <- "DOCTOR KENNETH KAUNDA"
merged.1.EHLANZENI$AREA_NAME <- "EHLANZENI"
merged.1.EKURHULENI$AREA_NAME <- "EKURHULENI"
merged.1.ETHEKWINI$AREA_NAME <- "ETHEKWINI"
merged.1.GERT_SIBANDE$AREA_NAME <- "GERT SIBANDE"
merged.1.LEJWELEPUTSWA$AREA_NAME <- "LEJWELEPUTSWA"
merged.1.MOPANI$AREA_NAME <- "MOPANI"
merged.1.NGAKA_MODIRI_MOLEMA$AREA_NAME <- "NGAKA MODIRI MOLEMA"
merged.1.NKANGALA$AREA_NAME <- "NKANGALA"
merged.1.OR_TAMBO$AREA_NAME <- "O.R. TAMBO"
merged.1.SEDIBENG$AREA_NAME <- "SEDIBENG"
merged.1.THABO_MOFUTSANYANE$AREA_NAME <- "THABO MOFUTSANYANE"
merged.1.UGU$AREA_NAME <- "UGU"
merged.1.UMGUNGUNDLOVU$AREA_NAME <- "UMGUNGUNDLOVU"
merged.1.UTHUKELA$AREA_NAME <- "UTHUKELA"
merged.1.UTHUNGULU$AREA_NAME <- "UTHUNGULU"
merged.1.ZULULAND$AREA_NAME <- "ZULULAND"


merged.2a <- rbind(merged.1.ALFRED_NZO,
                   merged.1.BOJANALA,
                   merged.1.BUFFALO_CITY,
                   merged.1.CAPRICORN,
                   merged.1.CITY_OF_CAPE_TOWN,
                   merged.1.CITY_OF_JOHANNESBURG,
                   merged.1.CITY_OF_TSHWANE,
                   merged.1.DOCTOR_KENNETH_KAUNDA,
                   merged.1.EHLANZENI,
                   merged.1.EKURHULENI,
                   merged.1.ETHEKWINI,
                   merged.1.GERT_SIBANDE,
                   merged.1.LEJWELEPUTSWA,
                   merged.1.MOPANI,
                   merged.1.NGAKA_MODIRI_MOLEMA,
                   merged.1.NKANGALA,
                   merged.1.OR_TAMBO,
                   merged.1.SEDIBENG,
                   merged.1.THABO_MOFUTSANYANE,
                   merged.1.UGU,
                   merged.1.UMGUNGUNDLOVU,
                   merged.1.UTHUKELA,
                   merged.1.UTHUNGULU,
                   merged.1.ZULULAND)

merged.1d <- merged.1c %>%
  filter(ADM2_NAME %in% DREAMS_Districts_SouthAfrica) %>%
  group_by(ADM2_NAME) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = ADM2_NAME)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1d)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

##Export Results
#For any manual checking
#write_excel_csv(merged.6, file = "SouthAfricaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.6, file = "preprocessing/data/SouthAfricaOutput_Denominators.RDS")



# TANZANIA ----
###DENOMINATOR: Huge country, ADM1s dropped, ADM2s treated as ADM1s, ADM3s treated as ADM2s
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Tan.sf <- st_read('preprocessing/data/Tanzania_adm1_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Tan.sf <- st_read('preprocessing/data/Tanzania_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Tan.sf, file = "data/TanzaniaADM1.RDS")
saveRDS(ADM2.1.Tan.sf, file = "data/TanzaniaADM2.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)

DREAMS_Districts_Tanzania <- c("MWANZA",
                               "KAGERA")

ADM1.DREAMS.Tan.sf.1 <- ADM1.1.Tan.sf  %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Tanzania) %>%
  filter(ADM1_NAME != "LAKE VICTORIA")

ADM1.DREAMS.Tan.sf.2.Mwanza <- ADM1.DREAMS.Tan.sf.1 %>%
  filter(AREA_NAME == "MWANZA")

ADM1.DREAMS.Tan.sf.2.Kagera <- ADM1.DREAMS.Tan.sf.1 %>%
  filter(AREA_NAME == "KAGERA")

# poly_check(ADM2.1.Tan.sf, ADM2.1.Tan.sf)
# poly_check(ADM1.1.Tan.sf, ADM1.DREAMS.Tan.sf.2.Kagera)
# 
# sf_check(ADM2.1.Tan.sf)

##Create neighbors list, does not make something it's own neighbor
sf::sf_use_s2(FALSE)

ADM2.1.Tan.sf$MwanzaNeighbor <- ifelse((sf::st_intersects(ADM2.1.Tan.sf, 
                                                          ADM1.DREAMS.Tan.sf.2.Mwanza,
                                                          sparse = F) & !(ADM2.1.Tan.sf$ADM1_NAME %in% DREAMS_Districts_Tanzania)),
                                       1,
                                       0)

ADM2.1.Tan.sf$KageraNeighbor <- ifelse((sf::st_intersects(ADM2.1.Tan.sf, 
                                                          ADM1.DREAMS.Tan.sf.2.Kagera,
                                                          sparse = F) & !(ADM2.1.Tan.sf$ADM1_NAME %in% DREAMS_Districts_Tanzania)),
                                       1,
                                       0)

sf::sf_use_s2(TRUE)


# sf_check(ADM2.1.Tan.sf)
# 
# poly_check(ADM1.1.Tan.sf, ADM1.DREAMS.Tan.sf.2.Mwanza)
# 
# test1 <- ADM2.1.Tan.sf %>%
#   filter(MwanzaNeighbor == 1)
# 
# poly_check(ADM1.1.Tan.sf, test1)
# 
# poly_check(ADM1.DREAMS.Tan.sf.2.Mwanza, test1)


#Re-adds ones from the district
ADM2.2.Tan.sf <- ADM2.1.Tan.sf

ADM2.2.Tan.sf$MwanzaNeighbor <- ifelse((ADM2.2.Tan.sf$ADM1_NAME == "MWANZA" | ADM2.2.Tan.sf$MwanzaNeighbor == 1),
                                       1,
                                       0)

ADM2.2.Tan.sf$KageraNeighbor <- ifelse((ADM2.2.Tan.sf$ADM1_NAME == "KAGERA" | ADM2.2.Tan.sf$KageraNeighbor == 1),
                                       1,
                                       0)

# sf_check(ADM2.2.Bot.sf)
# 
# test1 <- ADM2.2.Bot.sf %>%
#   filter(SouthernNeighbor == 1)
# 
# poly_check(ADM1.1.Bot.sf, test1)
# 
# poly_check(ADM1.DREAMS.Bot.sf.2.Southern, test1)

ADM2.2.Tan.sf <- ADM2.2.Tan.sf %>%
  as.data.frame()

neighborsLookupTanzania <- ADM2.2.Tan.sf

ADM2.2.Tan.sf$MwanzaNeighbor <- c(ADM2.2.Tan.sf$MwanzaNeighbor)
ADM2.2.Tan.sf$KageraNeighbor <- c(ADM2.2.Tan.sf$KageraNeighbor)

ADM2.2.Tan.sf$DREAMSneighbors <- ADM2.2.Tan.sf$MwanzaNeighbor + ADM2.2.Tan.sf$KageraNeighbor

ADM2.2.Tan.sf <- ADM2.2.Tan.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM1_NAME %in% DREAMS_Districts_Tanzania) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Tanzania_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
  select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.3.Tan.sf <- ADM2.2.Tan.sf %>% 
  select(-c("Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "ADM2_NAME"))

merged.00 <- merge(ADM2.3.Tan.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
  select(-c("AREA_NAME.y",
            "MwanzaNeighbor",
            "KageraNeighbor",
            "ADM1_NAME.y",
            "ADM2_NAME",
            "GEO_MATCH",
            "DREAMSneighbors",
            "NSO_NAME",
            "USCBCMNT")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM1_NAME=ADM1_NAME.x) %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Tanzania)


##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

##Create new pop total for each
#Take those that border X (inc. X itself and exc. other DREAMS districts)
### XXX WHY !is.na(F1014_2019)?
merged.1.Mwanza <- merged.1 %>%
  filter(MwanzaNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("KAGERA")))) %>%
  group_by(MwanzaNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Kagera <- merged.1 %>%
  filter(KageraNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("MWANZA")))) %>%
  group_by(KageraNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Mwanza$AREA_NAME <- "MWANZA"
merged.1.Kagera$AREA_NAME <- "KAGERA"

merged.2a <- rbind(merged.1.Mwanza,
                   merged.1.Kagera)

merged.1d <- merged.1c %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Tanzania) %>%
  group_by(ADM1_NAME) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = ADM1_NAME)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1d)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

##Export Results
#For any manual checking
#write_excel_csv(merged.6, file = "BotswanaOutput_Denominators.csv") #purely for external inspection
#For import to app
saveRDS(merged.6, file = "preprocessing/data/TanzaniaOutput_Denominators.RDS")

# ZIMBABWE ----
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Zim.sf <- st_read('preprocessing/data/Zimbabwe_adm1_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Zim.sf <- st_read('preprocessing/data/Zimbabwe_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Zim.sf, file = "data/ZimbabweADM1.RDS")
saveRDS(ADM2.1.Zim.sf, file = "data/ZimbabweADM2.RDS")

#NOTE Can possibly eliminate this step and use the "NSO_NAME" field instead (if it's consistently a match)

DREAMS_Districts_Zimbabwe <- c("BULAWAYO", 
                               "MANICALAND", 
                               "MASHONALAND CENTRAL", 
                               "MATABELELAND NORTH", 
                               "MATABELELAND SOUTH", 
                               "MIDLANDS")

ADM1.DREAMS.Zim.sf.1 <- ADM1.1.Zim.sf  %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Zimbabwe)

ADM1.DREAMS.Zim.sf.2.Bulawayo <- ADM1.DREAMS.Zim.sf.1%>%
  filter(AREA_NAME == "BULAWAYO")

ADM1.DREAMS.Zim.sf.2.Manicaland <- ADM1.DREAMS.Zim.sf.1%>%
  filter(AREA_NAME == "MANICALAND")

ADM1.DREAMS.Zim.sf.2.MCentral <- ADM1.DREAMS.Zim.sf.1%>%
  filter(AREA_NAME == "MASHONALAND CENTRAL")

ADM1.DREAMS.Zim.sf.2.MNorth <- ADM1.DREAMS.Zim.sf.1%>%
  filter(AREA_NAME == "MATABELELAND NORTH")

ADM1.DREAMS.Zim.sf.2.MSouth <- ADM1.DREAMS.Zim.sf.1%>%
  filter(AREA_NAME == "MATABELELAND SOUTH")

ADM1.DREAMS.Zim.sf.2.Midlands <- ADM1.DREAMS.Zim.sf.1%>%
  filter(AREA_NAME == "MIDLANDS")
# 
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.Bulawayo)
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.Manicaland)
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.MCentral)
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.MNorth)
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.MSouth)
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.Midlands)
# sf_check(ADM1.1.Zim.sf)

##Create neighbors list, does not make something it's own neighbor.
ADM2.1.Zim.sf$BulawayoNeighbor <- ifelse((sf::st_intersects(ADM2.1.Zim.sf, 
                                                            ADM1.DREAMS.Zim.sf.2.Bulawayo,
                                                            sparse = F) & !(ADM2.1.Zim.sf$ADM1_NAME %in% DREAMS_Districts_Zimbabwe)),
                                         1,
                                         0)

ADM2.1.Zim.sf$ManicalandNeighbor <- ifelse((sf::st_intersects(ADM2.1.Zim.sf, 
                                                              ADM1.DREAMS.Zim.sf.2.Manicaland,
                                                              sparse = F) & !(ADM2.1.Zim.sf$ADM1_NAME %in% DREAMS_Districts_Zimbabwe)),
                                           1,
                                           0)

ADM2.1.Zim.sf$MCentralNeighbor <- ifelse((sf::st_intersects(ADM2.1.Zim.sf, 
                                                            ADM1.DREAMS.Zim.sf.2.MCentral,
                                                            sparse = F) & !(ADM2.1.Zim.sf$ADM1_NAME %in% DREAMS_Districts_Zimbabwe)),
                                         1,
                                         0)


ADM2.1.Zim.sf$MNorthNeighbor <- ifelse((sf::st_intersects(ADM2.1.Zim.sf, 
                                                          ADM1.DREAMS.Zim.sf.2.MNorth,
                                                          sparse = F) & !(ADM2.1.Zim.sf$ADM1_NAME %in% DREAMS_Districts_Zimbabwe)),
                                       1,
                                       0)

ADM2.1.Zim.sf$MSouthNeighbor <- ifelse((sf::st_intersects(ADM2.1.Zim.sf, 
                                                          ADM1.DREAMS.Zim.sf.2.MSouth,
                                                          sparse = F) & !(ADM2.1.Zim.sf$ADM1_NAME %in% DREAMS_Districts_Zimbabwe)),
                                       1,
                                       0)

ADM2.1.Zim.sf$MidlandsNeighbor <- ifelse((sf::st_intersects(ADM2.1.Zim.sf, 
                                                            ADM1.DREAMS.Zim.sf.2.Midlands,
                                                            sparse = F) & !(ADM2.1.Zim.sf$ADM1_NAME %in% DREAMS_Districts_Zimbabwe)),
                                         1,
                                         0)

# sf_check(ADM2.1.Zim.sf)
# 
# poly_check(ADM1.1.Zim.sf, ADM1.DREAMS.Zim.sf.2.Midlands)

# test1 <- ADM2.1.Zim.sf %>%
#   filter(MNorthNeighbor == 1)
# 
# poly_check(ADM1.1.Zim.sf, test1)
# 
# poly_check(ADM1.DREAMS.Zim.sf.2.MNorth, test1)


#Re-adds ones from the district
ADM2.2.Zim.sf <- ADM2.1.Zim.sf

ADM2.2.Zim.sf$BulawayoNeighbor <- ifelse((ADM2.2.Zim.sf$ADM1_NAME == "BULAWAYO" | ADM2.2.Zim.sf$BulawayoNeighbor == 1),
                                         1,
                                         0)

ADM2.2.Zim.sf$ManicalandNeighbor <- ifelse((ADM2.2.Zim.sf$ADM1_NAME == "MANICALAND" | ADM2.2.Zim.sf$ManicalandNeighbor == 1),
                                           1,
                                           0)

ADM2.2.Zim.sf$MCentralNeighbor <- ifelse((ADM2.2.Zim.sf$ADM1_NAME == "MASHONALAND CENTRAL" | ADM2.2.Zim.sf$MCentralNeighbor == 1),
                                         1,
                                         0)

ADM2.2.Zim.sf$MNorthNeighbor <- ifelse((ADM2.2.Zim.sf$ADM1_NAME == "MATABELELAND NORTH" | ADM2.2.Zim.sf$MNorthNeighbor == 1),
                                       1,
                                       0)

ADM2.2.Zim.sf$MSouthNeighbor <- ifelse((ADM2.2.Zim.sf$ADM1_NAME == "MATABELELAND SOUTH" | ADM2.2.Zim.sf$MSouthNeighbor == 1),
                                       1,
                                       0)

ADM2.2.Zim.sf$MidlandsNeighbor <- ifelse((ADM2.2.Zim.sf$ADM1_NAME == "MIDLANDS" | ADM2.2.Zim.sf$MidlandsNeighbor == 1),
                                         1,
                                         0)

# sf_check(ADM2.2.Zim.sf)
# 
# test1 <- ADM2.2.Zim.sf %>%
#   filter(MNorthNeighbor == 1)
# 
# poly_check(ADM1.1.Zim.sf, test1)
# 
# poly_check(ADM1.DREAMS.Zim.sf.2.MNorth, test1)


# sf_check(ADM2.2.Zim.sf)

ADM2.2.Zim.sf <- ADM2.2.Zim.sf %>%
  as.data.frame()

neighborsLookupZimbabwe <- ADM2.2.Zim.sf

ADM2.2.Zim.sf$BulawayoNeighbor <- c(ADM2.2.Zim.sf$BulawayoNeighbor)
ADM2.2.Zim.sf$ManicalandNeighbor <- c(ADM2.2.Zim.sf$ManicalandNeighbor)
ADM2.2.Zim.sf$MCentralNeighbor <- c(ADM2.2.Zim.sf$MCentralNeighbor)
ADM2.2.Zim.sf$MNorthNeighbor <- c(ADM2.2.Zim.sf$MNorthNeighbor)
ADM2.2.Zim.sf$MSouthNeighbor <- c(ADM2.2.Zim.sf$MSouthNeighbor)
ADM2.2.Zim.sf$MidlandsNeighbor <- c(ADM2.2.Zim.sf$MidlandsNeighbor)

#sf_check(ADM2.2.Zim.sf)

ADM2.2.Zim.sf$DREAMSneighbors <- ADM2.2.Zim.sf$BulawayoNeighbor + ADM2.2.Zim.sf$ManicalandNeighbor + ADM2.2.Zim.sf$MCentralNeighbor + ADM2.2.Zim.sf$MNorthNeighbor + ADM2.2.Zim.sf$MSouthNeighbor + ADM2.2.Zim.sf$MidlandsNeighbor

ADM2.2.Zim.sf <- ADM2.2.Zim.sf %>%
  mutate(
    DREAMSneighbors = case_when(
      (ADM1_NAME %in% DREAMS_Districts_Zimbabwe) ~ 0,
      TRUE ~ as.numeric(DREAMSneighbors)
    )
  )
#sf_check(ADM2.2.Zim.sf)

##Attach demographic data
Demographic.1 <- readxl::read_xlsx("preprocessing/data/Zimbabwe_USCB.xlsx")

Demographic.2 <- Demographic.1 %>%
  select(c("AREA_NAME", 
           "GEO_MATCH", 
           "ADM1_NAME",
           "ADM2_NAME",
           "ADM_LEVEL", 
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
           "F2529_2022"))

Demographic.3 <- Demographic.2 %>%
  filter(ADM_LEVEL == 2) %>%
  select(-c("ADM_LEVEL"))

#Strip out remaining spatial parameters and unnecessary fields
ADM2.3.Zim.sf <- ADM2.2.Zim.sf %>% 
  select(-c("USCBCMNT",
            "Shape_Leng", 
            "Shape_Area",
            "ADM_LEVEL",
            "NSO_CODE",
            "GEO_CONCAT",
            "CNTRY_NAME",
            "geometry",
            "NSO_NAME",
            "ADM2_NAME"))

merged.00 <- merge(ADM2.3.Zim.sf, 
                   Demographic.3, 
                   by.x = "GEO_MATCH", 
                   by.y = "GEO_MATCH", 
                   all.x = TRUE)

#Stop here and visually check for any mismatches between paired X and Y dataset shared fields

##Split off original figures for later re-join
merged.1c <- merged.00 %>%
  select(-c("AREA_NAME.y",
            "BulawayoNeighbor",
            "ManicalandNeighbor",
            "MCentralNeighbor",
            "MNorthNeighbor",
            "MSouthNeighbor",
            "MidlandsNeighbor",
            "ADM1_NAME.y",
            "ADM2_NAME",
            "GEO_MATCH",
            "DREAMSneighbors")) %>%
  rename(AREA_NAME=AREA_NAME.x) %>%
  rename(ADM1_NAME=ADM1_NAME.x) %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Zimbabwe)


##Adjust for multiple DREAMS neighbors.
#Takes non-dreams districts, divides their population by the number of adjoining DREAMS districts
#for apportionment between those districts

merged.0a <- merged.00 %>%
  filter(DREAMSneighbors == 0)
merged.0b <- merged.00 %>%
  filter(DREAMSneighbors != 0)

merged.0b$F1014_2019 <- round((merged.0b$F1014_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2019 <- round((merged.0b$F1519_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2019 <- round((merged.0b$F2024_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2019 <- round((merged.0b$F2529_2019/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2020 <- round((merged.0b$F1014_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2020 <- round((merged.0b$F1519_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2020 <- round((merged.0b$F2024_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2020 <- round((merged.0b$F2529_2020/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2021 <- round((merged.0b$F1014_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2021 <- round((merged.0b$F1519_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2021 <- round((merged.0b$F2024_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2021 <- round((merged.0b$F2529_2021/merged.0b$DREAMSneighbors),0)
merged.0b$F1014_2022 <- round((merged.0b$F1014_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F1519_2022 <- round((merged.0b$F1519_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2024_2022 <- round((merged.0b$F2024_2022/merged.0b$DREAMSneighbors),0)
merged.0b$F2529_2022 <- round((merged.0b$F2529_2022/merged.0b$DREAMSneighbors),0)

merged.1 <- rbind(merged.0a,
                  merged.0b)

##Create new pop total for each
#Take those that border Berea (inc. Berea itself and exc. other DREAMS districts)
### XXX WHY !is.na(F1014_2019)?
merged.1.Bulawayo <- merged.1 %>%
  filter(BulawayoNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("MANICALAND", 
                                                                            "MASHONALAND CENTRAL", 
                                                                            "MATABELELAND NORTH", 
                                                                            "MATABELELAND SOUTH", 
                                                                            "MIDLANDS")))) %>%
  group_by(BulawayoNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Manicaland <- merged.1 %>%
  filter(ManicalandNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BULAWAYO", 
                                                                              "MASHONALAND CENTRAL", 
                                                                              "MATABELELAND NORTH", 
                                                                              "MATABELELAND SOUTH", 
                                                                              "MIDLANDS")))) %>%
  group_by(ManicalandNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.MCentral <- merged.1 %>%
  filter(MCentralNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BULAWAYO", 
                                                                            "MANICALAND", 
                                                                            "MATABELELAND NORTH", 
                                                                            "MATABELELAND SOUTH", 
                                                                            "MIDLANDS")))) %>%
  group_by(MCentralNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.MNorth <- merged.1 %>%
  filter(MNorthNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BULAWAYO", 
                                                                          "MANICALAND", 
                                                                          "MASHONALAND CENTRAL", 
                                                                          "MATABELELAND SOUTH", 
                                                                          "MIDLANDS")))) %>%
  group_by(MNorthNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.MSouth <- merged.1 %>%
  filter(MSouthNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BULAWAYO", 
                                                                          "MANICALAND", 
                                                                          "MASHONALAND CENTRAL", 
                                                                          "MATABELELAND NORTH", 
                                                                          "MIDLANDS")))) %>%
  group_by(MSouthNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Midlands <- merged.1 %>%
  filter(MidlandsNeighbor == 1 & !is.na(F1014_2019) & (!(AREA_NAME.x %in% c("BULAWAYO", 
                                                                            "MANICALAND", 
                                                                            "MASHONALAND CENTRAL", 
                                                                            "MATABELELAND NORTH", 
                                                                            "MATABELELAND SOUTH")))) %>%
  group_by(MidlandsNeighbor) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = 1)

merged.1.Bulawayo$AREA_NAME <- "BULAWAYO"
merged.1.Manicaland$AREA_NAME <- "MANICALAND"
merged.1.MCentral$AREA_NAME <- "MASHONALAND CENTRAL"
merged.1.MNorth$AREA_NAME <- "MATABELELAND NORTH"
merged.1.MSouth$AREA_NAME <- "MATABELELAND SOUTH"
merged.1.Midlands$AREA_NAME <- "MIDLANDS"

merged.2a <- rbind(merged.1.Bulawayo,
                   merged.1.Manicaland,
                   merged.1.MCentral,
                   merged.1.MNorth,
                   merged.1.MSouth,
                   merged.1.Midlands)

merged.1d <- merged.1c %>%
  filter(ADM1_NAME %in% DREAMS_Districts_Zimbabwe) %>%
  group_by(ADM1_NAME) %>%
  summarize(F1014_2019=sum(F1014_2019),
            F1519_2019=sum(F1519_2019),
            F2024_2019=sum(F2024_2019),
            F2529_2019=sum(F2529_2019),
            F1014_2020=sum(F1014_2020),
            F1519_2020=sum(F1519_2020),
            F2024_2020=sum(F2024_2020),
            F2529_2020=sum(F2529_2020),
            F1014_2021=sum(F1014_2021),
            F1519_2021=sum(F1519_2021),
            F2024_2021=sum(F2024_2021),
            F2529_2021=sum(F2529_2021),
            F1014_2022=sum(F1014_2022),
            F1519_2022=sum(F1519_2022),
            F2024_2022=sum(F2024_2022),
            F2529_2022=sum(F2529_2022)
  ) %>%
  rename(AREA_NAME = ADM1_NAME)

merged.3a <-pivot_step1(merged.2a)
merged.3b <-pivot_step1(merged.1d)

merged.3a$populationtx <- "Expanded"
merged.3b$populationtx <- "DistrictOnly"

merged.4a <-pivot_step2(merged.3a)
merged.4b <-pivot_step2(merged.3b)

merged.5a <- adjust_ages(merged.4a)
merged.5b <- adjust_ages(merged.4b)

merged.6 <- rbind(merged.5a,
                  merged.5b)

##Export Results
#For any manual checking
#write_excel_csv(merged.6, file = "ZimbabweOutput_Denominators.csv")  #purely for external inspection
#For import to app
saveRDS(merged.6,file = "preprocessing/data/ZimbabweOutput_Denominators.RDS")


# COMBINE AND PREPROCESS ----
Bot_Data <- readRDS('preprocessing/data/BotswanaOutput_Denominators.RDS') %>%
  mutate(country = "Botswana")
Ken_Data <- readRDS('preprocessing/data/KenyaOutput_Denominators.RDS') %>%
  mutate(country = "Kenya")
Les_Data <- readRDS('preprocessing/data/LesothoOutput_Denominators.RDS') %>%
  mutate(country = "Lesotho")
Mal_Data <- readRDS('preprocessing/data/MalawiOutput_Denominators.RDS') %>%
  mutate(country = "Malawi")
SAf_Data <- readRDS('preprocessing/data/SouthAfricaOutput_Denominators.RDS') %>%
  mutate(country = "South Africa")
Tan_Data <- readRDS('preprocessing/data/TanzaniaOutput_Denominators.RDS') %>%
  mutate(country = "Tanzania")
Zim_Data <- readRDS('preprocessing/data/ZimbabweOutput_Denominators.RDS') %>%
  mutate(country = "Zimbabwe")

countryData <- rbind(Bot_Data,
                     Ken_Data,
                     Les_Data,
                     Mal_Data,
                     SAf_Data,
                     Tan_Data,
                     Zim_Data) 

countryData <- countryData %>%
  pivot_longer(
    cols = `2019`:`2022`,
    names_to = c("fiscal_year"), 
    names_prefix = "F",
    values_to = "population"
  )

countryData$fiscal_year <- countryData$fiscal_year %>%
  as.numeric()

saveRDS(countryData, file = "data/countryData.RDS")


# NEIGHBORS LOOKUP PREPROCESSING ----

## Original version (nixed b/c too 'inelegant') ----
# 
# neighborsLesotho_Berea <- neighborsLookupLesotho %>%
#   filter(BereaNeighbor == 1 & (!AREA_NAME %in% DREAMS_Districts_Lesotho))
# 
# neighborsLesotho_Mafeteng <- neighborsLookupLesotho %>%
#   filter(MafetengNeighbor == 1 & (!AREA_NAME %in% DREAMS_Districts_Lesotho))
# 
# neighborsLesotho_Maseru <- neighborsLookupLesotho %>%
#   filter(MaseruNeighbor == 1 & (!AREA_NAME %in% DREAMS_Districts_Lesotho))
# 
# neighborsLesotho_MHoek <- neighborsLookupLesotho %>%
#   filter(MHoekNeighbor == 1 & (!AREA_NAME %in% DREAMS_Districts_Lesotho))
# 
# Lesotho_BEREA <- unique(neighborsLesotho_Berea$AREA_NAME)
# Lesotho_BEREA_Names <- rep("BEREA", length(Lesotho_BEREA))
# 
# Lesotho_MAFETENG <- unique(neighborsLesotho_Mafeteng$AREA_NAME)
# Lesotho_MAFETENG_Names <- rep("MAFETENG", length(Lesotho_MAFETENG))
# 
# Lesotho_MASERU <- unique(neighborsLesotho_Maseru$AREA_NAME)
# Lesotho_MASERU_Names <- rep("MASERU", length(Lesotho_MASERU))
# 
# Lesotho_MHOEK <- unique(neighborsLesotho_MHoek$AREA_NAME)
# Lesotho_MHOEK_Names <- rep("MOHALE'S HOEK", length(Lesotho_MHOEK))
# 
# childNamesLesotho <- Lesotho_BEREA %>% append(Lesotho_MAFETENG) %>%
#   append(Lesotho_MASERU) %>%
#   append(Lesotho_MHOEK)
# 
# parentNamesLesotho <- Lesotho_BEREA_Names %>% append(Lesotho_MAFETENG_Names) %>%
#   append(Lesotho_MASERU_Names) %>%
#   append(Lesotho_MHOEK_Names)
# 
# neighborsLookupLesotho_p <- data.frame(parent = parentNamesLesotho,
#            child = childNamesLesotho)
# 
# saveRDS(neighborsLookupLesotho, file = "data/neighborsLookup.RDS")

## Functionalized version ----
### Function for assigning neighbors ----
assignNeighbors <- function(lookup_df, neighbor, dreams_input) {

  # filter regions
  res <- lookup_df %>%
    filter(get(neighbor) == 1 & (!AREA_NAME %in% dreams_input)) %>%
    select(child = AREA_NAME) %>%
    unique() %>%
    mutate(parent = neighbor) %>%
    select(parent, child)
  #print(res)

  #pull unique region names

  return(
    res
  )
}


### lets create a vector to loop through and build ----

bot_neighbors <- c("CentralNeighbor",
                   "KgatlengNeighbor",
                   "KwenengNeighbor",
                   "NorthEastNeighbor",
                   "SouthEastNeighbor",
                   "SouthernNeighbor")
ken_neighbors <- c("HomaNeighbor",
                   "KiambuNeighbor",
                   "KisumuNeighbor",
                   "MigoriNeighbor",
                   "MombasaNeighbor",
                   "NairobiNeighbor", 
                   "SiayaNeighbor")
les_neighbors <- c("BereaNeighbor",
                   "MafetengNeighbor",
                   "MaseruNeighbor",
                   "MHoekNeighbor")
mal_neighbors <- c("BlantyreNeighbor",
                   "MachingaNeighbor",
                   "ZombaNeighbor")
saf_neighbors <- c("ALFRED_NZONeighbor",
                   "BOJANALANeighbor",
                   "BUFFALO_CITYNeighbor",
                   "CAPRICORNNeighbor",
                   "CITY_OF_CAPE_TOWNNeighbor",
                   "CITY_OF_JOHANNESBURGNeighbor",
                   "CITY_OF_TSHWANENeighbor",
                   "DOCTOR_KENNETH_KAUNDANeighbor",
                   "EHLANZENINeighbor",
                   "EKURHULENINeighbor",
                   "ETHEKWININeighbor",
                   "GERT_SIBANDENeighbor",
                   "LEJWELEPUTSWANeighbor",
                   "MOPANINeighbor",
                   "NGAKA_MODIRI_MOLEMANeighbor",
                   "NKANGALANeighbor",
                   "OR_TAMBONeighbor",
                   "SEDIBENGNeighbor",
                   "THABO_MOFUTSANYANENeighbor",
                   "UGUNeighbor",
                   "UMGUNGUNDLOVUNeighbor",
                   "UTHUKELANeighbor",
                   "UTHUNGULUNeighbor",
                   "ZULULANDNeighbor")
tan_neighbors <- c("MwanzaNeighbor",
                   "KageraNeighbor")
zim_neighbors <- c("BulawayoNeighbor",
                   "ManicalandNeighbor",
                   "MCentralNeighbor",
                   "MNorthNeighbor", 
                   "MSouthNeighbor", 
                   "MidlandsNeighbor")

### loop through for each country ----
#### Botswana ----
neighborsLookupBotswana_p_functional <- lapply(bot_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupBotswana,
    neighbor = x,
    dreams_input = DREAMS_Districts_Botswana
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "CentralNeighbor") ~ as.character("CENTRAL"),
      (parent == "KgatlengNeighbor") ~ as.character("KGATLENG"),
      (parent == "KwenengNeighbor") ~ as.character("KWENENG"),
      (parent == "NorthEastNeighbor") ~ as.character("NORTH EAST"),
      (parent == "SouthEastNeighbor") ~ as.character("SOUTH EAST"),
      (parent == "SouthernNeighbor") ~ as.character("SOUTHERN")
    )
  )

neighborsLookupBotswana_p_functional_counts <- neighborsLookupBotswana_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupBotswana_forExport <- left_join(neighborsLookupBotswana_p_functional,
          neighborsLookupBotswana_p_functional_counts)

neighborsLookupBotswana_forExport$country <- "Botswana"

#### Kenya ----
neighborsLookupKenya_p_functional <- lapply(ken_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupKenya,
    neighbor = x,
    dreams_input = DREAMS_Districts_Kenya
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "HomaNeighbor") ~ as.character("HOMA BAY"),
      (parent == "KiambuNeighbor") ~ as.character("KIAMBU"),
      (parent == "KisumuNeighbor") ~ as.character("KISUMU"),
      (parent == "MigoriNeighbor") ~ as.character("MIGORI"),
      (parent == "MombasaNeighbor") ~ as.character("MOMBASA"),
      (parent == "NairobiNeighbor") ~ as.character("NAIROBI CITY"),
      (parent == "SiayaNeighbor") ~ as.character("SIAYA")
    )
  )

neighborsLookupKenya_p_functional_counts <- neighborsLookupKenya_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupKenya_forExport <- left_join(neighborsLookupKenya_p_functional,
                                               neighborsLookupKenya_p_functional_counts)

neighborsLookupKenya_forExport$country <- "Kenya"

#### Lesotho ----
neighborsLookupLesotho_p_functional <- lapply(les_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupLesotho,
    neighbor = x,
    dreams_input = DREAMS_Districts_Lesotho
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "BereaNeighbor") ~ as.character("BEREA"),
      (parent == "MafetengNeighbor") ~ as.character("MAFETENG"),
      (parent == "MaseruNeighbor") ~ as.character("MASERU"),
      (parent == "MHoekNeighbor") ~ as.character("MOHALE'S HOEK"))
  )

neighborsLookupLesotho_p_functional_counts <- neighborsLookupLesotho_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupLesotho_forExport <- left_join(neighborsLookupLesotho_p_functional,
                                    neighborsLookupLesotho_p_functional_counts)

neighborsLookupLesotho_forExport$country <- "Lesotho"

#### Malawi ----
neighborsLookupMalawi_p_functional <- lapply(mal_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupMalawi,
    neighbor = x,
    dreams_input = DREAMS_Districts_Malawi
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "BlantyreNeighbor") ~ as.character("BLANTYRE"),
      (parent == "MachingaNeighbor") ~ as.character("MACHINGA"),
      (parent == "ZombaNeighbor") ~ as.character("ZOMBA")
    )
  )

neighborsLookupMalawi_p_functional_counts <- neighborsLookupMalawi_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupMalawi_forExport <- left_join(neighborsLookupMalawi_p_functional,
                                               neighborsLookupMalawi_p_functional_counts)

neighborsLookupMalawi_forExport$country <- "Malawi"


#### South Africa ----
neighborsLookupSouthAfrica_p_functional <- lapply(saf_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupSouthAfrica,
    neighbor = x,
    dreams_input = DREAMS_Districts_SouthAfrica
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "ALFRED_NZONeighbor") ~ as.character("ALFRED NZO"),
      (parent == "BOJANALANeighbor") ~ as.character("BOJANALA"),
      (parent == "BUFFALO_CITYNeighbor") ~ as.character("BUFFALO CITY"),
      (parent == "CAPRICORNNeighbor") ~ as.character("CAPRICORN"),
      (parent == "CITY_OF_CAPE_TOWNNeighbor") ~ as.character("CITY OF CAPE TOWN"),
      (parent == "CITY_OF_JOHANNESBURGNeighbor") ~ as.character("CITY OF JOHANNESBURG"),
      (parent == "CITY_OF_TSHWANENeighbor") ~ as.character("CITY OF TSHWANE"),
      (parent == "DOCTOR_KENNETH_KAUNDANeighbor") ~ as.character("DOCTOR KENNETH KAUNDA"),
      (parent == "EHLANZENINeighbor") ~ as.character("EHLANZENI"),
      (parent == "EKURHULENINeighbor") ~ as.character("EKURHULENI"),
      (parent == "ETHEKWININeighbor") ~ as.character("ETHEKWINI"),
      (parent == "GERT_SIBANDENeighbor") ~ as.character("GERT SIBANDE"),
      (parent == "LEJWELEPUTSWANeighbor") ~ as.character("LEJWELEPUTSWA"),
      (parent == "MOPANINeighbor") ~ as.character("MOPANI"),
      (parent == "NGAKA_MODIRI_MOLEMANeighbor") ~ as.character("NGAKA MODIRI MOLEMA"),
      (parent == "NKANGALANeighbor") ~ as.character("NKANGALA"),
      (parent == "OR_TAMBONeighbor") ~ as.character("O.R. TAMBO"),
      (parent == "SEDIBENGNeighbor") ~ as.character("SEDIBENG"),
      (parent == "THABO_MOFUTSANYANENeighbor") ~ as.character("THABO MOFUTSANYANE"),
      (parent == "UGUNeighbor") ~ as.character("UGU"),
      (parent == "UMGUNGUNDLOVUNeighbor") ~ as.character("UMGUNGUNDLOVU"),
      (parent == "UTHUKELANeighbor") ~ as.character("UTHUKELA"),
      (parent == "UTHUNGULUNeighbor") ~ as.character("UTHUNGULU"),
      (parent == "ZULULANDNeighbor") ~ as.character("ZULULAND")
    )
  )

neighborsLookupSouthAfrica_p_functional_counts <- neighborsLookupSouthAfrica_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupSouthAfrica_forExport <- left_join(neighborsLookupSouthAfrica_p_functional,
                                               neighborsLookupSouthAfrica_p_functional_counts)

neighborsLookupSouthAfrica_forExport$country <- "South Africa"


#### Tanzania ----
neighborsLookupTanzania_p_functional <- lapply(tan_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupTanzania,
    neighbor = x,
    dreams_input = DREAMS_Districts_Tanzania
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "MwanzaNeighbor") ~ as.character("MWANZA"),
      (parent == "KageraNeighbor") ~ as.character("KAGERA")
    )
  )

neighborsLookupTanzania_p_functional_counts <- neighborsLookupTanzania_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupTanzania_forExport <- left_join(neighborsLookupTanzania_p_functional,
                                               neighborsLookupTanzania_p_functional_counts)

neighborsLookupTanzania_forExport$country <- "Tanzania"


#### Zimbabwe ----
neighborsLookupZimbabwe_p_functional <- lapply(zim_neighbors, function(x) {
  neighbor_df <- assignNeighbors(
    lookup_df = neighborsLookupZimbabwe,
    neighbor = x,
    dreams_input = DREAMS_Districts_Zimbabwe
  )
}) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(
    parent = case_when(
      (parent == "BulawayoNeighbor") ~ as.character("BULAWAYO"),
      (parent == "ManicalandNeighbor") ~ as.character("MANICALAND"),
      (parent == "MCentralNeighbor") ~ as.character("MASHONALAND CENTRAL"),
      (parent == "MNorthNeighbor") ~ as.character("MATABELELAND NORTH"),
      (parent == "MSouthNeighbor") ~ as.character("MATABELELAND SOUTH"),
      (parent == "MidlandsNeighbor") ~ as.character("MIDLANDS")
    )
  )

neighborsLookupZimbabwe_p_functional_counts <- neighborsLookupZimbabwe_p_functional %>%
  group_by(parent) %>%
  tally() %>%
  rename(neighbors = n)

neighborsLookupZimbabwe_forExport <- left_join(neighborsLookupZimbabwe_p_functional,
                                            neighborsLookupZimbabwe_p_functional_counts)

neighborsLookupZimbabwe_forExport$country <- "Zimbabwe"

#### Create grouped df

neighborsLookupAll <- bind_rows(neighborsLookupBotswana_forExport,
                               neighborsLookupKenya_forExport,
                               neighborsLookupLesotho_forExport,
                               neighborsLookupMalawi_forExport,
                               neighborsLookupSouthAfrica_forExport,
                               neighborsLookupTanzania_forExport,
                               neighborsLookupZimbabwe_forExport
                               )

### Save results for app ----
#### Original (outdated as now all being exported together) ----
#saveRDS(neighborsLookupLesotho, file = "data/neighborsLookup.RDS")

saveRDS(neighborsLookupAll, file = "data/neighborsLookupAll.RDS")

## Other possible approaches, nixed for now ----
# # 
# # # compare (the parent name will need some work)
# # print(neighborsLookupLesotho_p_functional)
# # print(neighborsLookupLesotho_p)
# # 
# # # how do we extend to a country? ----
# # 
# # # we can either do a manual run of the above function on every country
# # # it will be manual but still look a lot cleaner
# # # or we can attempt a loop as shown below
# # 
# # # INPUTS -----
# # 
# # # we can create a multi-country dataframe
# # # here we create a fake congo data frame
# neighborsLookupCongo <- neighborsLookupLesotho %>%
#   select(-c(CNTRY_NAME)) %>%
#   mutate("CNTRY_NAME" = "CONGO") %>%
#   select(AREA_NAME, CNTRY_NAME, BereaNeighbor, MafetengNeighbor, MaseruNeighbor, MHoekNeighbor)
# #
# 
# neighborsLookupCongo_p_functional <- lapply(neighbors, function(x) {
#   neighbor_df <- assignNeighbors(
#     lookup_df = neighborsLookupCongo,
#     neighbor = x,
#     dreams_input = DREAMS_Districts_Lesotho
#   )
# }) %>% dplyr::bind_rows()
# 
# 
# # # imagine a list 16 countries in it each with their own regions
# # # another option is a list of data frames
# # master_geo <- 
# #   list(
# #     LESOTHO = neighborsLookupLesotho,
# #     CONGO = neighborsLookupCongo
# #   )
# # 
# # # imagine a list with all 16 neighbor regions
# # dreams_dristrict_list <- 
# #   list(
# #     LESOTHO = c("BEREA", 
# #                 "MAFETENG",
# #                 "MASERU",
# #                 "MOHALE'S HOEK"),
# #     CONGO = c("BEREA", 
# #               "MAFETENG",
# #               "MASERU",
# #               "MOHALE'S HOEK")
# #   )
# # 
# # neighbors_list <- 
# #   list(
# #     LESOTHO = neighbors,
# #     CONGO = neighbors # each country would get their own list
# #   )
# # 
# # # PROCESS----
# # 
# # master_list <- list()
# # for (country in unique(names(master_geo))) {
# #   
# #   print(paste0("processing for country ", country))
# #   
# #   # in this case we just have one vector for neighbors but we would probably have
# #   # a dataframe or something of a lookup for neighbors as well
# #   neighbors <- neighbors_list[[country]]
# #   df_lookup <- master_geo[[country]]
# #   dreams_districts <- dreams_dristrict_list[[country]]
# #   
# #   
# #   neighbors_result <- lapply(neighbors, function(x) {
# #     neighbor_df <- assignNeighbors(
# #       lookup_df = df_lookup, 
# #       neighbor = x, 
# #       dreams_input = DREAMS_Districts_Lesotho
# #     )
# #   }) %>% dplyr::bind_rows()
# #   
# #   print(neighbors_result)
# #   
# #   master_list[[country]] <- neighbors_result
# #   
# # }
# # 
# # # the final result, you can bind or work with the list
# # dplyr::bind_rows(master_list)


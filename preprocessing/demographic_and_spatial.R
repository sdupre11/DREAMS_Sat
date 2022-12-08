library(readxl)
library(tidyverse)
library(sf)
library(spdep)
library(leaflet)

#CONVENIENCE FUNCTION(S)
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

#Age table setup
Ages <- c(10:24) 
`2018` <- rep(20,15)
`2019` <- rep(20,15)
`2020` <- rep(20,15)
`2021` <- rep(20,15)
`2022` <- rep(20,15)

structure_table <- data.frame(Ages,
                              `2018`,
                              `2019`,
                              `2020`,
                              `2021`,
                              `2022`)

structure_table_modal <- data.frame(Ages,
                                    `2018`,
                                    `2019`,
                                    `2020`,
                                    `2021`,
                                    `2022`)

saveRDS(structure_table, file = "structure_table.RDS")
saveRDS(structure_table_modal, file = "structure_table_modal.RDS")

####COTE D'IVOIRE [Not currently in subnat estimates] [Will need to adjust for fact that ADM2s are the PEPFAR units]
####ESWATINI [Not currently in subnat estimates]
####HAITI
####MALAWI
####MOZAMBIQUE
####NAMIBIA
####RWANDA
####SOUTH AFRICA
####SOUTH SUDAN [This is from BHA, check format and content]
####TANZANIA
####UGANDA [Will need to adjust for fact that ADM2s are the PEPFAR units]
####ZAMBIA



###############################
####BOTSWANA
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Bot.sf <- st_read('Botswana_adm1_uscb_2019.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Bot.sf <- st_read('Botswana_adm2_uscb_2019.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Bot.sf, file = "BotswanaADM1.RDS")
saveRDS(ADM2.1.Bot.sf, file = "BotswanaADM2.RDS")

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
Demographic.1 <- readxl::read_xlsx("Botswana_USCB.xlsx")

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
write_excel_csv(merged.6, file = "BotswanaOutput_Denominators.csv")
#For import to app
saveRDS(merged.6, file = "BotswanaOutput_Denominators.RDS")

###############################
####KENYA
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Ken.sf <- st_read('Kenya_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Ken.sf <- st_read('Kenya_adm2_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Ken.sf, file = "KenyaADM1.RDS")
saveRDS(ADM2.1.Ken.sf, file = "KenyaADM2.RDS")

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
Demographic.1 <- readxl::read_xlsx("Kenya_USCB.xlsx")

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
write_excel_csv(merged.6, file = "KenyaOutput_Denominators.csv")
#For import to app
saveRDS(merged.6,file = "KenyaOutput_Denominators.RDS")

###############################
####LESOTHO
###DENOMINATOR: Small country, ADM1 only
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Les.sf <- st_read('Lesotho_adm1_uscb_2020.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Les.sf, file = "LesothoADM1.RDS")

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
Demographic.1 <- readxl::read_xlsx("Lesotho_USCB.xlsx")

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

##Export Results
#For any manual checking
write_excel_csv(merged.6, file = "LesothoOutput_Denominators.csv")
#For import to app
saveRDS(merged.6,file = "LesothoOutput_Denominators.RDS")

###############################
####ZIMBABWE
###DENOMINATOR: Larger country, ADM1 and 2
##Start with District cohort pop by year
##Add surrounding ADM2s to cohort pop by year
ADM1.1.Zim.sf <- st_read('Zimbabwe_adm1_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

ADM2.1.Zim.sf <- st_read('Zimbabwe_adm2_uscb_2022.shp') %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(ADM1.1.Zim.sf, file = "ZimbabweADM1.RDS")
saveRDS(ADM2.1.Zim.sf, file = "ZimbabweADM2.RDS")

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
Demographic.1 <- readxl::read_xlsx("Zimbabwe_USCB.xlsx")

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
write_excel_csv(merged.6, file = "ZimbabweOutput_Denominators.csv")
#For import to app
saveRDS(merged.6,file = "ZimbabweOutput_Denominators.RDS")




#COMBINE AND PREPROCESS
Bot_Data <- readRDS('data/BotswanaOutput_Denominators.RDS') %>%
  mutate(country = "Botswana")
Ken_Data <- readRDS('data/KenyaOutput_Denominators.RDS') %>%
  mutate(country = "Kenya")
Les_Data <- readRDS('data/LesothoOutput_Denominators.RDS') %>%
  mutate(country = "Lesotho")
Zim_Data <- readRDS('data/ZimbabweOutput_Denominators.RDS') %>%
  mutate(country = "Zimbabwe")

countryData <- rbind(Bot_Data,
                     Ken_Data,
                     Les_Data,
                     Zim_Data) %>%
  pivot_longer(
    cols = `2019`:`2022`,
    names_to = c("fiscal_year"), 
    names_prefix = "F",
    values_to = "population"
  )

saveRDS(countryData, file = "data/countryData.RDS")

saveRDS(countryData, file = "data/countryData.RDS")

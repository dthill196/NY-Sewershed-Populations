#########################################################
#########################################################
### Assign population values to sewersheds: Functions ###
#########################################################
#########################################################


###
# steps
###

#packages
library(tidyverse)
library(sf)
library(tidycensus)
#library(leaflet)
#library(mapview)
#library(lubridate)
library(dplyr)
library(readxl)
library(stringi)
library(readr)
library(tidyr)
library(tigris)
library(tmap)
library(data.table)
library(stringr)


#api key for tidy census
# you will need an API key for using tidycensus
census_api_key(Sys.getenv('CENSUS_API_KEY'))

#ACS18 <- load_variables(2010, "decennial", cache = TRUE) # find variables table numbers

# block function
data_grab_2010_Block_function <- function(x){
  dataGrab_2010 <- get_decennial(geography = "block",
                                 state = "NY",
                                 county = county_sewer,
                                 year = 2010,
                                 
                                 variables = c(population = "H010001"),
                                 moe_level = 95)
  
  #no margin of error for decennial census
  wwCensus_2010 <- dataGrab_2010 %>% 
    spread(variable, value) %>% 
    distinct()
  
  wwCensus_2010$year <- rep(2010, nrow(wwCensus_2010))
  
  
  
  wwCensus_2010 <- wwCensus_2010 %>% 
    dplyr::select(GEOID, year, population)
  censusGeo_block$GEOID <- censusGeo_block$GEOID10
  wwCensusGeo_block <- merge(censusGeo_block, wwCensus_2010, by = "GEOID")
  
}

#function calculate area for each block, intersect, and calculate new area then dissolve
census_dissolve_function <- function(wwCensusGeo_block, sewersheds){
  #area for each block
  wwCensusGeo_block$area_BG <- st_area(wwCensusGeo_block)
  
  #calculate area proportion in each sewershed
  
  #need to add slight buffer for intersection to work
  wwCensusGeo_block <- st_buffer(wwCensusGeo_block, dist=0)
  sewersheds <- st_buffer(sewersheds, dist = 0)
  Block_clip <- st_intersection(wwCensusGeo_block, sewersheds)
  #area for clipped blocks
  Block_clip$area_clip <- st_area(Block_clip)
  
  #area proportion in each
  Block_clip$area_prop <- Block_clip$area_clip / Block_clip$area_BG
  Block_clip$population_2010_est <- Block_clip$area_prop * Block_clip$population
  Block_dt <- as.data.table(Block_clip)
  Block_dt_final <-
    Block_dt %>%
    group_by(Sewershed) %>%
    summarise(population_block_2010 = sum(population_2010_est))
  
}

# 2010 and 2018 block group data grab and merge to shapefile function
datagrab_BG_function <- function(x){
  
  dataGrab_2010 <- get_decennial(geography = "block group",
                                 state = "NY",
                                 county = county_sewer,
                                 year = 2010,
                                 
                                 variables = c(population_BG_2010 = "H010001",
                                               group_quartersPop = "P029026",
                                               group_quartersNursing = "P042005"
                                 ),
                                 moe_level = 95)
  
  #no margin of error for decennial census
  wwCensus_2010 <- dataGrab_2010 %>% 
    spread(variable, value) %>% 
    distinct()
  
  
  dataGrab_2018 <- get_acs(geography = "block group",
                           state = "NY",
                           county = county_sewer,
                           variables = c(population_BG_2018 = "B01001_001",
                                         housing_units = "B25001_001",
                                         vacant_units = "B25002_003",
                                         owner_occupied = "B25002_002",
                                         Male_age_65_66 = "B01001_020",
                                         Male_age_67_69 = "B01001_021",
                                         Male_age_70_74 = "B01001_022",
                                         Male_age_75_79 = "B01001_023",
                                         Male_age_80_84 = "B01001_024",
                                         Male_age_85_plus = "B01001_025",
                                         F_age_65_66 = "B01001_044",
                                         F_age_67_69 = "B01001_045",
                                         F_age_70_74 = "B01001_046",
                                         F_age_75_79 = "B01001_047",
                                         F_age_80_84 = "B01001_048",
                                         F_age_85_plus = "B01001_049",
                                         black_alone = "B02001_003",
                                         Amer_indian = "B02001_004",
                                         Asian = "B02001_005",
                                         Pacific_islander = "B02001_006",
                                         Other_race = "B02001_007",
                                         Two_or_more_races = "B02001_008",
                                         hispanic_pop = "B03003_003",
                                         median_house_value = "B25077_001",
                                         Total_Education = "B15003_001",
                                         overHS1 = "B15003_017",
                                         overHS2 = "B15003_018",
                                         overHS3 = "B15003_019",
                                         overHS4 = "B15003_020",
                                         overHS5 = "B15003_021",
                                         overHS6 = "B15003_022",
                                         overHS7 = "B15003_023",
                                         overHS8 = "B15003_024",
                                         overHS9 = "B15003_025"
                           ),
                           year = 2018)
  
  wwCensus_2018 <- dataGrab_2018 %>% 
    dplyr::select(-moe) %>% 
    spread(variable, estimate) %>% 
    distinct()
  
  
  wwCensus_2018 <- wwCensus_2018 %>% 
    mutate(
      Over64_Per = (Male_age_65_66 + Male_age_67_69 +Male_age_70_74 +
                      Male_age_75_79 + Male_age_80_84 + Male_age_85_plus +
                      F_age_65_66 + F_age_67_69 + F_age_70_74 +
                      F_age_75_79 + F_age_80_84 + F_age_85_plus),
      Nonwhite_Pop = (black_alone + Amer_indian + Asian + Pacific_islander + Other_race + Two_or_more_races),
      Under_HS_tot = (overHS1 + overHS2 + overHS3 + overHS4 + overHS5 + overHS6 +
                        overHS7 + overHS8 +overHS9))
  
  
  wwCensus_2018 <- wwCensus_2018 %>% 
    dplyr::select(GEOID, population_BG_2018, owner_occupied, Over64_Per, housing_units, vacant_units, black_alone, hispanic_pop, Nonwhite_Pop, Total_Education, Under_HS_tot)
  wwCensus <- left_join(wwCensus_2010, wwCensus_2018, by = "GEOID")
  #ratio which can be multiplied by 2010 value to determine the increase or decrease in population
  
  #
  #merge to shapefile loaded earlier
  wwCensusGeo_change <- merge(censusGeo_BG, wwCensus, by = "GEOID")
  
}

census_BG_dissolve_function <- function(wwCensusBG_2010_2018, sewersheds){
  #area for each block group
  wwCensusBG_2010_2018$area_BG <- st_area(wwCensusBG_2010_2018)
  
  #calculate area proportion in each sewershed so can assign to the population change values to get estimate for population in each based on block group rather than block data
  wwCensusBG_2010_2018 <- st_buffer(wwCensusBG_2010_2018, dist=0)
  sewersheds <- st_buffer(sewersheds, dist=0)
  BG_clip <- st_intersection(wwCensusBG_2010_2018, sewersheds)
  #area for clipped block groups
  BG_clip$area_clip <- st_area(BG_clip)
  
  #area proportion in each
  BG_clip$area_prop <- BG_clip$area_clip / BG_clip$area_BG
  
  #now multiply by the population values for 2010 and 2018
  # block population data
  #now multiply by the population values for 2010 
  BG_clip_dt <- as.data.table(BG_clip)
  BG_clip_dt <- BG_clip_dt[, 12:24 := lapply(.SD, '*', area_prop), .SDcols = 12:24][]
  
  #now dissolve to sewershed and compare to block values to get a margin of error
  #but first change to sf object
  BG.final <-
    BG_clip_dt %>%
    group_by(Sewershed) %>%
    summarise(bg_pop_2010 = sum(population_BG_2010),
              bg_pop_2018 = sum(population_BG_2018),
              owner_occupied_2018 = sum(owner_occupied),
              Over64_2018 = sum(Over64_Per),
              housing_units_2018 = sum(housing_units),
              vacant_units_2018 = sum(vacant_units),
              group_quartersNursing_2010 = sum(group_quartersNursing),
              group_quartersPop_2010 = sum(group_quartersPop),
              black_alone_2018 = sum(black_alone),
              hispanic_pop_2018 = sum(hispanic_pop),
              Nonwhite_pop_2018 = sum(Nonwhite_Pop),
              Total_Education_2018 = sum(Total_Education),
              Under_HS_tot_2018 = sum(Under_HS_tot)
    )
  
}

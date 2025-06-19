################################################################
### Reads in StatsCan Census data and writes CSV + GeoJSON to /data
################################################################

library(cancensus)
library(dplyr)
library(sf)
library(tidyverse)
library(geojsonio)
library(here)

# Set persistent API key and cache path (only do install = TRUE once)
set_cancensus_api_key("CensusMapper_4157198e649c5a47d5759a6f6be9214a", install = TRUE)
set_cancensus_cache_path(here("cache"), install = TRUE)

# Define vector of variables and their labels
census_vars <- c(
  pop_Tot = "v_CA16_401",
  dwell_Tot = "v_CA16_404",
  land_area = "v_CA16_407",
  dwell_type_Smpl = "v_CA16_408",
  dwell_type_SingDet = "v_CA16_409",
  hh_inc_MedIncHHBefTax = "v_CA16_2397",
  hh_inc_MedIncHHAftTax = "v_CA16_2398",
  dwell_char_AvgRoomsSmpl = "v_CA16_4855",
  dwell_fin_NumbHHSmpl = "v_CA16_4890",
  dwell_fin_MedValueOfDwellingSmpl = "v_CA16_4895",
  dwell_fin_AvgValueOfDwellingSmpl = "v_CA16_4896",
  hh_educ_TotHighestDegreeSmpl = "v_CA16_5051",
  hh_educ_Bach = "v_CA16_5081",
  hh_educ_AboveBach = "v_CA16_5084",
  travel_dist_TotDestSmpl = "v_CA16_5777",
  traveL_dist_WithinCSDSmpl = "v_CA16_5780",
  traveL_dist_DiffCSDSameCDSmpl = "v_CA16_5783",
  traveL_dist_DiffCSDDiffCDSmpl = "v_CA16_5786",
  traveL_dist_DiffProvSmpl = "v_CA16_5789",
  traveL_mode_TotModeSmpl = "v_CA16_5792",
  traveL_mode_CarEtcDriverSmpl = "v_CA16_5795",
  traveL_mode_CarEtcPassSmpl = "v_CA16_5798",
  traveL_mode_PublicTransSmpl = "v_CA16_5801",
  traveL_time_TotCommuteTimeSmpl = "v_CA16_5813",
  traveL_time_Less15Smpl = "v_CA16_5816",
  traveL_time_15to29Smpl = "v_CA16_5819",
  traveL_time_30to44Smpl = "v_CA16_5822",
  traveL_time_45to59Smpl = "v_CA16_5825",
  travel_time_60PlusSmpl = "v_CA16_5828"
)

# Retrieve census data for Ontario (PR = 35) at CT level
census_df <- get_census(
  dataset = "CA16",
  regions = list(PR = "35"),
  vectors = census_vars,
  level = "CT",
  quiet = TRUE,
  geo_format = "sf",
  labels = "short"
)

census_df$CTUID <- as.character(census_df$GeoUID)

# Write outputs to /data directory using 
write_csv(census_df, here("data", "census_df.csv"))
geojson_write(census_df, file = here("data", "census_df.geojson"))


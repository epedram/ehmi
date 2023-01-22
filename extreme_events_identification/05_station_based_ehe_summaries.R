## title: "Spatio-temporal Delineation of Extreme Heat Events (EHE)"
## subtitle: "Estimation of the Impacted Population"

## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Libraries ----
library(here)
library(tidyverse)
library(sf)
sf_use_s2(FALSE)

spatial_projection <-  4269 #NAD83
default_crs = sf::st_crs(spatial_projection)

dpi <- 300
censusgeo <-  "census tracts"

## set the temporal parameters ----

start_date <-  as.Date("2017-01-01", format="%Y-%m-%d")
end_date <-  as.Date("2021-12-31", format="%Y-%m-%d")
start_year <-  format(start_date, format="%Y")
end_year <-  format(end_date, format="%Y")

year <- paste(start_year, end_year, sep = "_")

## I/O ----
input_dir <- "~/"

acs_dir <-  "~/acs/"

dir.create(file.path(input_dir, paste0("station_based_ehe_", year)))
output_path <- file.path(input_dir, paste0("station_based_ehe_", year))

normalize_by_range <- function(x)
{
  norm_x <- ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))# * 100
  return(norm_x)
}

##  load weather data ----
noaa_compiled_sf_selected <- readRDS(here(input_dir,
                                  "noaa_isd_2021_ca_state_100km.rds"))[[1]] %>%
  st_transform(crs = spatial_projection)

EHE_compiled <- readRDS(here(input_dir,
                                  "ca_EHE_compiled_2017_2021_95.rds")) %>%
  mutate(EHMI_normalized_by_range_global = normalize_by_range(EHMI))

##  load census data ----
CA_counties_sf <- readRDS(here(input_dir,
                                  "CA_counties_sf_ACS2019.rds"))[[1]] %>%
  st_transform(crs = spatial_projection)

## load the compiled ACS5 table for years 2015 and 2020 (output of the ehec_candidates.R)
acs_files <- list.files(acs_dir)
acs_table <- map(acs_files, ~read_rds(file=file.path(acs_dir,.))) %>% bind_rows() # sf file

urbanprobs <- c(0, .2, 1)
urban_key <- c("1" = "Rural", "2" = "Urban")

## select and label demographic variables
census_db_sf_urban_rural <- acs_table %>%
  mutate(d_total_population_abs = estimate_B02001_001,
         d_65_and_above_abs = d_total_male_over_age_65 + d_total_female_over_age_65,
         d_25_and_above_abs = d_total_male_over_age_25 + d_total_female_over_age_25,

         d_white_abs = estimate_B02001_002,
         d_black_abs = estimate_B02001_003,
         d_asian_abs = estimate_B02001_005,
         d_native_abs = estimate_B02001_004,
         d_hawaii_pi_abs = estimate_B02001_006,
         d_other_race_abs = estimate_B02001_007,
         d_two_more_race_abs = estimate_B02001_008,
         d_two_including_other_race_abs = estimate_B02001_009) %>%
  dplyr::filter(year == 2020) %>%
  dplyr::select(contains("_abs") | ends_with("population_size")
                | starts_with("GEOID")
                  ) %>%
  mutate(computed_area = round(as.numeric(st_area(.)), 1)) %>%
  mutate(area_hectare = round((computed_area / 10000), 1)) %>%
  mutate(pop_density_per_hect = round((d_total_population_abs / area_hectare), 1)) %>%

  mutate(urban_rural_code = as.factor(.bincode(pop_density_per_hect,
                                                 breaks=quantile(pop_density_per_hect,
                                                                 probs=urbanprobs, na.rm=TRUE),
                                                 include.lowest=TRUE))) %>%
  mutate(urban_rural_class = recode_factor(urban_rural_code,
                                       !!!urban_key))# %>%

census_db_sf <- census_db_sf_urban_rural %>%
  mutate(d_urban_pop =
           case_when(urban_rural_class == "Urban" ~ d_total_population_abs,
         #            TRUE ~ 0L)
         )) %>%
  mutate(d_rural_pop =
           case_when(urban_rural_class == "Rural" ~ d_total_population_abs,
         #            TRUE ~ 0L)
         )) %>%
  mutate(d_urban_area_hectare =
           case_when(urban_rural_class == "Urban" ~ area_hectare,
         #            TRUE ~ 0L)
         )) %>%
  mutate(d_rural_area_hectare =
           case_when(urban_rural_class == "Rural" ~ area_hectare,
         #            TRUE ~ 0L)
         )) %>%
  st_transform(crs = spatial_projection)

 census_db_sf %>%
   st_write(file.path(output_path,
     "census_db_sf.gpkg"),
            delete_dsn = TRUE)

# assign the nearest station to each census unit ----
CA_censusgeo_joined_noaa <- st_join(census_db_sf,
            noaa_compiled_sf_selected["station_id"],
            join = st_nearest_feature)

EHE_selected_period_geo <- st_as_sf(EHE_compiled,
                                   coords = c("lon_x", "lat_y"),
                                   remove = FALSE,
                                   crs = spatial_projection) %>%
                      filter(DATE >= start_date & DATE <= end_date) %>%
                      as_tibble() %>% st_as_sf() %>%
  mutate(EHMI_normalized_by_range = normalize_by_range(EHMI))

selected_period_geo_events_only <- EHE_selected_period_geo %>%
                                    filter(Event_duration > 0)

EHE_DATES <- selected_period_geo_events_only %>% st_drop_geometry() %>%
  dplyr::select(DATE) %>%
  unique(.) %>%
  arrange(DATE)

Event_Dates <- as.list(EHE_DATES[[1]])

CA_censusgeo_compiled_EHE <- merge(CA_censusgeo_joined_noaa,
                                    selected_period_geo_events_only %>% st_drop_geometry(),
                                      by.x=c("station_id"),
                                      by.y=c("station_id"),
                                      all.x = TRUE) %>%
                       st_as_sf() %>% as_tibble() %>% st_as_sf() %>%

  mutate(computed_area = round(as.numeric(st_area(.)), 1)) %>%
  mutate(area_hectare = round((computed_area / 10000), 1)) %>%

   mutate_at(vars(EHE_duration,
                  Event_duration,
                  EHF, EHMI
   ), ~replace_na(., 0))

# compute station based extreme events summaries ----
selected_variables <- c(
  "EHE_duration",
  "DM_AT",
  "temperature_avg",
  "wind_speed_avg",
  "temperature_dewpoint_avg",
  "relative_humidity_avg",
  "EHMI"
)

EHE_Summary <- selected_period_geo_events_only %>%

  st_drop_geometry() %>%

  summarise(
    events_records = n(),
    unique_impacted_stations = n_distinct(station_id),
    n_distinct_days_n = n_distinct(DATE, na.rm = T),
    n_distinct_events_n = n_distinct(EUID, na.rm = T),

    across(all_of(selected_variables),
           list(
             null = ~sum(is.na(.)),
             inf = ~sum(is.infinite(.)),
             avg = ~round(mean(as.double(.), na.rm = T), 1),
             min = ~round(min(as.double(.), na.rm = T), 1),
             max = ~round(max(as.double(.), na.rm = T), 1),
             median = ~median(as.double(.), na.rm = T),
             sd = ~round(sd(as.double(.), na.rm = T), 1),
             Q1 = ~round(quantile(., probs = .25, na.rm = TRUE), 1),
             Q3 = ~round(quantile(., probs = .75, na.rm = TRUE), 1)
           )
    ),
    .groups = 'drop') %>%
  mutate(YYYY = start_year) %>%
  mutate(
    EHE_duration_SE = round((EHE_duration_sd / sqrt(events_records)), 2),
    temperature_avg_SE = round((temperature_avg_sd / sqrt(events_records)), 2),
    DM_AT_SE = round((DM_AT_sd / sqrt(events_records)), 2)
  )

EHE_Summary

write_csv(EHE_Summary, file=file.path(output_path,
                                       paste0("EHE_Summary_",
                                              year,
                                              ".csv")))

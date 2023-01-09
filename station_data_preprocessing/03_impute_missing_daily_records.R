## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Impute missing daily records

library(here)
library(tidyverse)
library(data.table)
library(sf)

# set up the parameters ----
start_year <-  2016
end_year <-  2021

year <- paste(start_year, end_year, sep = "_")

# set the I/O path -----
input_dir <- "~/"

dir.create(file.path(input_dir, paste0("isd_imputed_daily_records_", year)))
output_path <- file.path(input_dir, paste0("isd_imputed_daily_records_", year))

# Set up computational parameters ----
spatial_projection  = 4269 #NAD83

# Computational script ----
noaa_isd_2021_sf_ca_state_100km_simplified <- readRDS(file.path(input_dir,
                                                              "noaa_isd_2021_sf_ca_state_100km_simplified.rds"))[[1]]

# Load daily and annual summaries from compiled yearly files
processed_daily_data_path <- file.path(input_dir,
                                       "noaa_isd_ca_web_compiled_daily_2016_2017")

processed_annual_data_path <- file.path(input_dir,
                                        "noaa_isd_ca_web_compiled_daily_2016_2017")

daily_processed_data_list <- list.files(path = processed_daily_data_path,
                                       pattern = "*daily*")

annual_processed_data_list <- list.files(path = processed_annual_data_path,
                                        pattern = "*annual*")

all_daily_summary_collector <- list()
all_annual_summary_collector <- list()

# Merge all the yearly tables for the entire study period-----
sapply(1:length(daily_processed_data_list), function(i) {

  print(i)

  # ANNUAL ----
  each_year_annual_df <- readRDS(file.path(processed_annual_data_path,
                                         annual_processed_data_list[[i]]))[[1]] %>%

    mutate(completeness_above_98_percent =
             case_when(
               (temperature_comp_ratio > 0.98 &
                 temperature_dewpoint_comp_ratio > 0.98 &
                 wind_speed_comp_ratio > 0.98)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_95_percent =
             case_when(
               (temperature_comp_ratio >= 0.95 &
                 temperature_dewpoint_comp_ratio >= 0.95 &
                 wind_speed_comp_ratio >= 0.95)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_90_percent =
             case_when(
               (temperature_comp_ratio >= 0.90 &
                 temperature_dewpoint_comp_ratio >= 0.90 &
                 wind_speed_comp_ratio >= 0.90)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_85_percent =
             case_when(
               (temperature_comp_ratio >= 0.85 &
                 temperature_dewpoint_comp_ratio >= 0.85 &
                 wind_speed_comp_ratio >= 0.85)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_80_percent =
             case_when(
               (temperature_comp_ratio >= 0.80 &
                 temperature_dewpoint_comp_ratio >= 0.80 &
                 wind_speed_comp_ratio >= 0.80)
               ~ 1L, TRUE ~ 0)) %>%
    mutate(completeness_above_75_percent =
             case_when(
               (temperature_comp_ratio >= 0.75 &
                 temperature_dewpoint_comp_ratio >= 0.75 &
                 wind_speed_comp_ratio >= 0.75)
               ~ 1L, TRUE ~ 0)) %>%

    mutate(completeness_code = completeness_above_75_percent +
             completeness_above_80_percent + completeness_above_85_percent +
             completeness_above_90_percent + completeness_above_95_percent +
             completeness_above_98_percent,

             completeness_percent = 100-((6-completeness_code)*5))

  # DAILY ----
  each_year_annual_df_qc <- each_year_annual_df %>%
    dplyr::select(ends_with("_id") | ends_with("completeness_code") |
                    contains("YYYY") )

  each_year_daily_df_raw <- readRDS(file.path(processed_daily_data_path,
                                        daily_processed_data_list[[i]]))[[1]] %>%
    # adjust the columns data type
    mutate(station_id = as.character(station_id)) %>%
    mutate(YYYY_MM_DD = as.Date(format(YYYY_MM_DD), "%Y-%m-%d"),
    )
  # assign completeness info to daily tables
  each_year_daily_df <-
    merge(each_year_daily_df_raw,
          each_year_annual_df_qc,
          #
          by.x = c("station_id", "YYYY"),
          by.y = c("station_id", "YYYY"),
          all.x = TRUE)

  all_daily_summary_collector[[i]] <<- each_year_daily_df
  all_annual_summary_collector[[i]] <<- each_year_annual_df

  gc()

})
# Compile summaries ----
## Annual-----
#### Assign "Completeness" flags ----
all_annual_summaries <- dplyr::bind_rows(all_annual_summary_collector) %>%
  dplyr::filter(YYYY >= start_year )

glimpse(all_annual_summaries)
dim(all_annual_summaries) /22

## Daily-----
## Replace inf values with NA
all_daily_summaries_inf2na <- dplyr::bind_rows(all_daily_summary_collector) %>%
  dplyr::filter(YYYY >= start_year ) %>%
  # convert inf to nulls
  mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

selected_variables <- c(
  "temperature_avg",
  "wind_speed_avg",
  "temperature_dewpoint_avg",
  "YYYY_MM_DD"
)

all_daily_inf2na_notimputed <- all_daily_summaries_inf2na %>%
  mutate(completeness_percent = 100-((6-completeness_code)*5)) %>%
  group_by(YYYY, completeness_percent) %>%
  summarise(
            n_records = n(),
            n_distinct_stations = n_distinct(station_id),
            across(all_of(selected_variables),
                       list(
                         null = ~sum(is.na(.)),
                         inf = ~sum(is.infinite(.)),
                         min = ~round(min(., na.rm = T), 1),
                         max = ~round(max(., na.rm = T), 1)
                       )
  ),
  .groups = 'drop')

    source(here("station_data_preprocessing",
                "0301_apply_completeness_criteria.R"), local=T)

## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## The computational code to identify Extreme Heat Events (EHE) at NOAA stations

library(here)
library(tidyverse)
library(data.table)
library(sf)
library(janitor)

# set up the parameters ----
start_year <-  2017
end_year <-  2021

year <- paste(start_year, end_year, sep = "_")

completeness_threshold <- 95

# set the I/O path -----
input_dir <- "~/"

dir.create(file.path(input_dir, paste0("station_based_ehe_", year)))
output_path <- file.path(input_dir, paste0("station_based_ehe_", year))

# computational script ----

stations_annual_summary_compiled <- readRDS(file.path(input_dir,
                                                      "noaa_isd_ca_imputed_2016_2021",
                                                      paste0("ca_stations_annual_summaries_geo_2016_2021_",
                                                             completeness_threshold,
                                                             "complete.rds")))[[1]]
glimpse(stations_annual_summary_compiled)


stations_daily_summary_compiled <- readRDS(file.path(input_dir,
                                                     "noaa_isd_ca_imputed_2016_2021",
                                                     paste0("ca_stations_daily_summaries_imputed_geo_2016_2021_",
                                                            completeness_threshold,
                                                            "complete.rds")))[[1]]
glimpse(stations_daily_summary_compiled)

# Compute stations Compute baseline distribution  -----
stations_daily_means_rolling_avg <- stations_daily_summary_compiled %>%
  mutate(DATE = YYYY_MM_DD) %>%
  mutate(dm_apparent_temperature = apparent_temperature) %>% # dm:daily mean
  arrange(DATE) %>%
  group_by(station_id) %>%
  # apparent temperature averaged over the immediate past three-day period
  mutate(dm_at_3_0 = round(runner::mean_run(x = apparent_temperature,
                                            k = 3L, lag = 0L, idx = DATE,
                                            na_rm=TRUE), 2)) %>%
  # apparent temperature averaged over 30 days prior lagged by three days
  mutate(dm_at_32_m3 = round(runner::mean_run(x = apparent_temperature,
                                              k = 32L, lag = 3L, idx = DATE,
                                              na_rm=TRUE), 2)) %>% ungroup() %>%
  # defined the climatalogical period of analysis including 30 days prior (to compute rolling means)
  filter(DATE < as.POSIXct("2022-01-01") &
         DATE > as.POSIXct("2016-11-25") ) %>%
  data.table()

# Distribution (percentiles) of Mean Daily Apparent Temperature  ----
dm_apparent_temperature_percentiles <- stations_daily_means_rolling_avg[ ,
                                                                     c(list(median = median(dm_apparent_temperature, na.rm = T)),
                                                                       as.list(round(quantile(dm_apparent_temperature, c(.01, .02, .05, .075, .10, .15, .25,
                                                                                                                         .85, .90, .925, .95, .98, .99),
                                                                                              na.rm = T),2))
                                                                     ),
                                                                     by = .(station_id)] %>%
  janitor::clean_names()

dm_apparent_temperature_compiled <- merge(stations_daily_means_rolling_avg,
                                          dm_apparent_temperature_percentiles,
                                          by.x="station_id",
                                          by.y="station_id",
                                          all.x = TRUE)

# Compute Sheridan terms ----
Sheridan <- dm_apparent_temperature_compiled %>%
  mutate(EHaccl = dm_at_3_0 - dm_at_32_m3) %>%

  # excess heat based on the Nairn and Fawcett
  mutate(EH = pmax(0, (dm_at_3_0 - x95_percent))) %>%
  mutate(EHF = pmax(0, EH) * pmax(1, EHaccl))

# Identify extreme events ----
EHF_percentiles <- Sheridan[ EHF > 0,
                             c(list(median = median(EHF, na.rm = T)),
                               as.list(round(quantile(EHF, c(.01, .02, .05, .10, .15, .25,
                                                             .75, .85, .90, .95, .98, .99), na.rm = T),2))
                             ),
                             by = .(station_id)]  %>% clean_names() %>%
  rename_with(.cols = where(is.numeric), function(x){paste0("ehf_", x)})

Sheridan_compiled <- merge(Sheridan,
                           EHF_percentiles,
                           by.x="station_id",
                           by.y="station_id",
                           suffix  = c("", ""),
                           all.x = TRUE)


# Identify and tag individual days of extreme events ----
stations_daily_means_compiled <- Sheridan_compiled %>%
  mutate(EHE = case_when(EHF > ehf_x85_percent ~ 1,
                         TRUE ~ 0))

EHE <- stations_daily_means_compiled[, .(EHE = EHE[1],
                                         DATE = DATE,

                                         EHF = EHF,
                                         EHMI = EHF * .N,
                                         DM_AT = dm_apparent_temperature,
                                         EHE_day = cumsum(rleidv(EHE)),
                                         EHE_duration = .N,
                                         EHE_start = first(DATE),
                                         EHE_end = last(DATE),
                                         Event_Type = "Extreme Heat Event"
),
by = .(station_id, rleidv(EHE))][EHE == 1][, c('EHE', 'rleidv') := NULL][]%>%
  mutate(UID = paste0(strftime(EHE_start, format = "%Y%m%d"),
                      "1")) %>%
  mutate(EUID = paste0(strftime(EHE_start, format = "%Y%m%d"),
                       strftime(EHE_end, format = "%Y%m%d"),
                      "1"))

    # Compile stations daily values with identified events-----
     EHE_compiled <- merge(stations_daily_means_rolling_avg  %>%
                                 filter(YYYY >=  start_year &
                                          YYYY <=  end_year),
                               EHE,
                               by.x=c("station_id", "DATE"),
                               by.y=c("station_id", "DATE"),
                               all.x = TRUE) %>%
       mutate_at(vars(EHE_duration,
                      EHF, EHMI), ~replace_na(., 0))

write_rds(EHE_compiled, file=file.path(output_path,
                                           paste0("EHE_compiled_",
                                           completeness_threshold,
                                           ".rds")))

write_csv(EHE_compiled, file=file.path(output_path,
                                           paste0("EHE_compiled_",
                                           completeness_threshold,
                                                  ".csv")))

# Impute missing daily records

library(here)
library(tidyverse)
library(data.table)
#library(skimr)
#library(rgdal)
library(sf)

# Set up computational parameters ----
spatial_projection  = 4269 #NAD83
#plots dimensions
ww = 22 #width
hh = 28 #height

start_year <-  2008
end_year <-  2022

project_title <- "noaa_isd"
task_title <- "imputing_daily_values"
geography <- "us"

year <- paste(start_year, end_year, sep = "_")

st_label <- paste(geography, year, sep = "_")
l1_label <- paste(project_title, task_title, sep = "_")
l2_label <- paste(project_title, year, sep = "_")
l3_label <- paste(task_title, year, sep = "_")
l4_label <- paste(l1_label, geography, sep = "_")
l5_label <- paste(l1_label, year, sep = "_")
l6_label <- paste(l4_label, year, sep = "_")

cat(st_label,
    l1_label, l2_label, l3_label,
    l4_label, l5_label, l6_label,
    sep = "\n")

source(here::here("runtime_setup",
                  "1_helper_functions.R"), local = T)

project_prefix_label <- l1_label

source(here::here("runtime_setup",
                  "2_io.R"), local = T)

zip(paste0(reports_path,
           "/00_sources_code",
           timestamped, ".zip"),
    files = c(here("runtime_setup"),
              here("bash_scripts"),

              here("data_preparation")),

    extras = "-x \\*Report*")


# Start writing to an output file
sink(paste0(reports_path,
            "/02_R_env_info",
            timestamped, ".log"))

source(here("runtime_setup",
            "3_info.R"), local=T)
# Stop writing to the file
sink()

gc()


# I/O -----

#source_dir <- "/n/groups/patel/pedram/climate_data/"

#reports_path in param_path in interim_dir
#interim_dir <- "~/ws_runtime_reports/"
#source_dir <- "/n/scratch3/users/p/pef004/climate_data/noaa_isd_ca_web_compiled_daily_2008_2022"
source_dir <- "/n/groups/patel/pedram/climate_data/02_us_compiled_daily_2008_2022"
#dir.create(file.path(scratch_dir, paste0("noaa_isd_ca_summaries_imputed_", year)))


#results_dir <- "/n/groups/patel/pedram/climate_data/isd_imputed_daily_records/"
results_dir_root <- "/n/groups/patel/pedram/climate_data/"

dir.create(file.path(results_dir_root, paste0("03_us_imputed_isd_daily_", year)))
output_dir <- file.path(results_dir_root, paste0("03_us_imputed_isd_daily_", year))

#dir.create(file.path(results_dir_root, paste0("noaa_isd_summaries_imputed_", year)))
#output_dir <- file.path(results_dir_root, paste0("noaa_isd_summaries_imputed_", year))

dir.create(file.path(output_dir, "Plots"))
dir.create(file.path(output_dir, "RObject_Files"))
dir.create(file.path(output_dir, "Metadata"))

plots_output_path <- file.path(output_dir, paste0("Plots"))
meta_output_path <- file.path(output_dir, paste0("Metadata"))
rds_output_path <- file.path(output_dir, paste0("RObject_Files"))


# Computational script ----
# noaa_isd_2021_sf_ca_state_100km_simplified <- readRDS(file.path(source_dir,
#                                                               "noaa_isd_2021_sf_ca_state_100km_simplified.rds"))[[1]]

#dim(noaa_isd_2021_sf_ca_state_100km_simplified)
#glimpse(noaa_isd_2021_sf_ca_state_100km_simplified)

# Load daily and annual summaries from compiled yearly files
# processed_daily_data_path <- file.path(source_dir,
#                                        #"2022")
#                                        "daily")
#
# processed_annual_data_path <- file.path(source_dir,
#                                         #"noaa_isd_ca_web_compiled_daily_2000",
#                                         #"RObject_Files")
#                                         "annual")

daily_processed_csv_list <- list.files(path = source_dir,
                                       pattern = "*daily*")

annual_processed_csv_list <- list.files(path = source_dir,
                                        pattern = "*annual*")

all_daily_summary_collector <- list()
all_annual_summary_collector <- list()

# Merge all the yearly tables for the entire study period-----
#sapply(1:length(daily_processed_csv_list), function(i) {
for (i in seq(1, length(daily_processed_csv_list))) {
  #i <- 1
  print(i)

  # ANNUAL ----

  each_year_daily_df <- readRDS(file.path(source_dir,
                                                   daily_processed_csv_list[[i]]))

  each_year_annual_df <- readRDS(file.path(source_dir,
                                          annual_processed_csv_list[[i]])) %>%
  # ,
  #                              header = TRUE,
  #                              check.names = FALSE) %>%
  #   as_tibble() %>%
     mutate(station_id = as.character(station_id)) %>%
  # ## Compute completeness columns for the three variables ----
      mutate(
        #temperature_completeness = (1 - temperature_annual_null_ratio),
        temperature_completeness_label = paste0(".3", temperature_comp_ratio)) %>%
      mutate(
  #      temperature_dewpoint_completeness = (1 - temperature_dewpoint_annual_null_ratio),
        temperature_dewpoint_completeness_label = paste0(".3", temperature_dewpoint_comp_ratio)) %>%
      mutate(
  #      wind_speed_completeness = (1 - wind_speed_annual_null_ratio),
        wind_speed_completeness_label = paste0(".3", wind_speed_comp_ratio)) %>%

     mutate(completeness_above_98_percent =
              case_when(
                (temperature_comp_ratio > 0.98 &
                  temperature_dewpoint_comp_ratio > 0.98 &
                  wind_speed_comp_ratio > 0.98)
                ~ 1L, TRUE ~ 0L)) %>%
     mutate(completeness_above_95_percent =
              case_when(
                (temperature_comp_ratio >= 0.95 &
                  temperature_dewpoint_comp_ratio >= 0.95 &
                  wind_speed_comp_ratio >= 0.95)
                ~ 1L, TRUE ~ 0L)) %>%
     mutate(completeness_above_90_percent =
              case_when(
                (temperature_comp_ratio >= 0.90 &
                  temperature_dewpoint_comp_ratio >= 0.90 &
                  wind_speed_comp_ratio >= 0.90)
                ~ 1L, TRUE ~ 0L)) %>%
     mutate(completeness_above_85_percent =
              case_when(
                (temperature_comp_ratio >= 0.85 &
                  temperature_dewpoint_comp_ratio >= 0.85 &
                  wind_speed_comp_ratio >= 0.85)
                ~ 1L, TRUE ~ 0L)) %>%
     mutate(completeness_above_80_percent =
              case_when(
                (temperature_comp_ratio >= 0.80 &
                  temperature_dewpoint_comp_ratio >= 0.80 &
                  wind_speed_comp_ratio >= 0.80)
                ~ 1L, TRUE ~ 0L)) %>%
     mutate(completeness_above_75_percent =
             case_when(
               (temperature_comp_ratio >= 0.75 &
                 temperature_dewpoint_comp_ratio >= 0.75 &
                 wind_speed_comp_ratio >= 0.75)
               ~ 1L, TRUE ~ 0L)) %>%

    mutate(completeness_code = completeness_above_75_percent +
             completeness_above_80_percent + completeness_above_85_percent +
             completeness_above_90_percent + completeness_above_95_percent +
             completeness_above_98_percent,

             completeness_percent = 100-((6-completeness_code)*5))

  # DAILY ----
  each_year_annual_df_qc <- each_year_annual_df %>%
    dplyr::select(ends_with("_id") | ends_with("completeness_code") |
                    contains("YYYY") )

  each_year_daily_df_raw <- readRDS(file.path(source_dir,
                                        daily_processed_csv_list[[i]])) %>%
  #,
   #                           header = TRUE,
    #                          check.names = FALSE) %>% # drop unnamed first column
    #as_tibble() %>%
    # adjust the columns data type
    mutate(station_id = as.character(station_id)) %>%
    mutate(YYYY_MM_DD = as.Date(format(YYYY_MM_DD), "%Y-%m-%d"), # create date data type column
    )
  # Assign completeness info to daily tables
  each_year_daily_df <-
    merge(each_year_daily_df_raw,
          each_year_annual_df_qc,
          #
          by.x = c("station_id", "YYYY"),
          by.y = c("station_id", "YYYY"),
          all.x = TRUE)


  all_daily_summary_collector[[i]] <- each_year_daily_df
  all_annual_summary_collector[[i]] <- each_year_annual_df
  print(length(all_annual_summary_collector))

  gc()

} # *1 close the for loop ----
#}) # *2 close the sapply ----

# Compile summaries ----
#length(all_daily_summary_collector)
## Annual-----
# #### Assign "Completeness" flags ----

 all_annual_summaries <- dplyr::bind_rows(all_annual_summary_collector) %>%
   dplyr::filter(YYYY >= start_year )
#
# glimpse(all_annual_summaries)
# dim(all_annual_summaries) /22

## Daily-----
## Replace inf values with NA

all_daily_summaries_inf2na <- dplyr::bind_rows(all_daily_summary_collector) %>%
  dplyr::filter(YYYY >= start_year) %>%
  # convert inf to nulls
  mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))

dim(all_daily_summaries_inf2na)
glimpse(all_daily_summaries_inf2na)

# Check number of nulls
#sum(is.na(all_daily_summaries_inf2na$completeness_code))
sum(is.na(all_daily_summaries_inf2na$temperature_avg))
sum(is.na(all_daily_summaries_inf2na$temperature_dewpoint_min))
sum(is.na(all_daily_summaries_inf2na$wind_speed_avg))

selected_variables <- c(
  "temperature_avg",
  "wind_speed_avg",
  "temperature_dewpoint_avg",
  "YYYY_MM_DD"
)
# << Export combined stations data ----

all_daily_inf2na_notimputed <- all_daily_summaries_inf2na %>%
  mutate(completeness_percent = 100-((6-completeness_code)*5)) %>%
  group_by(YYYY, completeness_percent
           ) %>%
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
# << Export compiled stations data ----
fx_saveCSVPar(
  all_daily_inf2na_notimputed,

  prefix = geography,
  suffix = "input_for_imputation_by_transitive_completeness",
  output_path = output_dir)

fx_saveRObjects(#noaa_isd_2021_sf_ca_state_100km_simplified,
                all_annual_summaries,
                all_daily_summaries_inf2na,

                prefix = geography,
                suffix = year,
                output_path = rds_output_path)

fx_reportFieldsPar(#noaa_isd_2021_sf_ca_state_100km_simplified,
                   all_annual_summaries,
                   all_daily_summaries_inf2na,

                #prefix = geography,
                suffix = year,
                output_path = meta_output_path)

    # list number of null values for each variables
    dim(all_daily_summaries_inf2na)
    sum(is.na(all_daily_summaries_inf2na$temperature_avg))/nrow(all_daily_summaries_inf2na)
    sum(is.na(all_daily_summaries_inf2na$temperature_dewpoint_avg))/nrow(all_daily_summaries_inf2na)
    sum(is.na(all_daily_summaries_inf2na$wind_speed_avg))/nrow(all_daily_summaries_inf2na)

    # QC ----
    gc()

     source(here("data_preparation",
                 "0500_apply_completeness_criteria.R"), local=T)

    #hist(all_annual_summaries$completeness_code)
    #hist(all_daily_summaries_inf2na$completeness_code)


    ## Vis: missing values gapsize and number of stations ----
      # source(here::here("data_preparation",
      #                   "0504_vis_stations_lineplot_by_qc_threshold.R"), local=T)

    ggplot(all_daily_summaries_inf2na, aes(
      x = YYYY,
      group = YYYY,
      y=num_columns_day)) +
      geom_boxplot() +
      #geom_jitter() +
      labs(title = paste0("Climatological Data Table Structure - US (2008 - 2022)")) +
      labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)"
      )) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_y_continuous(#labels = percent,
        breaks = scales::pretty_breaks(n = 8)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 12))
      #labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)")) +
      #coord_flip()

    # ggplot(all_daily_summaries_inf2na, aes(y=num_columns_day)) +
    #   geom_histogram() +
    #   coord_flip()

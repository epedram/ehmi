## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Compile daily weather records based on the hourly NOAA ISD climate data
## Visualize missing daily records

library(here)
library(tidyverse)
library(data.table)

# set up the parameters ----
### * 1 ----
start_year <-  2008
end_year <-  2022

year <- paste(start_year, end_year, sep = "_")

# set the I/O path -----
input_dir <- "/n/groups/patel/pedram/climate_data/01_us_web_raw_hourly_data_2008_2022"

#scratch_dir <- "/n/scratch3/users/p/pef004/climate_data/"
scratch_dir <- "/n/groups/patel/pedram/climate_data/"

dir.create(file.path(scratch_dir, paste0("us_compiled_daily_", year)))
output_path <- file.path(scratch_dir, paste0("us_compiled_daily_", year))

# set up computational parameters ----
#### Selected_variables -----
selected_variables <- c(
  "temperature",
  "temperature_dewpoint",
  "wind_speed")

#### Eliminated_types -----
eliminated_types <- c(
  "SOM", "SOD", " ",
  "COOPD", "COOPS",
  "PCP15", "PCP60")

# load the doParallel package and set the number of cores to use
library(doParallel)
cores <- 15
# create a cluster object
cl <- makeCluster(cores)
# register the cluster
registerDoParallel(cl)


foreach(year_i = seq(start_year, end_year), .verbose = TRUE) %dopar% {
#for (year_i in seq(start_year, end_year)) {
  source(here::here("runtime_setup",
                    "1_helper_functions.R"), local = T)
  #year_i <- 2018
    dir.create(file.path(output_path, year_i))
    missing_data_output_path <- file.path(output_path, year_i)

    library(here)
    library(tidyverse)
    library(data.table)
    library(isdparser)
    library(naniar)

    # Read the raw data in CSV format from permanent storage
    raw_stations_data_path <- file.path(input_dir,
                                        year_i)
    ### * 2 ----
    annual_csv_list <- list.files(path = raw_stations_data_path,
                                  pattern = ".csv")#[1408:1412]

    stations_daily_summary_collector <<- list()
    stations_seasonal_summary_collector <<- list()
    stations_annual_summary_collector <<- list()
    parsing_type_list <<- list()

    erroneous_stations_list = c()
    erroneous_stations_year = c()
    erroneous_stations_message = c()
    error_counter_i <- 0
    rows_counter <- 0

    # >> Load and pars stations raw data ----
    sapply(1:length(annual_csv_list), function(i) {
      #for(i in 1:length(annual_csv_list)){
      #i =1
      cat(year_i, i)

        each_station_id <- annual_csv_list[[i]] %>% str_sub(., start = 1L, end = 11L)

        each_station_cvs_path <- file.path(raw_stations_data_path,
                                           annual_csv_list[[i]])

        tryCatch({
          parsed_web_file <- isdparser::isd_parse_csv(each_station_cvs_path) %>%
            ## added to resolve the transformation function bug
            mutate(total_chars = 0L)
            parsing_type <- "by_table"

        }, error = function(e){
          parsed_web_file <- isdparser::isd_parse(each_station_cvs_path, additional = FALSE)
          parsing_type <- "by_row"

          erroneous_stations_list <<- append(erroneous_stations_list, each_station_id)
          erroneous_stations_year <<- append(erroneous_stations_year, year_i)
          erroneous_stations_message <<- append(erroneous_stations_message,
                                                as.character(conditionMessage(e)))
          error_counter_i <- error_counter_i + 1
        })

        tryCatch({
        ## handle data type and unit conversions
        each_station_nrow <- nrow(parsed_web_file)

        rows_counter <<- rows_counter + each_station_nrow

        ### filter out certain report types ----
        transformed_parsed_web_file <- isdparser::isd_transform(parsed_web_file) %>%
          mutate(station_id = as.character(station)) %>%
          as_tibble() %>%
          dplyr::filter(!grepl(' ', report_type)) %>%
          dplyr::filter(!report_type %in% eliminated_types)

        ### populate placeholder for missing dates
        each_station_df <- transformed_parsed_web_file %>%

          mutate(
            YYYY_MM_DD = as.Date(format(date), "%Y-%m-%d"),
            month_name = lubridate::month(format(date, "%Y-%m-%d"), label = TRUE)) %>%
          mutate(
            season = fct_collapse(
              .f = month_name,
              Spring = c("Mar", "Apr", "May"),
              Summer = c("Jun", "Jul", "Aug"),
              Autumn = c("Sep", "Oct", "Nov"),
              Winter = c("Dec", "Jan", "Feb"))
          ) %>%
          mutate(YYYY = year_i) %>%

          naniar::replace_with_na_at(.vars = selected_variables,
                                     condition = ~.x > 900) # to remove invalid temperature readings

        print("Compiling summaries")
        print("Daily")

        ## compute daily summaries for one station for a certain year----
        station_daily_summary <- each_station_df %>%
          group_by(station_id,
                   YYYY_MM_DD) %>%
          dplyr::summarise(
            num_columns_day = ncol(.),
            num_obs_per_day = n(),
            report_types_day = toString(unique(report_type)),

            across(all_of(selected_variables),
                   list(
                     avg = ~round(mean(as.integer(.), na.rm = T), 1),
                     min = ~round(min(as.integer(.), na.rm = T), 1),
                     max = ~round(max(as.integer(.), na.rm = T), 1)
                   )
            ), .groups = 'drop'
          ) %>%
            tidyr::complete(station_id, num_columns_day,
                            YYYY_MM_DD = seq.Date(as.Date(paste0(year_i, "-01-01"), "%Y-%m-%d"),
                                                  as.Date(paste0(year_i, "-12-31"), "%Y-%m-%d"),
                                                  by="day")
            ) %>%
          mutate(YYYY = year_i) %>%
          # convert inf to nulls
          mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
          mutate(
            month_name = lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE)
            ) %>%
          mutate(
            season = fct_collapse(
              .f = month_name,
              Spring = c("Mar", "Apr", "May"),
              Summer = c("Jun", "Jul", "Aug"),
              Autumn = c("Sep", "Oct", "Nov"),
              Winter = c("Dec", "Jan", "Feb"))
          ) %>%
          mutate(num_obs_per_day = replace_na(num_obs_per_day, 0))

        print("Annual")
        # compute annual summaries for one stations for a certain year----
        station_annual_summary <- station_daily_summary %>%
          group_by(station_id) %>%
          dplyr::summarise(
            num_columns = max(num_columns_day),
            num_obs_per_year = sum(num_obs_per_day, na.rm=T),
            num_distinct_days = n_distinct(YYYY_MM_DD, na.rm=T),
            distinct_days_ratio = round(n_distinct(YYYY_MM_DD, na.rm=T)/365, 2) ,
            avg_num_obs_per_day = round(num_obs_per_year/num_distinct_days, 1),

            temperature_comp_ratio = round(sum(!is.na(temperature_avg))/365, 3),
            temperature_dewpoint_comp_ratio = round(sum(!is.na(temperature_dewpoint_avg))/365, 3),
            wind_speed_comp_ratio = round(sum(!is.na(wind_speed_avg))/365, 3),

            .groups = 'drop'
          ) %>%
          mutate(YYYY = year_i) %>%
          # convert inf to nulls
          mutate(across(.cols = where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))


        cat("\n", annual_csv_list[[i]], "completed")

        # visualize missing daily values----
        daily_selected_vars <- station_daily_summary %>%
          dplyr::select(ends_with("station_id") | starts_with("YYYY_MM_DD") |
                          ends_with("_avg") | ends_with("_min") | ends_with("_max")) %>%
          tidyr::complete(station_id,
                          YYYY_MM_DD = seq.Date(as.Date(paste0(year_i, "-01-01"), "%Y-%m-%d"),
                                   as.Date(paste0(year_i, "-12-31"), "%Y-%m-%d"),
                                   by="day")
          )

          tryCatch({
            print("Missing value visualization")
         v_missing <- visdat::vis_miss(daily_selected_vars)
         v_missing_plot_fn <- paste0(missing_data_output_path, "/",
                                     "vis_miss_",
                                     each_station_id,
                                     "_",
                                     year_i,
                                     ".jpg")

         ggsave(v_missing_plot_fn,
                plot = v_missing, dpi = 200,
                width = 18, height = 28, units = "cm")

          #visualize missing records distribution
          v_missing1 <- imputeTS::ggplot_na_distribution(daily_selected_vars$temperature_avg,
                                                         title = paste0("NOAA Station: ", each_station_id,
                                                                        " - Variable: Temperature - Year (",
                                                                        year_i,
                                                                        ")"))
          v_missing_plot_fn1 <- paste0(missing_data_output_path, "/",
                                      "na_distribution_tmp_",
                                      each_station_id,
                                      "_",
                                      year_i,
                                      ".jpg")

          ggsave(v_missing_plot_fn1,
                 plot = v_missing1, dpi = 200,
                 width = 32, height = 16, units = "cm")

          v_missing2 <- imputeTS::ggplot_na_distribution(daily_selected_vars$wind_speed_avg,
                                                         title = paste0("NOAA Station: ", each_station_id,
                                                                        " - Variable: Wind Speed - Year (",
                                                                        year_i,
                                                                        ")"))
          v_missing_plot_fn2 <- paste0(missing_data_output_path, "/",
                                       "na_distribution_wnd_",
                                       each_station_id,
                                       "_",
                                       year_i,
                                       ".jpg")

          ggsave(v_missing_plot_fn2,
                 plot = v_missing2, dpi = 200,
                 width = 32, height = 16, units = "cm")


          v_missing3 <- imputeTS::ggplot_na_distribution(daily_selected_vars$temperature_dewpoint_avg,
                                                         title = paste0("NOAA Station: ", each_station_id,
                                                                        " - Variable: Dew Point Temperature - Year (",
                                                                        year_i,
                                                                        ")"))
          v_missing_plot_fn3 <- paste0(missing_data_output_path, "/",
                                       "na_distribution_dew_",
                                       each_station_id,
                                       "_",
                                       year_i,
                                       ".jpg")

          ggsave(v_missing_plot_fn3,
                 plot = v_missing3, dpi = 200,
                 width = 32, height = 16, units = "cm")

          }, error = function(nonull){
            cat("Visualization Error :",conditionMessage(nonull), "\n")
          })

        print("Daily_summaries")
        stations_daily_summary_collector[[i]] <<- station_daily_summary

        print("Annual_summaries")
        stations_annual_summary_collector[[i]] <<- station_annual_summary

      }, error = function(e){
        print(annual_csv_list[[i]])

        cat("ERROR :",conditionMessage(e), "\n")

        erroneous_stations_list <<- append(erroneous_stations_list, each_station_id)
        erroneous_stations_year <<- append(erroneous_stations_year, year_i)
        erroneous_stations_message <<- append(erroneous_stations_message,
                                              as.character(conditionMessage(e)))
        error_counter_i <- error_counter_i + 1
      })
      gc()

    })
    # bind daily climate data for all the stations for a certain year----
    print("Combine Daily Tables")
    stations_daily_summary <- dplyr::bind_rows(stations_daily_summary_collector) %>%
      mutate(
        month_name = as.factor(lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE))
        ) %>%
      mutate(
        season = fct_collapse(
          .f = month_name,
          Spring = c("Mar", "Apr", "May"),
          Summer = c("Jun", "Jul", "Aug"),
          Autumn = c("Sep", "Oct", "Nov"),
          Winter = c("Dec", "Jan", "Feb"))
      ) %>%
      mutate(year_factor = as.factor(YYYY)) %>%
      mutate(month_factor = as.factor(month_name)) %>%
      mutate(season_factor = as.factor(season))

    stations_annual_summary <- dplyr::bind_rows(stations_annual_summary_collector) %>%
      mutate(year_factor = as.factor(YYYY))

    ## << Export summarized stations data ----
    saveRDS(stations_daily_summary, file=file.path(output_path,
                                                     paste0("stations_daily_summary_",
                                                            year_i,
                                                            ".rds")))

    saveRDS(stations_annual_summary, file=file.path(output_path,
                                                     paste0("stations_annual_summary_",
                                                            year_i,
                                                            ".rds")))

    n_raw_hourly_records_yearly_report <- cbind(
      as.list(year_i),
      as.list(rows_counter))


    fx_saveCSVPar(n_raw_hourly_records_yearly_report,
                  prefix = year_i,
                  suffix = rows_counter,

                  output_path = missing_data_output_path)

    fx_appendCSV(n_raw_hourly_records_yearly_report,
                 prefix = "all_",
                 suffix = year,

                 output_path = output_path)

}

  print(showConnections())
  print(stopCluster(cl))
  print(rm(cl))

    n_raw_hourly_records_yearly_report_2008_2022 <- fread("~/data_interim/n_raw_hourly_records_yearly_report_2008_2022.csv")

glimpse(n_raw_hourly_records_yearly_report_2008_2022)

ggplot(n_raw_hourly_records_yearly_report_2008_2022, aes(
  x = Year,
  #group = YYYY,
  y=n_raw_parsed )) +

  geom_bar(stat="identity") +
  theme_bw()

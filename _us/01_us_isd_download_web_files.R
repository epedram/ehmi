## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Retrieve NOAA ISD climate data

library(here)
library(tidyverse)
library(data.table)

# set up the parameters ----
start_year <-  2007
end_year <-  2022

year <- paste(start_year, end_year, sep = "_")

spatial_projection <-  2163 #epsg

# set the I/O path -----
input_dir_root <- "/n/groups/patel/pedram/climate_data/"

dir.create(file.path(input_dir_root, paste0("01_us_web_raw_hourly_data_", year)))
input_dir <- file.path(input_dir_root, paste0("01_us_web_raw_hourly_data_", year))

scratch_dir <- "/n/scratch3/users/p/pef004/climate_data/"

dir.create(file.path(scratch_dir, paste0("us_web_raw_hourly_data_", year)))
output_path <- file.path(scratch_dir, paste0("us_web_raw_hourly_data_", year))

# read in station data for US from a CSV file in the temporary working directory and filter data
us_noaa_isd_stations <- fread(file.path(scratch_dir,
                                        "noaa_isd_us_web_raw_hourly_data_USA",
                                        "us_noaa_stations_2000_2022.csv")) %>%
                                        filter(between(YYYY, start_year, end_year))

unique(us_noaa_isd_stations$YYYY)

length(unique(us_noaa_isd_stations$station_id))
#3529

### * 1 ----
station_list <- unique(us_noaa_isd_stations$station_id)#[1408:1412] #sample
length(station_list)




#### Selected_variables -----
selected_variables <- c(
  "temperature",
  "temperature_dewpoint",
  "wind_speed")

# retrieve stations raw data through a parallel loop----
# load the doParallel package and set the number of cores to use
library(doParallel)
cores <- 16

# create a cluster object
cl <- makeCluster(cores)
# register the cluster
registerDoParallel(cl)

foreach(year_i = seq(start_year, end_year), .verbose = TRUE) %dopar% {
#for (year_i in seq(start_year, end_year)) {
  #year_i <- 2018

  library(here)
  library(dplyr)
  library(data.table)



  library(logger)
  log_threshold(ERROR)
  #my_logger <- logger(ERROR)

  #Define variables for missing stations list
  erroneous_stations_list <- "Station_id"
  erroneous_stations_year <- "Year"
  erroneous_stations_message <- "Error_message"

  #Define variables for missing stations list
  compiled_stations_list <- "Station_id"
  parsing_type_list <- "Parsing_type"
  num_rows_list <- "Num_rows"
  num_cols_list <- "Num_columns"
  compiled_stations_year <- "Year"


  error_counter_i <- 0

  dir.create(file.path(input_dir, year_i))
  download_dest_path <- file.path(input_dir, year_i)

  dir.create(file.path(output_path, paste0(year_i, "_refined_rds_files")))
  yearly_path_rds <- file.path(output_path, paste0(year_i, "_refined_rds_files"))

  dir.create(file.path(output_path, paste0(year_i, "_refined_csv_files")))
  yearly_path_csv <- file.path(output_path, paste0(year_i, "_refined_csv_files"))

  # loop through the stations list #[1408:1412]
  for (each_station in station_list) {
    #each_station <- "72472099999"
    # generate download urls based on the NOAA web api
    station_year_url <- paste0("https://www.ncei.noaa.gov/data/global-hourly/access/",
                               year_i, "/",
                               each_station, ".csv"
                               )
    # expect some download links cause errors
     tryCatch({
       # download by specific url
         each_station_filename <- paste0(each_station, ".csv")
         dumped_csv_path <- here(download_dest_path, each_station_filename)

         #log_error('This is a problem with the tryCatch')

         download.file(url = station_year_url,
                       destfile = dumped_csv_path,
                       quiet = TRUE,
                       mode = "wb")

         #log_info(my_logger, "The result is ")

         }, error = function(e) {

         #log_error('This is a problem with the Error')
          log_error("Error occurred: ", e$message)
           erroneous_stations_list <<- append(erroneous_stations_list, each_station)
           erroneous_stations_year <<- append(erroneous_stations_year, year_i)
           erroneous_stations_message <<- append(erroneous_stations_message,
                                               as.character(conditionMessage(e)))

         })

         tryCatch({
         downloaded_csv_df <- fread(dumped_csv_path,

                                         header = TRUE,
                                         check.names = FALSE) %>%
           # clean up the columns with "," issue
           #select(-one_of('EQD', 'REM')) %>%
           mutate(REM = "REM", EQD = "EQD")

         # write the refined station tables to disk for further parsing
         write.csv(downloaded_csv_df,
                   file.path(yearly_path_csv,
                             paste0(each_station, "_",
                                    year_i,
                                    ".csv")),
                   row.names = FALSE)

         tryCatch({
         parsed_web_file_df <- isdparser::isd_parse_csv(dumped_csv_path) %>%
           ## add a dummy value to resolve the transformation function bug
          mutate(total_chars = 0L)
         parsing_type <- "by_table"

         }, error = function(e){
         parsed_web_file_df <- isdparser::isd_parse(dumped_csv_path, additional = FALSE)
         parsing_type <- "by_row"

         erroneous_stations_list <<- append(erroneous_stations_list, each_station)
         erroneous_stations_year <<- append(erroneous_stations_year, year_i)
         erroneous_stations_message <<- append(erroneous_stations_message,
                                             as.character(conditionMessage(e)))
         })

         ## handle data type and unit conversions
         transformed_parsed_web_file <- isdparser::isd_transform(parsed_web_file_df) %>%
           mutate(station_id = as.character(station)) %>%
           as_tibble() %>%
           mutate(
             YYYY_MM_DD = as.Date(format(date), "%Y-%m-%d"), # create date data type column
             YYYY = year_i) %>%
           naniar::replace_with_na_at(.vars = selected_variables,
                                      condition = ~.x > 900)

         # export compiled stations data for individual table inspection
         # this file cannot be parsed with isd_parser
         saveRDS(transformed_parsed_web_file,
                 file.path(yearly_path_rds,
                 paste0(each_station, "_",
                        year_i,
                        ".rds")))

         cat("\n Station :", each_station, " table completed \n")

         # compile a data structure statistics for each station
         num_cols <- ncol(downloaded_csv_df)
         num_rows <- nrow(downloaded_csv_df)
         parsing_type_list <- append(parsing_type_list, parsing_type)

         compiled_stations_list <- append(compiled_stations_list, each_station)
         num_cols_list <- append(num_cols_list, num_cols)
         num_rows_list <- append(num_rows_list, num_rows)
         compiled_stations_year <- append(compiled_stations_year, year_i)

 #     # if station cannot be found
       }, error = function(e){
         error_counter_i <- error_counter_i + 1

         cat("\n Missing station: ", each_station,
         "\n ERROR MESSAGE:",conditionMessage(e), "\n")

    #     # write the missing station id to a list
         erroneous_stations_list <<- append(erroneous_stations_list, each_station)
         erroneous_stations_year <<- append(erroneous_stations_year, year_i)
         erroneous_stations_message <<- append(erroneous_stations_message,
                                               as.character(conditionMessage(e)))

         print(erroneous_stations_message)
         print(error_counter_i)
       }
    )}

  compiled_stations_report_collection <<- cbind(
                                          as.list(compiled_stations_list),
                                          as.list(parsing_type_list),
                                          as.list(num_rows_list),
                                          as.list(num_cols_list),
                                          as.list(compiled_stations_year))

  write.csv(compiled_stations_report_collection,
            file.path(output_path,
                      paste0("compiled_stations_report_",
                             year_i,
                             ".csv")),
            row.names = FALSE)

  # report missing stations ----
  print(error_counter_i)
  erroneous_stations_report_collection <<- cbind(
                                   as.list(erroneous_stations_list),
                                   as.list(erroneous_stations_message),
                                   as.list(erroneous_stations_year))

  write.csv(erroneous_stations_report_collection,
            file.path(output_path,
                   paste0("erroneous_stations_report_",
                   year_i,
                   ".csv")),
            row.names = FALSE)

} # closing the yearly loop
print(showConnections())

print("Closing")
print(stopCluster(cl))


isd_compiled_files <- list.files(output_path, pattern = "*compiled*")
isd_compiled_table <- map(isd_compiled_files, ~read_csv(file=file.path(output_path,.), skip = 1)) %>% bind_rows()

write.csv(isd_compiled_table,
          file.path(output_path,
                    paste0("all_compiled_tables_",
                           year,
                           ".csv")),
          row.names = T)


isd_missing_files <- list.files(output_path, pattern = "*missing*")
isd_missing_table <- map(isd_missing_files, ~read_csv(file=file.path(output_path,.), skip = 1)) %>% bind_rows()

write.csv(isd_missing_table,
          file.path(output_path,
                    paste0("all_missing_tables_",
                           year,
                           ".csv")),
          row.names = T)

saveRDS(isd_compiled_table,
        file.path(output_path,
                  paste0("isd_compiled_table", "_",
                         year,
                         ".rds")))


#log_info(my_logger, "End of script")

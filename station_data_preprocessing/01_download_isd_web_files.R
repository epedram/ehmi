## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

## Retrieve NOAA ISD climate data

library(here)
library(tidyverse)
library(data.table)

# set up the parameters ----
start_year <-  2016
end_year <-  2021

year <- paste(start_year, end_year, sep = "_")

# set the I/O path -----
input_dir <- "~/"

dir.create(file.path(input_dir, paste0("noaa_isd_ca_web_raw_hourly_data_", year)))
output_path <- file.path(input_dir, paste0("noaa_isd_ca_web_raw_hourly_data_", year))

noaa_isd_2021_ca_state_100km <- fread(file.path(input_dir,
                                                "noaa_isd_2021_ca_state_100km.csv"))

station_list <- noaa_isd_2021_ca_state_100km$station_id

# retrieve stations raw data through a parallel loop----
# load the doParallel package and set the number of cores to use
library(doParallel)
cores <- 6
# create a cluster object
cl <- makeCluster(cores)
# register the cluster
registerDoParallel(cl)

foreach(year_i = seq(start_year, end_year), .verbose = TRUE) %dopar% {

  library(here)
  library(dplyr)
  library(data.table)

  missing_stations_list = c()
  missing_stations_year = c()
  error_counter_i <- 1

  dir.create(file.path(tempdir(), year_i))
  download_dest_path <- file.path(tempdir(), year_i)

  dir.create(file.path(output_path, year_i))
  yearly_path <- file.path(output_path, year_i)

  # loop through the stations list
  for (each_station in station_list) {

    # generate download urls based on the NOAA web api
    station_year_url <- paste0("https://www.ncei.noaa.gov/data/global-hourly/access/",
                               year_i, "/",
                               each_station, ".csv"
                               )
    # expect some station have errors
     tryCatch(
       {
       # download by specific url
         each_station_filename <- paste0(each_station, ".csv")

         download.file(url = station_year_url,
                       destfile = here(download_dest_path,
                                       each_station_filename),
                       quiet = TRUE,
                       mode = "wb")

         # write the downloaded station table on disk
         downloaded_csv <- fread(here(download_dest_path,
                                      each_station_filename),

                                         header = TRUE,
                                         check.names = FALSE)

         write.csv(downloaded_csv,
                   file.path(yearly_path,
                   paste0(each_station, "_",
                          year_i,
                          ".csv")),

                   row.names = FALSE)

         cat("station :", each_station, " processing completed\n")

    #     # if station cannot be found
       }, error = function(e){
         cat("\n Missing station: ",each_station)

    #     # write the missing station id to a list
         missing_stations_list <<- append(missing_stations_list, each_station)
         missing_stations_year <<- append(missing_stations_year, year_i)
         error_counter_i <- error_counter_i + 1
       }
    )
  }

  #write_annual_list of missing stations
  missing_stations_report <<- cbind(#col_index,
                                   as.list(missing_stations_list),
                                   as.list(missing_stations_year))

    # report missing stations ----
  write.csv(missing_stations_report,

            file.path(output_path,
                   paste0("missing_stations_report_",
                   year_i,
                   ".csv")),

            row.names = FALSE)
}
print(showConnections())

print("Closing")
print(stopCluster(cl))

## Author: Pedram Fard
## Email: Pedram_Fard@hms.harvard.edu

## Postdoctoral Research Fellow at Chirag Patel Group
## Department of Biomedical Informatics, Harvard Medical School

# Subset the combined daily data set based on completeness criteria ----
# ## Loop through completeness thresholds ----
 library(doParallel)
 cores <- 6
 cl <- makeCluster(cores)
 registerDoParallel(cl)
 print(cl)

foreach(i = 6:1, .verbose = TRUE) %dopar% {
  library(here)
  library(tidyverse)
  library(data.table)
  library(sf)

  threshold_label <- (100-((6-i)*5))
  print(threshold_label)


  qc_annual_summaries <- all_annual_summaries %>%
    dplyr::filter(completeness_code >= i)

  qc_daily_summaries <- all_daily_summaries_inf2na %>%
    dplyr::filter(completeness_code >= i) %>%
    group_by(station_id)

  # Impute missing values ----
  library(imputeTS)
  ## https://www.rdocumentation.org/packages/imputeTS/
  qc_daily_summaries_imputed <- qc_daily_summaries %>%
    imputeTS::na_interpolation(., maxgap = 3) %>%
    imputeTS::na_kalman(., maxgap = maxgap + 2) # for imputing larger gap sizes

  ## Compute Heat Index ----
  qc_daily_summaries_imputed_heat_index <- qc_daily_summaries_imputed %>%
    mutate(relative_humidity_avg = weathermetrics::dewpoint.to.humidity(t = temperature_avg,
                                                               dp = temperature_dewpoint_avg,
                                                               temperature.metric = "celsius")) %>%
    
    mutate(relative_humidity_avg_hum_lib = humidity::RH(t = temperature_avg,
                                                      Td = temperature_dewpoint_avg,
                                                      isK = F)) %>%
    
    # address observations for which the imputed dew point temperature was higher than the temperature
    # * 4 imputation settings Kalman/NoKalman----
    imputeTS::na_interpolation(., maxgap = maxgap + 2) %>% 
  
    # https://github.com/caijun/humidity (2019)
    mutate(vapure_pressure_avg = humidity::WVP1(Td = temperature_dewpoint_avg, 
                                                isK = F) * 0.1) %>%
    # convert hectopascal (hPa) to kilopascal (kPa) 
  
    # https://github.com/anacv/HeatStress
    mutate(apparent_temperature_heatstress_lib = HeatStress::apparentTemp(
        tas = temperature_avg,
        hurs = relative_humidity_avg,
        wind = wind_speed_avg)) %>%
    
    mutate(affective_temperature_heatstress_lib = HeatStress::effectiveTemp(
      tas = temperature_avg,
      hurs = relative_humidity_avg,
      wind = wind_speed_avg)) %>%
       
    mutate(heat_index_heatstress_lib = HeatStress::hi(
      tas = temperature_avg,
      hurs = relative_humidity_avg)) %>%
    
    # https://github.com/geanders/weathermetrics (2017)
    mutate(heat_index_weathermetrics_lib = weathermetrics::heat.index(
        t = temperature_avg,
        dp = temperature_dewpoint_avg,
        temperature.metric = "celsius")) %>%
  
    # Apparent Temperature Computation ----
    mutate(apparent_temperature = -2.7 + (1.04 * temperature_avg) +
                                         (2 * vapure_pressure_avg) - 
                                         (0.65 * wind_speed_avg)) %>%

    mutate(diff_avgat_vs_avgt = apparent_temperature - temperature_avg) %>% 
    mutate_if(is.double, ~ round(., 1))

  ## join summary tables with stations ----
  stations_daily_summaries_imputed_geo <- qc_daily_summaries_imputed_heat_index %>%
    merge(.,
          noaa_isd_2021_sf_ca_state_100km_simplified,
          by.x="station_id",
          by.y="station_id",
          all.x = TRUE,
          suffix = c("","_sp")) %>%
    mutate(YYYY_MM = format(as.Date(YYYY_MM_DD), "%Y-%m")) %>%
    mutate(month_name = lubridate::month(format(as.Date(YYYY_MM_DD), "%Y-%m-%d"), label = TRUE))

  stations_annual_summaries_geo <- qc_annual_summaries %>%
    merge(.,
          noaa_isd_2021_sf_ca_state_100km_simplified,
          by.x="station_id",
          by.y="station_id",
          all.x = TRUE,
          suffix = c("","_web_api"))

  # Selected variables -----
  selected_variables <- c(
    "temperature_avg",
    "temperature_dewpoint_avg",
    "wind_speed_avg")


  qc_daily_summaries <- qc_daily_summaries_imputed_heat_index %>%
    group_by(YYYY, station_id) %>%
    summarise(
      n_records = n(),
      n_distinct_days = n_distinct(YYYY_MM_DD),
      across(all_of(selected_variables),
             list(
               null = ~sum(is.na(.)),
               inf = ~sum(is.infinite(.)),
               min = ~round(min(., na.rm = T), 1),
               max = ~round(max(., na.rm = T), 1)
             )
      ),
      .groups = 'drop')


  saveRDS(stations_annual_summaries_geo, file=file.path(output_path,
                                                 paste0("stations_annual_summaries_geo_",
                                                        year, "_",
                                                        threshold_label,
                                                        ".rds")))

  saveRDS(stations_daily_summaries_imputed_geo, file=file.path(output_path,
                                                  paste0("stations_daily_summaries_imputed_geo_",
                                                         year, "_",
                                                         threshold_label,
                                                         ".rds")))
gc()

}

print(showConnections())
print("Closing")
print(stopCluster(cl))
print(rm(cl))

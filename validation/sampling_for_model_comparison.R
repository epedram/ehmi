# Validation based on random Sampling of 20% validation set  ----
fx_fieldsView(noaa_points_sf %>% st_drop_geometry(),
              output_path = rds_output_path,
              "00",
              "noaa_points_sf")

fx_fieldsView(noaa_stations_climate_dt %>% st_drop_geometry(),
              output_path = rds_output_path,
              "02",
              "noaa_stations_climate_dt")

fx_fieldsView(EHE_ECE_compiled,
              output_path = rds_output_path,
              "03",
              "EHE_ECE_compiled")

length(unique(noaa_stations_climate_dt$shape_id))
length(unique(noaa_stations_climate_dt$DATE))

noaa_stations_n <- length(unique(noaa_stations_climate_dt$shape_id))
noaa_stations_list <- as.list(unique(noaa_stations_climate_dt$shape_id))

noaa_points_basic_sf <- noaa_points_sf[c("geoid", "shape_id",
                                                    "Station_Name")]

fx_fieldsView(noaa_points_basic_sf %>% st_drop_geometry(),
              output_path = rds_output_path,
              "01",
              "noaa_points_basic_sf")

fx_saveSHP(noaa_points_basic_sf, folder = rds_output_path,
           shpname = "noaa_points_basic_sf",
           subfolder = "noaa_stations_geo")

# All voronoi for comparison
all_stations_voronoi_dtsf <- fx_VoronoiPolygons_fromSF(noaa_points_basic_sf)

ca_voronoi_all_stations_dtsf <- st_intersection(st_cast(all_stations_voronoi_dtsf),
                                                st_union(CA_counties_sf))

fx_saveSHP(ca_voronoi_all_stations_dtsf, folder = rds_output_path,
           shpname = "ca_all_stations_voronoi",
           prefix = "",
           subfolder = "ca_all_stations_voronoi")

## Sampling ----
library(doParallel)
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)
print(cl)

foreach(counter=1:iteration_n, .verbose = TRUE) %dopar% {
  source(here::here("R", "libraries.R"), local=T)
  source(here::here("R", "helper_functions.R"), local=T)
  source(here::here("validation", "runtime_conf.R"), local=T)

  theme_set(new)

#for(counter in 1:iteration_n){
  dir.create(file.path(output_path,
                       paste0("sample_", counter)))
  sample_output_path <- file.path(output_path,
                        paste0("sample_", counter))

  dir.create(file.path(sample_output_path,
                       "Viz_Nearest_Station"))
  vis_output_path <- file.path(sample_output_path,
                               "Viz_Nearest_Station")
### sample size definition ----
smaple_size <- noaa_stations_n * sampling_ratio

sampled_noaa_stations_80 <- sample(noaa_stations_list, smaple_size)
glimpse(noaa_points_sf)
### extract dataframes of sampled/excluded dataframes accordingly ----
test_stations_points_dtsf <- noaa_points_basic_sf %>% filter(.,
                                                             shape_id %in% sampled_noaa_stations_80)

validation_stations_points_dtsf <- noaa_points_basic_sf %>% filter(.,
                                                                   !shape_id %in% sampled_noaa_stations_80)

validation_stations_points_sp <- as_Spatial(validation_stations_points_dtsf,
                                cast = TRUE)

validation_stations_points_df <- data.frame(validation_stations_points_sp)

test_stations_climate_dtsf <- noaa_stations_climate_dt %>% filter(.,
                              shape_id %in% sampled_noaa_stations_80)

validation_stations_climate_dtsf <- noaa_stations_climate_dt %>% filter(.,
                                    !shape_id %in% sampled_noaa_stations_80)

length(unique(test_stations_climate_dtsf$Station_Name))
length(unique(test_stations_climate_dtsf$DATE))

glimpse(noaa_stations_climate_dt)
plot(test_stations_points_dtsf[1])
plot(validation_stations_points_dtsf[1])

# Create Voronoi polygons ----
voronoi_test_stations_points_dtsf <- fx_VoronoiPolygons_fromSF(test_stations_points_dtsf)
glimpse(voronoi_test_stations_points_dtsf)
plot(voronoi_test_stations_points_dtsf[1])

ca_test_set_voronoi_dtsf <- st_intersection(st_cast(voronoi_test_stations_points_dtsf),
                                                 st_union(CA_counties_sf))

glimpse(ca_test_set_voronoi_dtsf)
plot(ca_test_set_voronoi_dtsf[1])

# Join the nearest stations records
validation_set_interim <- st_join(validation_stations_climate_dtsf,
                                  ca_test_set_voronoi_dtsf["shape_id"],
                             join = st_within, suffix = c("","_Nearest_Station"), left = TRUE)

validation_set_climate_data <- merge(validation_set_interim,
                           noaa_stations_climate_dt %>% st_drop_geometry(),
                           all.x = TRUE,
                           by.x=c("DATE","shape_id_Nearest_Station"), # key columns to join to the data table
                           by.y=c("DATE","shape_id"), # original table key columns
                           suffix = c("","_Nearest"))

fx_saveCSV(validation_stations_points_dtsf,
           sample_output_path,
           prefix = paste0("sample_", counter, "_"),
           suffix = "")

fx_fieldsView(validation_stations_climate_dtsf %>% st_drop_geometry(),
              output_path = rds_output_path,
              paste0("04_sample_", counter),
              "validation_stations_climate_dtsf")

fx_saveSHP(ca_test_set_voronoi_dtsf, folder = sample_output_path,
           shpname = "ca_test_set_voronoi",
           prefix = "", suffix = paste0("_sample_", counter),
           subfolder = "ca_test_set_voronoi"
)

fx_saveSHP(validation_stations_points_dtsf, folder = sample_output_path,
           prefix = "",
           shpname = paste0("validation_set_stations_sample_", counter),
           subfolder = paste0("validation_set_sample_", counter)
)

validation_set_climate_data_diff <- validation_set_climate_data %>%
  mutate(Delta_AT_nearest =
           round(dm_apparent_temperature - dm_apparent_temperature_Nearest, 1))

# Apply spatial models over the selected samples

# Estimate interpolated surface ----

selected_day <- start_date
W <- as_Spatial(CA_counties_sf,
                cast = TRUE)

# Loop ----
## Create empty DF to collect and compile daily outputs ----
CA_blocks_joined_EHE <- NULL
CA_AT_Boundaries <- NULL
CA_AT_Vectorized_Grid <- NULL
CA_AT_Contours <- NULL
idw_estimates_collector <- NULL
i <- 1
loop_length <- time_length(end_date - start_date + 1, unit = "day")

while (selected_day <= end_date)
{
  #if (selected_day %in% AT_DATES[[2]])
  #{
  # Convert rasters to dataframes
  # Convert sf_points to dataframes with explicit coordinates

  test_stations_climate_daily <- test_stations_climate_dtsf[c("DATE", "shape_id",
                                                              "dm_apparent_temperature")] %>%
    filter(!is.na(dm_apparent_temperature)) %>%
    filter(DATE == selected_day)

  test_stations_sp <- as_Spatial(test_stations_climate_daily,
                                 cast = TRUE)

  test_stations_df <- data.frame(test_stations_sp)

  # Create an empty DF for parsing daily estimates from idw
  idw_estimates_collector_day <- validation_stations_points_dtsf["shape_id"] %>% st_drop_geometry()
  idw_estimates_collector_day$DATE <- selected_day

  # Create a voronoi layer to store and visualize estimates from the nearest station
  ca_test_set_voronoi_compiled_dtsf <-
    merge(ca_test_set_voronoi_dtsf,
          test_stations_climate_daily %>% st_drop_geometry(),
          all.x = TRUE,
          by.x=c("shape_id"), # key columns to join to the data table
          by.y=c("shape_id"), # original table key columns
          suffix = c("","_Nearest"))

  print(selected_day)
  print("interpolating temperature surface")

  # Replace point boundary extent with that of the state
  test_stations_sp@bbox <- W@bbox
  length(unique(test_stations_sp@data$DATE))
  length(unique(test_stations_sp@data$dm_apparent_temperature))
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(test_stations_sp, "regular",
                                             n = number_of_grid_cells))

  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object

  # Add P's projection information to the empty grid
  proj4string(test_stations_sp) <- proj4string(test_stations_sp)
  proj4string(grd) <- proj4string(test_stations_sp)

  #Interpolate the grid cells using a power value of 2 (idp=2.0) ----
  interpolated_idw <- gstat::idw(dm_apparent_temperature ~ 1,
                                 test_stations_sp,
                                 nmax=5,#
                                 newdata=grd,
                                 idp = idw)

  # Convert the surface into raster object and clip it to the Window of Analysis
  interpolated_idw_raster   <-   raster(interpolated_idw)

  interpolated_idw_raster_masked   <-   mask(interpolated_idw_raster, W)

  ## Convert raster into SP and SF objects for visualization ----
  interpolated_idw_raster_masked_sp <- as(interpolated_idw_raster_masked,'SpatialPolygonsDataFrame')
  interpolated_idw_raster_masked_sf <- st_as_sf(interpolated_idw_raster_masked_sp,
                                                crs = spatial_projection) %>%
    rename_at(vars(contains("var1.pred")), ~"Estimated_Level") %>%
    mutate(DATE = as.Date(selected_day, "%a, %d %b %Y")) %>%
    mutate(Estimated_Level = round(Estimated_Level, 2))

  st_crs(interpolated_idw_raster_masked_sf) <- spatial_projection
  print(st_crs(interpolated_idw_raster_masked_sf)$epsg)


  ## Compile the selected day DF ----
  AT_Vectorized_Grid_daily <- interpolated_idw_raster_masked_sf %>%
    #filter(Estimated_Level > AT_threshold) %>%
    mutate(computed_area = round(as.numeric(st_area(.)), 1)) %>%
    mutate(area_hectare = round((computed_area / 10000), 2)) %>%
    mutate(DATE = selected_day)


  AT_Boundaries_daily <- AT_Vectorized_Grid_daily %>%
    st_union(.) %>% as_tibble() %>% st_as_sf() %>%
    #mutate(event_type = "EHE") %>%
    mutate(Estimated_Level = 1) %>%
    mutate(computed_area = round(as.numeric(st_area(.)), 1)) %>%
    mutate(area_hectare = round((computed_area / 10000), 2)) %>%
    mutate(DATE = selected_day)


  blocks_joined_AT_daily <- st_join(
    CA_blocks_compiled,
    AT_Boundaries_daily,

    join = st_intersects,
    suffix = c("", "_GIS"),
    left = FALSE) %>%
    mutate(DATE = as.Date(selected_day, "%a, %d %b %Y")) %>%
    st_as_sf() %>% as_tibble() %>% st_as_sf()


  # Resolve the issue with the spatially too small contours
  tryCatch({
    r2c <- rasterToContour(interpolated_idw_raster_masked, nlevels = 10)

  }, error = function(e){
    print(selected_day)
    r2c <<- r2c[0,]

    cat("ERROR :",conditionMessage(e), "\n")
    print(r2c)
    return(r2c)
  })

  AT_Contours_daily <- st_as_sf(r2c,
                                crs = spatial_projection) %>%
    mutate(DATE = selected_day)

  st_crs(AT_Contours_daily) <- spatial_projection
  #print(st_crs(AT_Contours_daily)$epsg)


  ## Visualization of the estimated AT based on the nearest station ----
  nearest_AT_plot <- ggplot() +
    geom_sf(data = ca_test_set_voronoi_compiled_dtsf,
            mapping = aes(fill = dm_apparent_temperature),
            colour = NA) +
    scale_fill_gradientn(colors = c("white",
                                    "lightblue",
                                    "yellow",
                                    "red", "brown")) +

    geom_sf(data = CA_counties_sf,
            fill = NA,
            color = "Black",
            lwd = .5
    ) +

    geom_point(data = validation_stations_points_df,
               color = "Blue", size = 2, shape = 22,
               aes(x=coords.x1, y=coords.x2)
    ) +

    geom_sf(data = test_stations_climate_dtsf,
            color = "Black", size = 1.5) +
    #scale_shape_manual(values = 23) +

    labs(title = paste0("California State (estimated temperature based on the nearest ",
                        #nrow(test_stations_df),
                        " test set stations)"
    )) +
    labs(subtitle = paste0(
      "Date: ",
      selected_day))

  plot_file_name <- paste0(vis_output_path, "/",
                           "Estimated_AT",
                           "_", #counter, "_",
                           selected_day,
                           ".jpg")
  ggsave(plot_file_name,
         plot = nearest_AT_plot,
         dpi = 300,
         width = 16, height = 22, units = "cm")


  ### Compile the stacked iteration outputs ----
  #### Vector format
  CA_AT_Vectorized_Grid <- rbind(CA_AT_Vectorized_Grid,
                                 AT_Vectorized_Grid_daily)

  CA_AT_Contours <- rbind(CA_AT_Contours,
                          AT_Contours_daily)

  CA_blocks_joined_EHE <- rbind(CA_blocks_joined_EHE,
                                blocks_joined_AT_daily)


  #### Raster format
  rasValue = round(extract(interpolated_idw_raster_masked,
                           validation_stations_points_dtsf), 2)

  idw_estimates_collector_day$dm_apparent_temperature_IDW <- rasValue
  idw_estimates_collector <- rbind(idw_estimates_collector,
                                   idw_estimates_collector_day)

  #### Index stacked Raster based on the reformatted date string
  raster_index <- paste0("X", format(selected_day, "%Y%m%d"))
  names(interpolated_idw_raster_masked) <- raster_index

  if (i < 2) { # apply to the initial round only
    stacked_interpolated_idw <- interpolated_idw_raster_masked
    cumulative_interpolated_idw <- interpolated_idw_raster_masked

    print(i)
  } else { # for the second round and afterwards
    print(i)

    stacked_interpolated_idw <- stack(stacked_interpolated_idw,
                                      projectRaster(
                                        interpolated_idw_raster_masked,
                                        stacked_interpolated_idw,
                                        #method = 'bilinear'
                                        method = 'ngb'
                                      ))

    cumulative_interpolated_idw <- cumulative_interpolated_idw +
      projectRaster(
        interpolated_idw_raster_masked,
        stacked_interpolated_idw,
        #method = 'bilinear'
        method = 'ngb'
      )
  }
  i <- i + 1
  print("stacking interpolated surface")
  print(selected_day)

  cat(paste0(scales::percent(i/loop_length),
             " Completed", "\n\n"))

  selected_day <- selected_day + 1

} #

### IDW Error estimation ------
validation_set_climate_data_diff_2_interim <- merge(validation_set_climate_data_diff,
                                                    idw_estimates_collector,
                                                    all.x = TRUE,
                                                    by.x=c("DATE","shape_id"), # key columns to join to the data table
                                                    by.y=c("DATE","shape_id"), # original table key columns
                                                    suffix = c("","_IDW"))

validation_set_climate_data_diff_2 <- validation_set_climate_data_diff_2_interim %>%
  mutate(Delta_AT_idw =
           round(dm_apparent_temperature - dm_apparent_temperature_IDW, 1))

validation_results <- validation_set_climate_data_diff_2 %>%
  dplyr::select(!starts_with("EH") &
                  !starts_with("EC") &
                  !starts_with("x")
  ) %>%
  mutate(Sample_ID = paste0("sample_", counter)) %>%
  st_drop_geometry()

fx_saveCSV(validation_results, sample_output_path,
           prefix = paste0("sample_", counter, "_"),
           suffix = "")

fx_saveCSV(validation_results, rds_output_path,
           prefix = paste0("sample_", counter, "_"),
           suffix = "")

fx_appendCSV(validation_results, output_path)

fx_fieldsView(validation_results,
              output_path = sample_output_path,
              "09",
              "validation_results")

ObjSave(
  CA_AT_Contours,
  validation_set_climate_data_diff_2,
  validation_results,

  folder = sample_output_path)

###  Visualize the cumulative raster ----
# compute the average interpolated surface
cumulative_interpolated_idw <- cumulative_interpolated_idw / loop_length

class(cumulative_interpolated_idw)
nlayers(cumulative_interpolated_idw)
dim(cumulative_interpolated_idw)
##```

##```{r}
str(cumulative_interpolated_idw@data)
#glimpse(cumulative_interpolated_idw@data)

# cumulative_interpolated_idw <- readRDS(here(input_dir,
#                                  "ehe_cumulative_interpolated_idw.rds"))[[1]]
length(cumulative_interpolated_idw[cumulative_interpolated_idw])
length(cumulative_interpolated_idw[cumulative_interpolated_idw > 1])
length(cumulative_interpolated_idw[cumulative_interpolated_idw < 1])
length(cumulative_interpolated_idw[cumulative_interpolated_idw == 1])
cumulative_AT_coverage <- 100 * round(
  (length(cumulative_interpolated_idw[cumulative_interpolated_idw > 1]) / length(cumulative_interpolated_idw[cumulative_interpolated_idw]))
  ,4)

plot_file_name <- paste0(sample_output_path, "/_AT_",
                         "00_cumulative_interpolated_idw_",
                         cumulative_AT_coverage,
                         "_percent.jpg")

jpeg(plot_file_name, width = 880, height = 1200)

print(plot(cumulative_interpolated_idw))

dev.off()

class(cumulative_interpolated_idw)
plot_file_name <- paste0(sample_output_path, "/_AT_",
                         "01_cumulative_interpolated_idw_",
                         cumulative_AT_coverage,
                         "_percent.jpg")

jpeg(plot_file_name, width = 880, height = 1200)

print(spplot(cumulative_interpolated_idw))

dev.off()
##```

### Export spatial objects ----
writeRaster(cumulative_interpolated_idw, paste0(sample_output_path, "/_AT_",
                                                "01_cumulative_interpolated_idw"), overwrite=TRUE)

writeRaster(stacked_interpolated_idw, paste0(sample_output_path, "/_AT_",
                                             "02_stacked_interpolated_idw"), overwrite=TRUE)

stacked_interpolated_idw_rb <- brick(stacked_interpolated_idw)
writeRaster(stacked_interpolated_idw_rb, paste0(sample_output_path, "/_AT_",
                                                "03_stacked_interpolated_idw_rb"), overwrite=TRUE)

ObjSave(
  cumulative_interpolated_idw,
  stacked_interpolated_idw,
  stacked_interpolated_idw_rb,

  folder = sample_output_path)


source(here::here("validation", "idw_model_visualization.R"), local=T)

#} # CLOSE SAMPLING LOOP (serial)
} # CLOSE SAMPLING LOOP (parallel)

### closing parallel computing
print(showConnections())

print("Closing")
print(stopCluster(cl))
print(rm(cl))
gc()

print("Check")
print(showConnections())


# title: "Spatiotemporal Delineation of Extreme Heat/Cold Events"
## subtitle: "Estimation of the Impacted Population"

## Model parameters
##```{r}
ehe_threshold <- 0.5
ece_threshold <- -0.5
idw <-  2
number_of_grid_cells <-  1020100 #~600m
spatial_projection <-  4269 #NAD83
join_type <-  "st_intersects"
start_date <-  as.Date("2019-01-01", format="%Y-%m-%d")
end_date <-  as.Date("2019-12-31", format="%Y-%m-%d")

##```


## I/O
##```{r set IO parameters, include=FALSE}
library(here)

project_name <- "EHE_ECE_Impacted_Population_"

input_dir <- "data"

output_dir <- (here("outputs","/"))

timestamped <- format(Sys.time(), "%m%d_%H%M")

print(Sys.time)
print(timestamped)

timestamped_folder <- paste0(project_name ,timestamped)

dir.create(file.path(here("outputs"), timestamped_folder))

output_path <- paste0(output_dir, timestamped_folder)

print(output_dir)
print(output_path)

dir.create(file.path(output_path, "Method_1_Nearest_Station"))
dir.create(file.path(output_path, "Method_2_Contour_Overlay"))
dir.create(file.path(output_path, "Methods_Comparison"))

M1_output_path <- file.path(output_path, "Method_1_Nearest_Station")
M2_output_path <- file.path(output_path, "Method_2_Contour_Overlay")
MC_output_path <- file.path(output_path, "Methods_Comparison")
##```


## Libraries and functions
##```{r libs}
source(here("R", "libraries.R"))

source(here("R", "helper_functions.R"))

writeLines(capture.output(sessionInfo()),
           paste0(output_path, "/_",
                  timestamped, "_session_lib_info.txt"))
##```


## Set the graphical theme
##```{r}
new <-  theme_classic() + theme(
        axis.text.x=element_text(angle=45 ,hjust=1),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        legend.box = "vertical",
        legend.key.height = unit(1.2, "mm"),
        legend.position="none"
        #strip.text.x = element_blank(),
        #strip.text.y = element_blank()
        )

theme_set(new)
##```


# Load the input data
##```{r}
noaa_compiled_sf_selected <- readRDS(here(input_dir,
                                  "noaa_compiled_sf_selected.rds"))[[1]]

CA_voronoi_polygons_sf <- readRDS(here(input_dir,
                                 "CA_voronoi_polygons_sf.rds"))[[1]]

SCA_voronoi_polygons_sf <- readRDS(here(input_dir,
                                 "SCA_voronoi_polygons_sf.rds"))[[1]]

CA_fixed_grid_sf <- readRDS(here(input_dir,
                                  "CA_fixed_grid_sf.rds"))[[1]]

CA_counties_sf <- readRDS(here(input_dir,
                                 "CA_counties_sf_ACS2019.rds"))[[1]]

CA_tracts_sf <- readRDS(here(input_dir,
                                 "CA_tracts_sf_ACS2019.rds"))[[1]]

CA_blocks_sf <- readRDS(here(input_dir,
                                 "CA_blocks_sf_ACS2019.rds"))[[1]]

CA_zipcodes_sf <- readRDS(here(input_dir,
                                  "CA_zipcodes_sf_ACS2019.rds"))[[1]]

EHE <- readRDS(here(input_dir,
                                 "EHE.rds"))[[1]]

ECE <- readRDS(here(input_dir,
                                 "ECE.rds"))[[1]]

EHE_ECE_compiled <- readRDS(here(input_dir,
                                 "EHE_ECE_compiled.rds"))[[1]]
##```


# Set the spatial boundaries (for specific case studies)
##```{r}
SCA_counties_sf_name <- c("San Luis Obispo", "Kern",
                       "Ventura", "Los Angeles", "Santa Barbara",
                       "Orange", "Riverside",
                       "San Diego", "Imperial", "San Bernardino"
                       )

LA_county_name <- c("Los Angeles")


SCA_counties_sf <- CA_counties_sf %>%
  filter(NAME %in% SCA_counties_sf_name)

SCA_tracts_sf <- CA_tracts_sf %>%
  filter(NAME %in% SCA_counties_sf_name)

SCA_blocks_sf <- CA_blocks_sf %>%
  filter(NAME %in% SCA_counties_sf_name)


xlim_sca = st_bbox(SCA_counties_sf)[c(1,3)]
ylim_sca = st_bbox(SCA_counties_sf)[c(2,4)]


#xlim_la = st_bbox(LA_county_sf)[c(1,3)]
#ylim_la = c(33.6, 34.8)
##```


# Join the census blocks with voronoi polygons
##```{r}
sp_points <- as_Spatial(noaa_compiled_sf_selected[c("shape_id", "NAME")],
                        cast = TRUE)


CA_blocks_joined_stations <- st_join(CA_blocks_sf,
                             CA_voronoi_polygons_sf,
                             join = join_type,
                             left = TRUE,
                             largest = TRUE) %>%
                          st_as_sf() %>% as_tibble() %>% st_as_sf()


CA_blocks_compiled <- CA_blocks_joined_stations %>%
                              dplyr::select(
                              !starts_with("popM_block") &
                              !starts_with("NAME_block")
                              ) %>%
                              filter(!is.na(shape_id))
##```


# Setup interpolation parameters
##```{r}
#start_date <- as.Date("2019-08-27",format="%Y-%m-%d")
#end_date   <- as.Date("2019-08-29",format="%Y-%m-%d")

start_date <- as.Date("2019-06-01",format="%Y-%m-%d")
end_date   <- as.Date("2019-11-01",format="%Y-%m-%d")

EHE_ECE_compiled_null2zero <- EHE_ECE_compiled %>%
  mutate_at(vars(ECE_duration, EHE_duration,
                 EHF, EHMI,
                 ECF, ECMI,
                 ), ~replace_na(., 0))

EHE_ECE_compiled_sf <- st_as_sf(EHE_ECE_compiled_null2zero,
                                   coords = c("longitude", "latitude"),
                                   crs = spatial_projection) %>% as_tibble() %>% st_as_sf()


EHE_selected_period_geo <- EHE_ECE_compiled_sf %>%
                filter(DATE >= start_date & DATE <= end_date) %>%
                filter(EHE_duration >= 1)

EHE_DATES <- EHE_selected_period_geo[c("EHE_duration", "DATE")] %>%
  filter(EHE_duration >= 1)

CA_blocks_compiled_EHE <- merge(CA_blocks_compiled,
                                     EHE_selected_period_geo %>% st_drop_geometry(),
                                     by.x=c("shape_id"),
                                     by.y=c("shape_id"),
                                     all.x = TRUE) %>%
                      st_as_sf() %>% as_tibble() %>% st_as_sf()
##```

# Compute statistical summaries
## Estimate impacted popualtion based on the nearest station to census blocks
##```{r}
CA_impacted_stations <- EHE_ECE_compiled_sf %>%
  filter(!is.na(DATE)) %>%
  filter(EHE_duration >= 1) %>% st_drop_geometry() %>%
  group_by(
           DATE) %>%
  summarise(
    impacted_stations_n = n())

CA_impacted_stations

########################

CA_blocks_sum <- CA_blocks_joined_stations %>% st_drop_geometry() %>%
  summarise(
    blocks_n = n(),
    blocks_population = sum(population_block))

pop_sum <- CA_blocks_sum[[2]]
pop_sum


CA_impactedd_blocks_population <- CA_blocks_compiled_EHE %>% st_drop_geometry() %>%
  filter(!is.na(DATE)) %>%
  group_by(
           DATE) %>%
  summarise(
    impacted_blocks_n = n(),
    impacted_blocks_population = sum(population_block)) %>%
  mutate(percent_impacted = round(impacted_blocks_population / pop_sum, 3)) %>%
  mutate(percent_impacted_txt = paste0(sprintf("%.1f", percent_impacted * 100), "%"))

CA_impactedd_blocks_population

fx_saveCSV(CA_impactedd_blocks_population, output_path,
             prefix = "11_", suffix = "nearest_station_daily_")
##```


### Visualize the impacted census blocks (Method 1)
##```{r}
selected_day <- start_date

while (selected_day <= end_date)
{
if (selected_day %in% EHE_DATES[[2]])
  {

  CA_blocks_compiled_EHE_day <- CA_blocks_compiled_EHE %>%
                   filter(DATE == selected_day)

  Census_Blocks_Nearest_Stations <-  ggplot() +

     geom_sf(data = CA_blocks_compiled_EHE_day,
             fill = "darkorange", #bisque
             colour = NA,
             alpha = 0.8
             ) +

    geom_sf(data = CA_counties_sf,
               fill = NA,
               color = "Black",
               lwd = .6
               ) +

    geom_point(data = EHE,
                   aes(x=Longitude,
                       y=Latitude
                       ), color = "Black") +

        labs(title = paste0("California State (impacted blocks based on the nearest station)")) +
    labs(subtitle = paste0(
                           "Extreme Heat Event Date: ",
                           selected_day))

  Census_Blocks_Nearest_Stations

  plot_file_name <- paste0(M1_output_path, "/",
                           substitute(Census_Blocks_Nearest_Stations),
                           "_",
                           selected_day,
                           ".jpg")
  ggsave(plot_file_name,
         plot = Census_Blocks_Nearest_Stations,
         dpi = 300,
         width = 16, height = 22, units = "cm")

  plot_file_name <- paste0(M1_output_path, "/",
                           substitute(Census_Blocks_Nearest_Stations),
                           "_voronoi_",
                           selected_day,
                           ".jpg")
  ggsave(plot_file_name,
         plot = Census_Blocks_Nearest_Stations +
    geom_sf(data = CA_voronoi_polygons_sf,
               fill = NA,
               color = "Brown",
               lwd = .5
               ),
         dpi = 300,
         width = 16, height = 22, units = "cm")

    selected_day <- selected_day + 1
  } else {
  selected_day <- selected_day + 1
  }
}
##```


## Estimate impacted popualtion based on the interpolated surface
### Visualize the impacted census blocks (Method 2)
##```{r}
colorfill <- "gold"
#             "bisque"
#             "cornsilk3"

selected_day <- start_date
W <- as_Spatial(CA_counties_sf,
                        cast = TRUE)


# Create empty dataframes to compile daily outputs
CA_blocks_joined_EHE <- NULL
CA_EHE_Boundaries <- NULL
CA_EHE_Vectorized_Grid <- NULL
CA_EHE_Contours <- NULL


while (selected_day <= end_date)
{
if (selected_day %in% EHE_DATES[[2]])
  {
  P <- as_Spatial(EHE_ECE_compiled_sf %>%
                  #filter(!is.na(EHE_duration)) %>%
                  filter(DATE == selected_day),
                          cast = TRUE)

  # Replace point boundary extent with that of the state
  P@bbox <- W@bbox

  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(P, "regular",
                                             n = number_of_grid_cells))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object

  # Add P's projection information to the empty grid
  proj4string(grd) <- proj4string(P)

  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  interpolated_idw <- gstat::idw(EHE_duration ~ 1, P, nmax=5,#
                      newdata=grd,
                      idp = idw)

  # Convert the surface into raster object and clip it to the Window of Analysis
  r       <- raster(interpolated_idw)
  rm     <- mask(r, W)

  ## Compute and plot polygons
  SP <- rasterToPolygons(clump(r > 2), dissolve = TRUE)

  rm_sp <- as(rm,'SpatialPolygonsDataFrame')
  rm_sf <- st_as_sf(rm_sp,
                    crs = spatial_projection) %>%
    rename_at(vars(contains("var1.pred")), ~"Estimated_Level") %>%
    mutate(DATE = as.Date(selected_day, "%a, %d %b %Y")) %>%
    mutate(Estimated_Level = round(Estimated_Level, 2))

  st_crs(rm_sf) <- spatial_projection
  print(st_crs(rm_sf)$epsg)

  EHE_Vectorized_Grid_daily <- rm_sf %>% filter(Estimated_Level > ehe_threshold) %>%
    mutate(computed_area = round(as.numeric(st_area(.)), 2)) %>%
    mutate(area_hectare = round((computed_area / 10000), 2))


  EHE_Boundaries_daily <- EHE_Vectorized_Grid_daily %>%
    st_union(.) %>% as_tibble() %>% st_as_sf() %>%
    mutate(event_type = "EHE") %>%
    mutate(Estimated_Level = 1) %>%
    mutate(computed_area = round(as.numeric(st_area(.)), 2)) %>%
    mutate(area_hectare = round((computed_area / 10000), 2))


  blocks_joined_EHE_daily <- st_join(
                          CA_blocks_compiled,
                          EHE_Boundaries_daily,

                         join = st_intersects,
                         suffix = c("", "_GIS"),
                         left = FALSE) %>%
                         mutate(DATE = as.Date(selected_day, "%a, %d %b %Y")) %>%
                         st_as_sf() %>% as_tibble() %>% st_as_sf()


  # Resolve the issue with the spatially too small contours
  tryCatch({
      r2c <- rasterToContour(rm)

    }, error=function(e){
    print(selected_day)
    r2c <<- r2c[0,]

    cat("ERROR :",conditionMessage(e), "\n")
    print(r2c)
    return(r2c)
    })

  EHE_Contours_daily <- st_as_sf(r2c,
                crs = spatial_projection)

  ## Visualization
  interpolated_EHE_plot <-  ggplot() +
     geom_sf(data = rm_sf,
             mapping = aes(fill = Estimated_Level),
             colour = NA) +
     scale_fill_gradientn(colors = c("white",
                                     "lightblue",
                                     "yellow",
                                     "red", "brown")) +

    geom_sf(data = CA_counties_sf,
               fill = NA,
               color = "Black",
               lwd = .6
               ) +

    geom_sf(data = EHE_Contours_daily,
              color = "Black",
              lwd = .5
               ) +

     geom_point(data = EHE,
                   aes(x=Longitude,
                       y=Latitude
                       ), color = "Black") +

    labs(title = paste0("California State (interpolated surface)")) +
    labs(subtitle = paste0(
                           "Extreme Heat Event Date: ",
                           selected_day))

  plot_file_name <- paste0(M2_output_path, "/",
                           "interpolated_EHE_contour_overlay",
                           "_",
                           selected_day,
                           ".jpg")
  ggsave(plot_file_name,
         plot = interpolated_EHE_plot,
         dpi = 300,
         width = 16, height = 22, units = "cm")


  Census_Blocks_contour_overlay <-  ggplot() +

     geom_sf(data = blocks_joined_EHE_daily,
             fill = colorfill,
             colour = NA
             ) +

    geom_sf(data = CA_counties_sf,
               fill = NA,
               color = "Black",
               lwd = .6
               ) +

    geom_sf(data = EHE_Contours_daily,

              color = "Brown",
              lwd = .5
               ) +

      geom_point(data = EHE,
                   aes(x=Longitude,
                       y=Latitude
                       ), color = "Black") +

      labs(title = paste0("California State (impacted blocks based on the overlaid contour)")) +
    labs(subtitle = paste0(
                           "Extreme Heat Event Date: ",
                           selected_day))

  plot_file_name <- paste0(M2_output_path, "/",
                           substitute(Census_Blocks_contour_overlay),
                           "_",
                           selected_day,
                           ".jpg")
  ggsave(plot_file_name,
         plot = Census_Blocks_contour_overlay,
         dpi = 300,
         width = 16, height = 22, units = "cm")


  # Compile the iteration outputs
    CA_EHE_Boundaries <- rbind(CA_EHE_Boundaries,
                               EHE_Boundaries_daily)

    CA_EHE_Vectorized_Grid <- rbind(CA_EHE_Vectorized_Grid,
                                    EHE_Vectorized_Grid_daily)

    CA_EHE_Contours <- rbind(CA_EHE_Contours,
                             EHE_Contours_daily)

    CA_blocks_joined_EHE <- rbind(CA_blocks_joined_EHE,
                                  blocks_joined_EHE_daily)

    selected_day <- selected_day + 1
  } else {
  selected_day <- selected_day + 1
  }
}

ObjSave(
        CA_EHE_Boundaries,
        CA_EHE_Vectorized_Grid,
        CA_EHE_Contours,
        CA_blocks_joined_EHE,

        folder = output_path)
##```


# Methods comparison
## Compute statistical summaries
##```{r}
CA_impactedd_blocks_population_contour_overlay <- CA_blocks_joined_EHE %>%
  st_drop_geometry() %>%
  filter(!is.na(DATE)) %>%
  group_by(
    DATE) %>%
  summarise(
    impacted_blocks_n = n(),
    impacted_blocks_population = sum(population_block)) %>%
  mutate(percent_impacted = round(impacted_blocks_population / pop_sum, 3)) %>%
  mutate(percent_impacted_txt = paste0(sprintf("%.1f", percent_impacted * 100), "%"))

fx_saveCSV(CA_impactedd_blocks_population_contour_overlay, output_path,
           prefix = "21_", suffix = "daily_")


CA_blocks_joined_both <- merge(CA_blocks_joined_EHE[c("DATE", "GEOID_block", "Estimated_Level")],
                               CA_blocks_compiled_EHE[c("DATE", "GEOID_block", "shape_id",
                                                        "EHE_duration")] %>%
                                 st_drop_geometry(),
                               by.x=c("GEOID_block", "DATE"),
                               by.y=c("GEOID_block", "DATE"),
                               all = TRUE) %>%
  st_as_sf() %>% as_tibble() %>% st_as_sf() %>%
  mutate(Identified_Method =
           as.character(case_when(Estimated_Level > 0 &
                                    EHE_duration > 0
                                  ~ "Both",
                                  is.na(EHE_duration) &
                                    Estimated_Level > 0
                                  ~ "Interpolated Surface",
                                  TRUE ~ "Nearest Station")))


CA_blocks_joined_both_Sf <- merge(CA_blocks_compiled,
                                  CA_blocks_joined_both %>% st_drop_geometry(),
                                  by.x=c("GEOID_block"),
                                  by.y=c("GEOID_block"),
                                  all = TRUE) %>%
  st_as_sf() %>% as_tibble() %>% st_as_sf()

CA_impactedd_blocks_population_comparison_daily <- CA_blocks_joined_both_Sf %>%
  st_drop_geometry() %>%
  filter(!is.na(DATE)) %>%
  group_by(
    DATE, Identified_Method) %>%
  summarise(
    impacted_blocks_n = n(),
    impacted_blocks_population = sum(population_block)) %>%
  mutate(percent_impacted = round(impacted_blocks_population / pop_sum, 3)) %>%
  mutate(percent_impacted_txt = paste0(sprintf("%.1f", percent_impacted * 100), "%")) %>%
  mutate(ratio_to_total_impacted = impacted_blocks_population / sum(impacted_blocks_population)) %>%
  mutate(ratio_to_total_impacted_txt = paste0(sprintf("%.1f", ratio_to_total_impacted * 100), "%"))

CA_impactedd_blocks_population_comparison <- CA_blocks_joined_both_Sf %>%
  st_drop_geometry() %>%
  filter(!is.na(DATE)) %>%
  group_by(
    Identified_Method) %>%
  summarise(
    impacted_blocks_n = n(),
    impacted_blocks_population = sum(population_block)) %>%
  mutate(percent_impacted = round(impacted_blocks_population / pop_sum, 3)) %>%
  mutate(percent_impacted_txt = paste0(sprintf("%.1f", percent_impacted * 100), "%")) %>%
  mutate(ratio_to_total_impacted = impacted_blocks_population / sum(impacted_blocks_population)) %>%
  mutate(ratio_to_total_impacted_txt = paste0(sprintf("%.1f", ratio_to_total_impacted * 100), "%"))

fx_saveCSV(CA_impactedd_blocks_population_comparison_daily, output_path,
           prefix = "31_", suffix = "")

fx_saveCSV(CA_impactedd_blocks_population_comparison, output_path,
           prefix = "32_", suffix = "aggregated_")
##```

### Visualize the comparison of areas identified through either methods
##```{r}
selected_day <- start_date

while (selected_day <= end_date)
{
if (selected_day %in% EHE_DATES[[2]])
  {

  CA_blocks_joined_both_Sf_day <- CA_blocks_joined_both_Sf %>%
    filter(DATE == selected_day)

  Census_Blocks_Both <-  ggplot() +

    geom_sf(data = CA_blocks_joined_both_Sf_day,
            color = NA,
            aes(
              fill = Identified_Method,
              alpha = 0.8
            )) +
    scale_fill_manual(values = c("Nearest Station" = "#5CB85CFF",
                                 "Interpolated Surface" = "#EEA236FF",
                                 "Both" = "#D43F3AFF"
    ),
    name = "Event Identification Method:") +

    guides(alpha = FALSE) +
    theme(legend.direction = "vertical", legend.position = "bottom") +

    geom_sf(data = CA_counties_sf,
            fill = NA,
            color = "Black",
            lwd = .6
    ) +

    geom_point(data = EHE,
               aes(x=Longitude,
                   y=Latitude
               ), color = "Black") +

    labs(title = paste0("California State (comparison of identified blocks)")) +
    labs(subtitle = paste0(
      "Extreme Heat Event Date: ",
      selected_day))

  plot_file_name <- paste0(MC_output_path, "/",
                           "Census_Blocks_identified_by_both_methods",
                           "_",
                           selected_day,
                           ".jpg")
  ggsave(plot_file_name,
         plot = Census_Blocks_Both,
         dpi = 300,
         width = 16, height = 22, units = "cm")

  selected_day <- selected_day + 1
  } else {
  selected_day <- selected_day + 1
  }
}
##```

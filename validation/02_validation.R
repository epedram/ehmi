# title: "Spatiotemporal Delineation of Extreme Heat/Cold Events"
## subtitle: "Spatial Model Comparison"

## Model parameters ----
##```{r}
source(here::here("validation", "runtime_conf.R"), local=T)
##```

## I/O ----
##```{r set IO parameters, include=FALSE}
library(here)

project_name <- "Validation_"

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

# dir.create(file.path(output_path, "Method_1_Nearest_Station"))
# dir.create(file.path(output_path, "Method_2_Contour_Overlay"))
# dir.create(file.path(output_path, "Methods_Comparison"))
dir.create(file.path(output_path, "RDS_Archive"))

# M1_output_path <- file.path(output_path, "Method_1_Nearest_Station")
# M2_output_path <- file.path(output_path, "Method_2_Contour_Overlay")
# MC_output_path <- file.path(output_path, "Methods_Comparison")
rds_output_path <- file.path(output_path, "RDS_Archive")
##```


## Libraries and functions ----
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
        axis.text.x = element_text(angle=45 ,hjust=1),
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
# Load the input data ----
##```{r}
noaa_points_sf <- readRDS(here(input_dir,
                                  "noaa_compiled_sf_selected.rds"))[[1]] %>%
  rename_at("name", ~ "Station_Name")

noaa_stations_climate_dt <- readRDS(here(input_dir,
                                         "stations_daily_means_compiled_sf.rds"))[[1]] %>%
  filter(DATE >= start_date & DATE <= end_date) %>%
  filter(!is.na(dm_apparent_temperature))


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


# Set the spatial boundaries (for specific case studies) ----
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

# set zoom
xlim_sca = st_bbox(SCA_counties_sf)[c(1,3)]
ylim_sca = st_bbox(SCA_counties_sf)[c(2,4)]

#xlim_la = st_bbox(LA_county_sf)[c(1,3)]
#ylim_la = c(33.6, 34.8)
##```


# Join the census blocks with voronoi polygons ----
##```{r}
sp_points <- as_Spatial(noaa_points_sf[c("shape_id", "NAME")],
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

##```{r}
# Setup EHE/ECE base dataframe based on the study period----
EHE_ECE_compiled_null2zero <- EHE_ECE_compiled %>%
  mutate_at(vars(ECE_duration, EHE_duration,
                 EHF, EHMI,
                 ECF, ECMI,
                 ), ~replace_na(., 0))

### Convert EHE_ECE dataframe into a spatial layer ----
EHE_ECE_compiled_sf <- st_as_sf(EHE_ECE_compiled_null2zero,
                                   coords = c("longitude", "latitude"),
                                   crs = spatial_projection) %>% as_tibble() %>% st_as_sf()

st_crs(EHE_ECE_compiled_sf) <- spatial_projection
print(st_crs(EHE_ECE_compiled_sf)$epsg)
length(unique(EHE_ECE_compiled_sf$DATE))

EHE_selected_period_geo <- EHE_ECE_compiled_sf %>%
                filter(DATE >= start_date & DATE <= end_date) %>%
                filter(EHE_duration >= 1)

EHE_DATES <- EHE_selected_period_geo[c("EHE_duration", "DATE")] %>%
  filter(EHE_duration >= 1)
length(unique(EHE_DATES$DATE))

CA_blocks_compiled_EHE <- merge(CA_blocks_compiled,
                                     EHE_selected_period_geo %>% st_drop_geometry(),
                                     by.x=c("shape_id"),
                                     by.y=c("shape_id"),
                                     all.x = TRUE) %>%
                      st_as_sf() %>% as_tibble() %>% st_as_sf()
##```


# Compute statistical summaries ----
### Monthly boxplot ----
multipanel_boxplots <-
  ggplot(EHE_selected_period_geo %>% st_drop_geometry(),
         aes(x = EHE_duration,
             y = Month,
             group = Month)) +
  geom_boxplot()+
  guides(x =  guide_axis(angle = 0)) +

  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Events Duration",
       y = "Month") +
  labs(title = paste0("Monthly Distribution of Extreme Heat Events"
                      #"State of California ",
  )) +
  labs(subtitle = paste0("State of California ",
                         "2019"
  )) + theme_classic() +
  scale_x_continuous(
    breaks = (1:10))

#print(multipanel_boxplots)
plot_file_name <- paste0(output_path,
                         "/EHE_",
                         "Boxplot_by_",
                         "Month",
                         ".jpg")

ggsave(plot_file_name,
       plot = multipanel_boxplots,
       dpi = 300,
       width = 12, height = 12, units = "cm")

# Estimate impacted population ----
source(here::here("validation", "sampling_for_model_comparison.R"), local=T)

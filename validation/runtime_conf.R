number_of_grid_cells <-  1020100 #~600m
start_date <-  as.Date("2019-01-01", format="%Y-%m-%d")
end_date <-  as.Date("2019-12-31", format="%Y-%m-%d")

EHE_threshold <- 0.5
ECE_threshold <- -0.5
iteration_n <- 15
idw <-  2
sampling_ratio <-  0.8 #80% of stations with valid data records

spatial_projection <-  4269 #NAD83
dpi <- 300
join_type <-  "st_intersects"


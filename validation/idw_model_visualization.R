
  dir.create(file.path(sample_output_path,
                       "Viz_Interpolation"))
  vis_output_path <- file.path(sample_output_path,
                               "Viz_Interpolation")

  colorfill <- "gold"
  #             "bisque"
  #             "cornsilk3"

  nlayers(stacked_interpolated_idw)
  #names(stacked_interpolated_idw)

selected_day <- start_date
while (selected_day <= end_date)
{
  # Extract the raster layer from the stack and transform it into a data frame
  raster_index <- paste0("X", format(selected_day, "%Y%m%d"))
  selected_day_interpolated_idw <- gplot_data(stacked_interpolated_idw[[raster_index]])

  ## Visualization of the interpolated surface ----
  interpolated_AT_plot <- ggplot() +
  #https://gis.stackexchange.com/questions/377444/plotting-a-raster-stack-with-ggplot2
    geom_tile(data = selected_day_interpolated_idw %>% dplyr::filter(!is.na(value)),
              aes(x = x, y = y, fill = value)
    ) +
    #coord_equal() +
    #facet_wrap(~ variable) +
    #scale_fill_viridis() +
    #scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  #https://gis.stackexchange.com/questions/377444/plotting-a-raster-stack-with-ggplot2
    scale_fill_gradientn(colors = c("white",
                                      "lightblue",
                                      "yellow",
                                      "red", "brown")) +

    geom_sf(data = CA_counties_sf,
            fill = NA,
            color = "Black",
            lwd = .5
    ) +

    geom_sf(data = CA_AT_Contours %>% dplyr::filter(DATE == selected_day),
           color = "Gray30",
            lwd = .3) +

    geom_sf(data = test_stations_climate_dtsf,
            color = "Black", size = 1.5) +
    #scale_shape_manual(values = 23) +

    geom_point(data = validation_stations_points_df,
               color = "Blue", size = 2, shape = 22,
               aes(x=coords.x1, y=coords.x2)
    ) +

    labs(title = paste0("California State (interpolated temperature from ",
                        nrow(test_stations_df),
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
         plot = interpolated_AT_plot,
         dpi = 300,
         width = 16, height = 22, units = "cm")

  selected_day <- selected_day + 1
}


## ---- R helper functions
# functions


# f(x): Create a csv file presenting descriptive stats of a dataframe
## https://stackoverflow.com/questions/45176431/extract-name-of-data-frame-in-r-as-character
WriteStats <- function(df){
  stats_df <- skim(df) #%>% st_drop_geometry()
  write.csv(stats_df,
            paste0(output_path, "/",
                   "stats", "_",
                   deparse(substitute(df)), "_" ,
                   nrow(df), "x", length(df), ".csv"),
            row.names = TRUE, )
}


# f(x): metadata creator for geodb
FgdbInfo <- function(fgdb_path){
  x <- sf::st_layers(dsn = fgdb_path)
  layer_name <- x$name
  layer_type <- as.character(x$geomtype)
  n_features <- x$features
  n_fields <- x$fields
  tbt <- as_tibble(cbind(layer_name, layer_type, n_fields, n_features))
  return(tbt)
}


# f(x): Save multiple objects using saveRDS
## https://stackoverflow.com/questions/43304135/save-multiple-objects-using-saverds-and-lapply
ObjSave <- function(..., folder) {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  sapply(1:length(objects), function(i) {
    filename = paste0(folder, "/", object_names[i], ".rds")
    saveRDS(objects[i], filename)
  })
}


# f(x): Append prefix or suffix to colnames
## https://stackoverflow.com/questions/35697940/append-suffix-to-colnames
FixColNames <- function(df, prefix="", suffix="", sep="_")
{
  colnames(df) <- paste(prefix, colnames(df), suffix, sep=sep)
  return(df)
}


# f(x): Write a csv file (with added row numbers) on a certain folder
fx_saveCSV <- function(x, folder = output_path,
                       prefix = "", suffix = ""){
  write.csv(x,
            paste0(folder, "/",
                   prefix, #"_",
                   substitute(x), "_",
                   suffix, #"_",
                   nrow(x), "x",
                   length(x),
                   ".csv"),
            row.names = TRUE, )
}


# f(x): Append to a csv file (with same number of columns) on a certain folder
fx_appendCSV <- function(x, folder = output_path,
                         prefix = "", suffix = ""){
  table_path <-
    paste0(folder, "/",
           prefix, #"_",
           substitute(x), "_",
           suffix, "_",
           length(x),
           "_columns",
           ".csv")
  write.table(x, table_path,
              sep = ",", row.names = FALSE,
              col.names = !file.exists(table_path),
              append = T)
}


# f(x): Write a shapefile (with a subfolder) on a certain folder

fx_saveSHP <- function(sf, folder = output_path,
                       shpname = "shapefile",
                       prefix = "", suffix = "",
                       subfolder = "spatial_layer"
){
  writeOGR(as_Spatial(sf,
                      cast = TRUE),
           layer = paste0(prefix,
                          shpname,
                          "_n", nrow(sf)
           ),
           dsn = paste0(folder, "/",
                        subfolder,
                        suffix
           ),
           driver = "ESRI Shapefile",
           layer_options ="wkbPolygon",
           verbose = FALSE, delete_dsn = TRUE, overwrite_layer = TRUE)
}

# f(x): take sf point and return sf voronoi with data
fx_VoronoiPolygons_fromSF <- function(points){
  if(!all(st_geometry_type(points) == "POINT")){
    stop("Input not  POINT geometries")
  }
  g = st_combine(st_geometry(points)) # make multipoint
  v = st_voronoi(g)
  v = st_collection_extract(v)
  w = v[unlist(st_intersects(points, v))]
  pv = st_set_geometry(points, w)
  return(pv)
}


# f(x): Create a csv file presenting data columns and their data type
## https://stackoverflow.com/questions/21125222/determine-the-data-types-of-a-data-frames-columns

fx_fieldsView <- function(x, output_path,
                          prefix = "00", suffix = "name_fc_"){
  col_class <- sapply(x, class)
  col_type <- sapply(x, typeof)

  write.csv(cbind(col_class, col_type),
            paste0(output_path, "/",
                   prefix, "_",
                   suffix, "_",
                   nrow(x), "x", length(x),
                   ".csv"),
            row.names = TRUE, )
}

# f(x): Create a csv file presenting descriptive stats of a dataframe
## https://stackoverflow.com/questions/45176431/extract-name-of-data-frame-in-r-as-character
fx_dfWriteStats <- function(df,
                            tables_output_path = tables_output_path,
                            prefix = "", suffix = ""){
  stats_df = skim(df)
  df_name = substitute(sfdf)
  print(df_name)
  fx_saveCSV(stats_df, tables_output_path,
             paste0(prefix, "_",
                    df_name, "_"),
             suffix)
}


fx_spWriteStats <- function(sfdf,
                            tables_output_path = tables_output_path,
                            prefix = "", suffix = ""){
  stats_df = skim(sfdf %>% st_drop_geometry())
  df_name = substitute(sfdf)
  print(df_name)
  fx_saveCSV(stats_df, tables_output_path,
             paste0(prefix, "_",
                    df_name, "_"),
             suffix)
}

#' Transform raster as data.frame to be later used with ggplot
#' Modified from rasterVis::gplot
#'
#' @param x A Raster* object
#' @param maxpixels Maximum number of pixels to use
#'
#' @details rasterVis::gplot is nice to plot a raster in a ggplot but
#' if you want to plot different rasters on the same plot, you are stuck.
#' If you want to add other information or transform your raster as a
#' category raster, you can not do it. With `SDMSelect::gplot_data`, you retrieve your
#' raster as a tibble that can be modified as wanted using `dplyr` and
#' then plot in `ggplot` using `geom_tile`.
#' If Raster has levels, they will be joined to the final tibble.
#'
#' @export
#' https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
#' https://gis.stackexchange.com/questions/377444/plotting-a-raster-stack-with-ggplot2

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')

  dat <- dplyr::as.tbl(data.frame(coords, dat))

  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}

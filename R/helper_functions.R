## ---- R helper functions
# functions


# f(x): Creat a csv file presenting descriptive stats of a dataframe
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


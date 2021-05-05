library(here)
source(here("R", "libraries.R"))

source(here("R", "helper_functions.R"))

render_report  <-  function(rmd_file) {
  require(rmarkdown)
  tf <- tempfile()
  dir.create(tf)

  rmarkdown::render(here("code",
                         rmd_file),

    output_file = paste0(rmd_file,
                         "_report_",
                         format(Sys.time(), "_%m%d_%H%M"),
                         ".html"),

    envir = parent.frame()
  )

  unlink(tf)
}

render_report("03_ehe_ece_pop_estimation.Rmd")

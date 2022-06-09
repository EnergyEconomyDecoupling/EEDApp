# This module contains the ReboundTools documentation

# Render r markdown ReboundTools documentation file to HMTL
rmarkdown::render(input = "reboundtools_doc.Rmd",
                  output_dir = "www"
)

# Establishes UI module function
rebound_docUI <- function(id) {
  ns <- NS(id)

  htmltools::tags$iframe(src = "reboundtools_doc.html",
                         width = '100%',
                         height = 1000,
                         style = "border:none;")

}

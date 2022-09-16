# JS code for rendering math using katex in tables, from:
# https://stackoverflow.com/questions/67436872/rendering-katex-in-multiple-tables-in-an-r-shiny-app/67436928?noredirect=1#comment119211884_67436928
render_katex <- "
$(document).on('shiny:value', function(event) {
  if((/stages_table$/).test(event.name) || (/results_table$/).test(event.name)){
    var matches = event.value.match(/(%%+[^%]+%%)/g);
    var newvalue = event.value;
    for(var i=0; i<matches.length; i++){
      var code = '' + matches[i].slice(2,-2);
      newvalue = newvalue.replace(matches[i], katex.renderToString(code));
    }
    event.value = newvalue;
  }
})
"

# Establishes function which moves the position of the yaxis label and the legend title,
# on a plotly object produced by calling plotly::ggplotly on a ggplot object
# which has used facet_wrap() or facet_grid() therefore converting labels into annotations.
# https://github.com/ropensci/plotly/issues/1224
move_annotations <- function(gg, x, y, mar = 80){
  # Position of the Y axis label is always 1, sets distance from Y axis equal to x
  gg[['x']][['layout']][['annotations']][[1]][['x']] <- x

  # Position of the legend title is always the last element of the annotations list,
  # sets distance from bottom of graph area equal to y
  leg_title_pos <- as.numeric(length(gg[['x']][['layout']][['annotations']]))
  gg[['x']][['layout']][['annotations']][[leg_title_pos]][['y']] <- y

  # Returns plot with altered values and margin width = mar, default is 80
  gg %>% layout(margin = list(l = mar))
}


# Establishes function which moves the position of the legend title in non-faceted plots,
# where the only annotation is the legend title
move_legend_annotation_no_facet <- function(gg, y, mar = 80){
  # Position of the Y axis label is always 1, sets distance from Y axis equal to x
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- y

  # Returns plot with altered values and margin width = mar, default is 80
  gg %>% layout(margin = list(l = mar))
}

# Establishes function which moves the position of the axis titles and other annotations
# in the decoupling space plots
move_annotations_ds <- function(gg, x_y, y_x, mar = 80){

  # Position of the Y axis label, sets x co-ordinate
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y_x

  # Position of the X axis label, sets y co-ordinate
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x_y

  # Returns plot with altered values and margin width = mar, default is 80
  gg %>% layout(margin = list(l = mar))
}

# Establishes function which moves the position of the axis titles and other annotations
# in non-faceted decoupling space and rebound space plots
move_xy_ds_rs <- function(gg, x_y, y_x, mar = 80){

  # Position of the Y axis label, sets x co-ordinate
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y_x

  # Position of the X axis label, sets y co-ordinate
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x_y

  # Returns plot with altered values and margin width = mar, default is 80
  gg %>% layout(margin = list(l = mar))
}

# Function to create cumulative animation frame for a given variable, from:
# https://plotly.com/ggplot2/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# Calculates the CAAGR based on a number of years supplied by the user,
# and for two options "Centre" with calculates the CAAGR based on values either
# side of that particular year
calc_roll_caagr <- function(metric, period, direction) {
  if (period == 1) {
    caagr <- (metric / dplyr::lag(metric)) - 1
  }
  if (period > 1 && direction == "Lag") {
    p <- period
    caagr <- ((metric / dplyr::lag(metric, n = p)) ^ (1/p)) - 1
  }
  if (period > 1 && direction == "Center") {
    p <- (as.numeric(period) - 1)/2
    caagr <- ((dplyr::lead(metric, n = p) / dplyr::lag(metric, n = p)) ^ (1/period)) - 1
  }
  return(caagr)
}


# Reactively hides color bar (a plotly legend which contains a gradient)
# toggle can be set to as.logical(input$legend), which corresponds to regular legend
# TRUE FALS syntax
toggle_colorbar <- function(toggle) {

  if (toggle == TRUE) {
    plotly::hide_colorbar()
  }
  if (toggle == FALSE) {}

}















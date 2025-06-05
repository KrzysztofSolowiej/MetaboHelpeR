#' ranges UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ranges_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("linerange_plot_ui"))
  )
}

#' ranges Sidebar Function
#'
#' @description Sidebar UI for a shiny Ranges Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ranges_sidebar <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("checkbox_mean_point"), "Add mean point", value = FALSE),
    sliderInput(ns("plot_height"), 'Adjust plot height',
            min = 250, max = 1000, value = 750, step = 1
          ),
    sliderInput(ns("plot_offset"), 'Adjust segments offset',
                min = -0.5, max = 0.5, value = -0.25, step = 0.01
    )
  )
}

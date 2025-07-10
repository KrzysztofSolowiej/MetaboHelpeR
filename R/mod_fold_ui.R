#' fold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fold_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("fold_plot_ui"))
  )
}

#' fold module Sidebar Function
#'
#' @description Sidebar UI for a shiny Table Viewer Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fold_sidebar <- function(id) {
  ns <- NS(id)

  tagList(
    textOutput(ns("file_name_display")),
    uiOutput(ns("group_selector_ui")),
    selectInput(
      ns("calc_type"),
      label = "Calculation type",
      choices = c("Fold Change" = "fc", "Percent Change" = "pc"),
      selected = "fc"
    ),
    tags$div(
      sliderInput(ns("fold_plot_height"), 'Adjust plot height',
                  min = 250, max = 1000, value = 750, step = 1
      )),
    radioButtons(ns("view_mode"), "Display:",
                 choices = c("Plot" = "plot", "Table" = "table"),
                 selected = "plot", inline = TRUE),
    uiOutput(ns("download_folds_button"))
  )
}

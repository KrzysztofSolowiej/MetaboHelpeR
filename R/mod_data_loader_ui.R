#' data_loader UI Function
#'
#' @description UI Side of a shiny Data Loader Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_loader_ui <- function(id) {
  ns <- NS(id)

  tagList(
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("has_metadata")),
      h4("Group Vector"),
      DT::dataTableOutput(ns("group_metadata"))
    ),
    br(),
    DT::dataTableOutput(ns("preview"))
  )
}

#' data_loader Sidebar Function
#'
#' @description Sidebar UI for a shiny Data Loader Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_loader_sidebar <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("file_input_ui")),
    actionButton(ns("load_example_button"), "Load Example Data"),
    actionButton(ns("discard_data"), "Discard Uploaded Data")
  )
}

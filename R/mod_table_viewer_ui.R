#' table_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("cleaned_table"))
  )
}

#' table_viewer Sidebar Function
#'
#' @description Sidebar UI for a shiny Table Viewer Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_viewer_sidebar <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("download_table_button"))
    #downloadButton(ns("download_table_button"), "Download CSV file")
  )
}

#' RF UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("rf_table"))
  )
}

#' RF module Sidebar Function
#'
#' @description Sidebar UI for a shiny Table Viewer Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rf_sidebar <- function(id) {
  ns <- NS(id)

  tagList(
    textOutput(ns("file_name_display"))
  )
}

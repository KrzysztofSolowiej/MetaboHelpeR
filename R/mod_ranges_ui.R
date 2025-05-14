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
    checkboxInput(ns("checkbox_mean_point"), "Add mean point", value = FALSE)
  )
}

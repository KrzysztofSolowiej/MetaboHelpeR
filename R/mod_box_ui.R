#' box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_box_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("box_plot_ui"))
  )
}

#' box Sidebar Function
#'
#' @description Sidebar UI for a shiny Box Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_box_sidebar <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("compound_selector_ui")),
    tags$div(
      style = "display: flex; align-items: center; gap: 6px; position: relative;",
      checkboxInput(ns("checkbox_signif"), "Add significance brackets", value = FALSE),
      tags$div(
        class = "custom-tooltip-wrapper",
        tags$div(
          icon("info-circle"),
          class = "info-tooltip"
        ),
        tags$div(
          class = "custom-tooltip-text",
          "Uses Wilcoxon rank-sum test (non-parametric) to test pairwise group differences."
        )
      )
    ),
    selectInput(
      ns("plot_type"),
      label = "Plot type",
      choices = c("Boxplot" = "box", "Violin" = "violin"),
      selected = "box"
    ),
    tags$div(
      sliderInput(ns("box_plot_height"), 'Adjust plot height',
                  min = 250, max = 1000, value = 750, step = 1
    ))
  )
}


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
# app_ui <- function(request) {
#   tagList(
#     golem_add_external_resources(),
#         fluidPage(
#         titlePanel(title = span("MetaboHelpeR", style = "color: #0052cc; font-size: 55px; font-weight: bold; font-family: 'K2D';")),
#         sidebarLayout(
#           sidebarPanel(uiOutput("dynamic_sidebar"), width = 3),  # dynamically switch sidebar
#           mainPanel(
#             tabsetPanel(
#               id = "main_tabs",
#               tabPanel("Load Data", mod_data_loader_ui("loader")),
#               tabPanel("Explore Data", mod_table_viewer_ui("viewer")),
#               tabPanel("Show Ranges", mod_ranges_ui("viewer"))
#             )
#           )
#         )
#       )
#
#   )
# }
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      titlePanel(title = span("MetaboHelpeR", style = "color: #0052cc; font-size: 55px; font-weight: bold; font-family: 'K2D';")),
      sidebarLayout(
        sidebarPanel(uiOutput("dynamic_sidebar"), width = 3),
        mainPanel(
          tabsetPanel(
            id = "main_tabs",
            tabPanel("Load Data", mod_data_loader_ui("loader"))
            # DO NOT include Explore Data or Show Ranges here
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MetaboHelpeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

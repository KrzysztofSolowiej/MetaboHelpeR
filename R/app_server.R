#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  loader <- mod_data_loader_server("loader")
  mod_table_viewer_server("viewer", loader$data_loader)

  mod_ranges_server("ranges")

  tabs_inserted <- reactiveVal(FALSE)  # ← Track if we've already added the tabs

  output$dynamic_sidebar <- renderUI({
    switch(input$main_tabs,
           "Load Data" = mod_data_loader_sidebar("loader"),
           "Explore Data" = mod_table_viewer_sidebar("viewer"),
           "Show Ranges" = mod_ranges_sidebar("ranges")
    )
  })

  observeEvent(loader$data_loaded(), {
    if (loader$data_loaded() && !tabs_inserted()) {
      isolate({
        insertTab(
          inputId = "main_tabs",
          tab = tabPanel("Explore Data", mod_table_viewer_ui("viewer")),
          target = "Load Data",
          position = "after"
        )
        insertTab(
          inputId = "main_tabs",
          tab = tabPanel("Show Ranges", mod_ranges_ui("ranges")),
          target = "Explore Data",
          position = "after"
        )
        tabs_inserted(TRUE)  # Mark tabs as inserted
        updateTabsetPanel(session, "main_tabs", selected = "Load Data")
      })
    }
  }, ignoreInit = TRUE)
}

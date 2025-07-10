#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  loader <- mod_data_loader_server("loader")
  mod_table_viewer_server("viewer", loader$data_loader)
  mod_ranges_server("ranges", loader$data_loader)
  mod_box_server("box", loader$data_loader)
  mod_fold_server("fold", loader$data_loader)
  mod_pca_server("pca", loader$data_loader)
  mod_rf_server("rf", loader$data_loader)

  tabs_inserted <- reactiveVal(FALSE)  # ← Track if we've already added the tabs

  output$dynamic_sidebar <- renderUI({
    switch(input$main_tabs,
           "Load Data" = mod_data_loader_sidebar("loader"),
           "Explore Data" = mod_table_viewer_sidebar("viewer"),
           "Show Ranges" = mod_ranges_sidebar("ranges"),
           "Visualize" = mod_box_sidebar("box"),
           "Get Fold Changes" = mod_fold_sidebar("fold"),
           "See PCA" = mod_pca_sidebar("pca"),
           "Try RF" = mod_rf_sidebar("rf")
    )
  })

  observeEvent(loader$data_loaded(), {
    req(loader$data_loaded())
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
        insertTab(
          inputId = "main_tabs",
          tab = tabPanel("Visualize", mod_box_ui("box")),
          target = "Show Ranges",
          position = "after"
        )
        insertTab(
          inputId = "main_tabs",
          tab = tabPanel("Get Fold Changes", mod_fold_ui("fold")),
          target = "Visualize",
          position = "after"
        )
        insertTab(
          inputId = "main_tabs",
          tab = tabPanel("See PCA", mod_pca_ui("pca")),
          target = "Get Fold Changes",
          position = "after"
        )
        insertTab(
          inputId = "main_tabs",
          tab = tabPanel("Try RF", mod_rf_ui("rf")),
          target = "See PCA",
          position = "after"
        )
        tabs_inserted(TRUE)  # Mark tabs as inserted
        updateTabsetPanel(session, "main_tabs", selected = "Load Data")
      })
    }

  }, ignoreInit = FALSE)
}

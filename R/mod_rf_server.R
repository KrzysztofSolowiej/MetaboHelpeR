#' RF Server Functions
#'
#' @noRd
mod_rf_server <- function(id, data_loader_reactive){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$file_name_display <- renderText({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      path <- data_loaded$get_file_name()
      if (!is.null(path)) {
        path
      } else {
        "Example data"
      }
    })

    output$rf_table <- DT::renderDataTable({
      data_loaded <- data_loader_reactive()
      data <- data_loaded$get_data_excl_metadata()
      validate(need(!is.null(data), "No data available"))

      compound_col <- data_loaded$get_compound_col()
      group_vector <- data_loaded$get_group_vector()

      req(data)

      DT::datatable(
        data,
        extensions = c("FixedColumns", "ColReorder"),
        options = list(
          scrollX = TRUE,
          ordering = TRUE,
          fixedColumns = list(leftColumns = 2, rightColumns = 0),
          colReorder = TRUE,
          dom = "Bfrtip",
          pageLength = 20
        ),
        rownames = TRUE,
        class = "stripe hover"
      )


    }, server = TRUE)
  })
}

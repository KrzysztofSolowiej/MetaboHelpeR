#' table_viewer Server Functions
#'
#' @noRd
mod_table_viewer_server <- function(id, data_loader_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cleaned_table <- DT::renderDataTable({
      dl <- data_loader_reactive()
      req(dl)
      data <- dl$get_data()
      req(data)

      DT::datatable(
        data,
        extensions = c("Buttons", "FixedColumns", "ColReorder"),
        options = list(
          scrollX = TRUE,
          ordering = TRUE,
          fixedColumns = list(leftColumns = 2, rightColumns = 0),
          colReorder = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 20
        ),
        rownames = TRUE,
        filter = "top",
        class = "stripe hover"
      )
    })
  })
}

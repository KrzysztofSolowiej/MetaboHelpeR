#' table_viewer Server Functions
#'
#' @noRd
mod_table_viewer_server <- function(id, data_loader_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cleaned_table <- DT::renderDataTable({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      data <- data_loaded$get_data_excl_metadata()
      validate(need(!is.null(data), "No data available"))

      group_vector <- data_loaded$get_group_vector()
      group_indices <- data_loaded$get_group_indices()
      group_names <- names(group_indices)
      group_colors <- data_loaded$get_group_colors()

      print("Metadata checkup")
      print(data_loaded$get_metadata_info())

      group_labels <- rep("", ncol(data))
      colnames_data <- colnames(data)

      for (i in seq_along(colnames_data)) {
        col_name <- colnames_data[i]
        if (!is.null(names(group_vector)) && col_name %in% names(group_vector)) {
          group_labels[i] <- group_vector[[col_name]]
        } else {
          group_labels[i] <- ""
        }
      }

      print("group_labels")
      print(group_labels)
      print("colnames_data")
      print(colnames_data)

      display_colnames <- paste0("<div style='line-height:1.2;'>",
                                 ifelse(group_labels != "", paste0("<span style='font-size:smaller;'>", group_labels, "</span><br>"), ""),
                                 "<b>", colnames_data, "</b></div>")

      # Create datatable
      dt <- DT::datatable(
        data,
        colnames = display_colnames,
        escape = FALSE,  # Allow HTML in headers
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
        filter = "top",
        class = "stripe hover"
      )

      # Apply group-based column coloring if any groups are defined
      if (length(group_names) > 0 && !is.null(group_colors)) {
        for (group in group_names) {
          cols <- group_indices[[group]]
          color <- group_colors[[group]]
          for (col in cols) {
            dt <- DT::formatStyle(dt,
                                  columns = col,
                                  backgroundColor = color
            )
          }
        }
      }

      rownames(dt) <- NULL
      dt
    })

  })
}

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

      group_indices <- data_loaded$get_group_indices()
      group_indices_shifted <- lapply(group_indices, function(idxs) idxs + 1)
      group_names <- names(group_indices)

      # Base datatable
      dt <- DT::datatable(
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

      # Apply group-based column coloring if any groups are defined
      if (length(group_names) > 0) {
        group_colors <- if (length(group_names) < 3) {
          # Manual fallback for <3 groups
          setNames(
            head(c("#66c2a5", "#fc8d62", "#8da0cb"), length(group_names)),
            group_names
          )
        } else {
          # Use Set2 palette from RColorBrewer
          setNames(
            RColorBrewer::brewer.pal(min(length(group_names), 8), "Set2"),
            group_names
          )
        }

        # Apply color to columns for each group
        for (group in group_names) {
          cols <- group_indices_shifted[[group]]
          color <- group_colors[[group]]
          for (col in cols) {
            dt <- DT::formatStyle(dt,
                                  columns = col,
                                  backgroundColor = color)
          }
        }
      }

      rownames(dt) <- NULL
      dt
    })

  })
}

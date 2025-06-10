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
      group_names <- data_loaded$get_unique_groups()
      other_samples <- data_loaded$get_other_samples()
      group_colors <- data_loaded$get_group_colors()


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

      if (!is.null(other_samples)) {
        data <- dplyr::bind_cols(data, other_samples)
        group_labels <- c(group_labels, rep("Other", ncol(other_samples)))
        colnames_data <- c(colnames_data, colnames(other_samples))
      }

      display_colnames <- paste0("<div style='line-height:1.2;'>",
                                 ifelse(group_labels != "", paste0("<span style='font-size:smaller;'>", group_labels, "</span><br>"), ""),
                                 "<b>", colnames_data, "</b></div>")

      # print("Group vector:")
      # print(group_vector)
      # print("Group names:")
      # print(group_names)
      # print("Data structure:")
      # str(data)

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

      if (length(group_names) > 0 && !is.null(group_colors)) {
        for (group in group_names) {
          # Get column names belonging to this group
          cols <- names(group_vector)[group_vector == group]
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

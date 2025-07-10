#' pca Server Functions
#'
#' @noRd
mod_pca_server <- function(id, data_loader_reactive){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$file_name_display <- renderText({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      path <- data_loaded$get_file_name()
      if (!is.null(path)) path else "Example data"
    })

    pca_scores <- reactive({
      data_loaded <- data_loader_reactive()
      data <- data_loaded$get_data_excl_metadata()
      validate(need(!is.null(data), "No data available"))

      compound_col <- data_loaded$get_compound_col()
      group_vector <- data_loaded$get_group_vector()

      data_matrix <- data %>%
        dplyr::select(-all_of(compound_col)) %>%
        as.matrix() %>%
        t()

      data_matrix <- data_matrix[complete.cases(data_matrix), ]
      validate(need(ncol(data_matrix) > 1, "Not enough numeric data to compute PCA"))

      pca_result <- prcomp(data_matrix, center = TRUE, scale. = TRUE, rank. = 2)
      scores <- as.data.frame(pca_result$x)
      scores$Sample <- rownames(scores)

      if (!is.null(group_vector)) {
        sample_ids <- colnames(data %>% dplyr::select(-all_of(compound_col)))
        scores$Group <- group_vector[sample_ids]
      }

      scores
    })

    output$pca_table <- DT::renderDataTable({
      DT::datatable(
        pca_scores(),
        extensions = c("FixedColumns", "ColReorder"),
        options = list(
          scrollX = TRUE,
          ordering = TRUE,
          fixedColumns = list(leftColumns = 1, rightColumns = 0),
          colReorder = TRUE,
          dom = "Bfrtip",
          pageLength = 20
        ),
        rownames = FALSE,
        filter = "top",
        class = "stripe hover"
      )
    })

    output$pca_plot <- plotly::renderPlotly({
      scores <- pca_scores()
      req(scores)
      validate(need(all(c("PC1", "PC2") %in% colnames(scores)), "Missing PC1/PC2"))

      plotly::plot_ly(
        data = scores,
        x = ~PC1,
        y = ~PC2,
        type = "scatter",
        mode = "markers",
        text = ~Sample,
        color = ~Group,
        colors = "Set1",
        marker = list(size = 10, opacity = 0.8)
      ) %>%
        plotly::layout(
          title = "PCA Plot (PC1 vs PC2)",
          xaxis = list(title = "PC1"),
          yaxis = list(title = "PC2"),
          legend = list(title = list(text = "Group"))
        )
    })

  })
}

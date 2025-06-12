#' box Server Functions
#'
#' @noRd
mod_box_server <- function(id, data_loader_reactive){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$compound_selector_ui <- renderUI({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      data <- data_loaded$get_data_excl_metadata()
      compound_col <- data_loaded$get_compound_col()
      validate(need(!is.null(data), "No data available"))

      compound_names <- data[[compound_col]]
        selectizeInput(
          ns("selected_compound"),
          label = "Search compound",
          choices = compound_names,
          selected = compound_names[[1]],
          options = list(
            placeholder = "Type to search...",
            maxOptions = 10000
          )
        )
    })


    output$box_plot <- plotly::renderPlotly({
      data_loaded <- data_loader_reactive()
      height_value <- input$box_plot_height
      signif_check_value <- input$checkbox_signif
      compound_selected <- input$selected_compound
      plot_type <- input$plot_type
      req(data_loaded, height_value, compound_selected)

      compound_col <- data_loaded$get_compound_col()
      group_vector <- data_loaded$get_group_vector()
      group_colors <- data_loaded$get_group_colors()
      data <- data_loaded$get_data_excl_metadata()
      validate(need(!is.null(data), "No data available"))

      selected_row <- data[data[[compound_col]] == compound_selected, ]
      validate(need(nrow(selected_row) == 1, "Selected compound not found or duplicated"))

      numeric_data <- selected_row[, colnames(selected_row) != compound_col, drop = FALSE]
      compound_values <- as.numeric(numeric_data[1, ])
      sample_names <- colnames(numeric_data)

      df <- data.frame(
        Sample = sample_names,
        Value = compound_values,
        Group = ifelse(sample_names %in% names(group_vector), group_vector[sample_names], "Other"),
        stringsAsFactors = FALSE
      )
      df <- df[!is.na(df$Value) & !is.na(df$Group), ]
      df$Group <- factor(df$Group, levels = unique(group_vector))

      # Start plot
      plot_type <- input$plot_type

      if (plot_type == "box") {
        p <- plotly::plot_ly(
          df,
          x = ~Group,
          y = ~Value,
          type = "box",
          color = ~Group,
          colors = group_colors
        )

        # Calculate means per group
        group_means <- aggregate(Value ~ Group, data = df, FUN = mean)

        # Create custom hover text
        group_means$hover_text <- paste0("Group: ", group_means$Group,
                                         "<br>Mean: ", round(group_means$Value, 2))

        # Add mean points with custom tooltip
        p <- p %>%
          plotly::add_trace(
            data = group_means,
            x = ~Group,
            y = ~Value,
            type = "scatter",
            mode = "markers",
            marker = list(symbol = "diamond", size = 9, color = group_colors),
            text = ~hover_text,
            hoverinfo = "text",
            inherit = FALSE,
            showlegend = FALSE
          )
      } else if (plot_type == "violin") {
        p <- plotly::plot_ly(df, x = ~Group, y = ~Value, type = "violin",
                     color = ~Group, colors = group_colors,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     points = "all",
                     side = "both",
                     jitter = 0.5,
                     scalemode = "count",
                     pointpos = 0,
                     spanmode = "hard"
        )
      }

      annotations <- list()
      shapes <- list()

      if (signif_check_value && length(unique(df$Group)) >= 2) {
        group_levels <- unique(df$Group)
        combinations <- combn(group_levels, 2, simplify = FALSE)

        y_min <- min(df$Value, na.rm = TRUE)
        y_max <- max(df$Value, na.rm = TRUE)
        y_range <- y_max - y_min
        initial_offset <- y_max + y_range * 0.05
        offset_step <- y_range * 0.05
        current_offset <- 0

        for (i in seq_along(combinations)) {
          g1 <- combinations[[i]][1]
          g2 <- combinations[[i]][2]

          values1 <- df$Value[df$Group == g1]
          values2 <- df$Value[df$Group == g2]

          #test <- suppressWarnings(wilcox.test(values1, values2))
          test <- tryCatch(
            wilcox.test(values1, values2),
            error = function(e) NULL,
            warning = function(w) NULL
          )
          if (!is.null(test) && !is.na(test$p.value)) {
            print(paste("Comparing groups", g1, "vs", g2, "p-value:", test$p.value))
            pval <- test$p.value
            if (pval < 0.05) {
              label <- if (pval < 0.001) "***"
              else if (pval < 0.01) "**"
              else "*"

              x1 <- g1
              x2 <- g2
              x1_pos <- which(levels(factor(df$Group)) == g1)
              x2_pos <- which(levels(factor(df$Group)) == g2)
              y_bracket <- initial_offset + current_offset

              # Add bracket line
              shapes <- append(shapes, list(
                list(type = "line", x0 = x1_pos - 1, x1 = x2_pos - 1, y0 = y_bracket, y1 = y_bracket,
                     line = list(color = "black"))
              ))

              # Add vertical ticks
              shapes <- append(shapes, list(
                list(type = "line", x0 = x1_pos - 1, x1 = x1_pos - 1, y0 = y_bracket, y1 = y_bracket - offset_step / 3,
                     line = list(color = "black")),
                list(type = "line", x0 = x2_pos - 1, x1 = x2_pos - 1, y0 = y_bracket, y1 = y_bracket - offset_step / 3,
                     line = list(color = "black"))
              ))

              # Add significance annotation
              annotations <- append(annotations, list(
                list(
                  x = mean(c(x1_pos - 1, x2_pos - 1)),
                  y = y_bracket + offset_step / 5,
                  text = label,
                  showarrow = FALSE,
                  font = list(size = 16)
                )
              ))

              current_offset <- current_offset + offset_step
            }
          }
        }
      }

      p <- p %>% plotly::layout(
        title = list(text = paste("Boxplot for", compound_selected)),
        xaxis = list(
          title = "Group",
          type = "category"
        ),
        yaxis = list(title = "Value"),
        showlegend = TRUE,
        shapes = shapes,
        annotations = annotations
      )

      p
    })


    output$box_plot_ui <- renderUI({
      req(input$box_plot_height)
      plotly::plotlyOutput(ns("box_plot"), height = paste0(input$box_plot_height, "px"))
    })
  })
}

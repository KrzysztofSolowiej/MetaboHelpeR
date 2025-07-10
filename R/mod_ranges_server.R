#' ranges Server Functions
#'
#' @noRd
mod_ranges_server <- function(id, data_loader_reactive){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$file_name_display <- renderText({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      path <- data_loaded$get_file_name()
      if (!is.null(path)) {
        basename(path)
      } else {
        "No file loaded yet"
      }
    })

    output$linerange_plot <- plotly::renderPlotly({
      data_loaded <- data_loader_reactive()
      height_value <- input$plot_height
      offset <- input$plot_offset
      mean_check_value <- input$checkbox_mean_point
      req(data_loaded, height_value)
      compound_col <- data_loaded$get_compound_col()
      group_vector <- data_loaded$get_group_vector()
      unique_groups <- data_loaded$get_unique_groups()
      group_counts <- data_loaded$get_group_counts()
      group_indices <- data_loaded$get_group_indices()
      group_names <- names(group_indices)
      group_colors <- data_loaded$get_group_colors()
      data <- data_loaded$get_data_excl_metadata()
      validate(need(!is.null(data), "No data available"))

      long_data <- data %>%
        tidyr::pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
        dplyr::mutate(group = group_vector[.data$sample])

      grouped_ranges <- long_data %>%
        dplyr::group_by(!!rlang::sym(compound_col), group) %>%
        dplyr::summarize(
          min_value = min(value, na.rm = TRUE),
          max_value = max(value, na.rm = TRUE),
          mean_value = mean(value, na.rm = TRUE),
          range_value = max_value - min_value,
          .groups = "drop"
        )

      # Find top 25 compounds per group by range
      top_per_group <- grouped_ranges %>%
        dplyr::group_by(group) %>%
        dplyr::arrange(desc(range_value)) %>%
        dplyr::slice_head(n = 25) %>%
        dplyr::ungroup()

      # Take union of compounds from top lists
      union_compounds <- unique(top_per_group[[compound_col]])

      # Compute overall max range across groups
      overall_ranges <- grouped_ranges %>%
        dplyr::filter(!!rlang::sym(compound_col) %in% union_compounds) %>%
        dplyr::group_by(!!rlang::sym(compound_col)) %>%
        dplyr::summarize(
          max_range = max(range_value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(desc(max_range)) %>%
        dplyr::slice_head(n = 25)

      # Limit list to top 25 overall
      final_compounds <- overall_ranges[[compound_col]]

      # Filter grouped_ranges to only top compounds
      summary_data <- grouped_ranges %>%
        dplyr::filter(!!rlang::sym(compound_col) %in% final_compounds) %>%
        dplyr::ungroup()

      names(summary_data)[1] <- "Compound"

      summary_data <- summary_data %>%
        dplyr::mutate(
          Compound = forcats::fct_rev(forcats::fct_inorder(Compound)),
          Compound_num = as.numeric(Compound)
        ) %>%
        dplyr::group_split(group)

      summary_data <- purrr::imap(summary_data, function(df, i) {
        offset_centered <- (i - (length(summary_data) + 1) / 2) * offset
        df %>% dplyr::mutate(Compound_offset = Compound_num + offset_centered)
      })

      plot <- plotly::plot_ly()

      # Map each group data frame into add_segments call and add to base plot
      plot <- purrr::reduce(summary_data, function(p, df_grp) {
        group_name <- unique(df_grp$group)

        p <- p %>% plotly::add_segments(
          data = df_grp,
          x = ~min_value,
          xend = ~max_value,
          y = ~Compound_offset,
          yend = ~Compound_offset,
          name = group_name,
          type = 'scatter',
          mode = 'lines',
          line = list(width = 6, color = group_colors[[group_name]] %||% NULL),
          hoverinfo = 'text',
          legendgroup = group_name,
          text = ~paste0(
            "Compound: ", Compound, "<br>",
            "Group: ", group_name, "<br>",
            "Min: ", signif(min_value, 4), "<br>",
            "Max: ", signif(max_value, 4), "<br>",
            "Mean: ", signif(mean_value, 4)
          )
        )

        # Optional: Add mean points
        if (mean_check_value) {
          p <- p %>% plotly::add_markers(
            data = df_grp,
            x = ~mean_value,
            y = ~Compound_offset,
            name = paste0(group_name, " mean"),
            marker = list(size = 10, symbol = "circle", color = group_colors[[group_name]] %||% NULL),
            hoverinfo = 'text',
            legendgroup = group_name,
            text = ~paste0(
              "Compound: ", Compound, "<br>",
              "Group: ", group_name, "<br>",
              "Min: ", signif(min_value, 4), "<br>",
              "Max: ", signif(max_value, 4), "<br>",
              "Mean: ", signif(mean_value, 4)
            ),
            showlegend = FALSE
          )
        }
        p
      }, .init = plot)

      compound_labels <- purrr::map_dfr(summary_data, ~.x[, c("Compound", "Compound_offset")]) %>%
        dplyr::group_by(Compound) %>%
        dplyr::summarise(Compound_center = mean(Compound_offset), .groups = "drop")

      plot <- plot %>%
        plotly::layout(
          xaxis = list(title = "Value"),
          yaxis = list(
            title = "Compound",
            tickvals = compound_labels$Compound_center,
            ticktext = compound_labels$Compound,
            autorange = "reversed"
          ),
          legend = list(title = list(text = "Group"))
        )

      plot
    })

    output$linerange_plot_ui <- renderUI({
      req(input$plot_height)
      plotly::plotlyOutput(ns("linerange_plot"), height = paste0(input$plot_height, "px"))
    })
  })
}

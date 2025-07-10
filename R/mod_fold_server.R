#' fold Server Functions
#'
#' @noRd
mod_fold_server <- function(id, data_loader_reactive){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$file_name_display <- renderText({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      path <- data_loaded$get_file_name()
      if (!is.null(path)) {
        path
      } else {
        "No file loaded yet"
      }
    })

    group_names <- reactive({
      data_loaded <- data_loader_reactive()
      req(data_loaded)
      groups <- data_loaded$get_unique_groups()
      req(length(groups) > 0)
      groups
    })

    output$group_selector_ui <- renderUI({
      selectInput(
        ns("control_group"),
        "Select Control Group:",
        choices = group_names(),
        selected = group_names()[1]
      )
    })

    output$download_folds_button <- renderUI({
      data_loaded <- data_loader_reactive()
      req(data_loaded, data_with_means())
      tagList(
        br(),
        tags$p("Download the table"),
        downloadHandler(
          filename = function() {
            current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
            paste0("fold_change_table_", current_datetime, ".csv")
          },
          content = function(file) {
            data <- data_with_means()
            other_samples <- data_loaded$get_other_samples()

            if (!is.null(other_samples)) {
              data <- dplyr::bind_cols(data, other_samples)
            }

            write.csv(data, file, row.names = TRUE)
          }
        ))
    })

    data_with_means <- reactive({
      data_loaded <- data_loader_reactive()

      data <- data_loaded$get_data_excl_metadata()
      validate(need(!is.null(data), "No data available"))
      compound_col <- data_loaded$get_compound_col()
      group_vector <- data_loaded$get_group_vector()
      all_groups <- group_names()
      control_group <- input$control_group
      req(data_loaded, control_group)
      validate(need(control_group %in% all_groups, "Control group not found"))

      control_cols <- which(group_vector == control_group)
      only_data <- data %>%
        dplyr::select(-all_of(compound_col))
      control_means <- rowMeans(only_data[, control_cols, drop = FALSE], na.rm = TRUE)

      control_means_df <- data.frame(
        SelControlMean = control_means,
        stringsAsFactors = FALSE
      )
      control_means_df[[compound_col]] <- data[[compound_col]]
      control_means_df <- control_means_df[, c(compound_col, "SelControlMean")]

      for (group in all_groups) {
        if (group == control_group) next

        group_cols <- which(group_vector == group)
        group_mean <- rowMeans(only_data[, group_cols, drop = FALSE], na.rm = TRUE)

        mean_col_name <- paste0("Mean_", group)
        percent_col_name <- paste0("PercentChange_", group, "_vs_", control_group)
        fold_col_name <- paste0("FoldChange_", group, "_vs_", control_group)

        control_means_df[[mean_col_name]] <- group_mean
        control_means_df[[percent_col_name]] <- (group_mean - control_means) / control_means * 100
        control_means_df[[fold_col_name]] <- group_mean / control_means
      }

      dplyr::left_join(data, control_means_df, by = compound_col)
    })

    output$fold_table <- DT::renderDataTable({
      dat <- data_with_means()
      req(dat)

      DT::datatable(
        dat,
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
    })

    output$fold_plotly <- plotly::renderPlotly({
      data_with_means <- data_with_means()
      data_loaded <- data_loader_reactive()
      group_colors <- data_loaded$get_group_colors()
      calc_type <- input$calc_type
      height_value <- input$fold_plot_height
      req(data_with_means, group_colors, calc_type, height_value)
      compound_col <- data_loader_reactive()$get_compound_col()
      # Decide prefix based on calculation type
      prefix <- if (calc_type == "fc") "FoldChange_" else "PercentChange_"
      value_label <- if (calc_type == "fc") "Fold Change" else "Percent Change"

      # Dynamically find relevant columns
      value_cols <- grep(paste0("^", prefix), colnames(data_with_means), value = TRUE)

      long_df <- data_with_means %>%
        dplyr::select(all_of(compound_col), all_of(value_cols)) %>%
        tidyr::pivot_longer(
          cols = all_of(value_cols),
          names_to = "Group",
          values_to = "ChangeValue"
        ) %>%
        dplyr::mutate(
          Group = sub(paste0("^", prefix), "", Group),
          GroupClean = sub("_vs_.*", "", Group)
        )

      top_25_compounds <- long_df %>%
        dplyr::group_by(!!rlang::sym(compound_col)) %>%
        dplyr::summarise(max_change = max(abs(ChangeValue), na.rm = TRUE)) %>%
        dplyr::arrange(desc(max_change)) %>%
        dplyr::slice_head(n = 25) %>%
        dplyr::pull(!!rlang::sym(compound_col))

      long_df_filtered <- long_df %>%
        dplyr::filter(.data[[compound_col]] %in% top_25_compounds)

      color_map <- group_colors[names(group_colors) %in% long_df_filtered$GroupClean]

      plotly::plot_ly(
        data = long_df_filtered,
        x = ~ChangeValue,
        y = ~.data[[compound_col]],
        color = ~GroupClean,
        colors = color_map,
        type = "bar",
        orientation = "h"
      ) %>%
        plotly::layout(
          barmode = "group",
          yaxis = list(
            title = "Compound",
            categoryorder = "array",
            categoryarray = rev(top_25_compounds)
          ),
          xaxis = list(title = value_label),
          legend = list(title = list(text = "Group"))
        )
    })

    output$fold_plot_ui <- renderUI({
      req(input$fold_plot_height, input$view_mode)
      if (input$view_mode == "table") {
        DT::dataTableOutput(ns("fold_table"))
      } else {
        plotly::plotlyOutput(ns("fold_plotly"), height = paste0(input$fold_plot_height, "px"))
      }
    })

  })
}


#' data_loader UI Function
#'
#' @description Sever Side of a shiny Data Loader Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_loader_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_loader <- reactiveVal(DataLoader$new())
    is_example_data <- reactiveVal(FALSE)
    transpose_prompt_shown <- reactiveVal(FALSE)
    file_input_reset <- reactiveVal(0)
    group_mapping <- reactiveVal(NULL)
    color_mapping <- reactiveVal(NULL)
    fix_step <- reactiveVal(NULL)
    non_numeric_checked <- reactiveVal(FALSE)
    non_numeric_cols_to_fix <- reactiveVal(NULL)
    non_numeric_indices_to_fix <- reactiveVal(NULL)
    na_checked <- reactiveVal(FALSE)
    na_cols_to_fix <- reactiveVal(NULL)
    neg_cols_to_fix <- reactiveVal(NULL)
    dupli_names_to_fix <- reactiveVal(NULL)
    dup_rename_id_map <- reactiveVal()
    na_rows_to_fix <- reactiveVal(NULL)

    selected_metadata_row <- reactive({
      input$metadata_preview_rows_selected
    })

    output$file_input_ui <- renderUI({
      file_input_reset()
      fileInput(ns("file"), "Upload CSV or Excel file")
    })


    observeEvent(input$file, {
      req(input$file)
      path <- input$file$datapath
      loader <- DataLoader$new(path)
      loader$compound_col <- NULL
      data_loader(loader)
      loader$set_file_name(input$file$name)
      is_example_data(FALSE)
      transpose_prompt_shown(FALSE)

      if (!is.null(loader$sheets) && length(loader$sheets) > 1) {
        showModal(modalDialog(
          title = "Select a Sheet",
          tagList(
            selectInput(ns("sheet_select"), "Available Sheets:", choices = loader$sheets)
          ),
          footer = tagList(
            tags$div(
              style = "display: flex; justify-content: center; gap: 100px; flex-wrap: wrap;",
              actionButton(ns("load_sheet_btn"), "Load Selected Sheet"),
              actionButton(ns("dismiss_sheet_btn"), "Cancel")
            )
          ),
          easyClose = FALSE
        ))
      }
    })

    observeEvent(input$load_sheet_btn, {
      req(data_loader(), input$sheet_select)
      data_loader()$load_sheet(input$sheet_select)
      transpose_prompt_shown(FALSE)
      removeModal()
    })

    observeEvent(input$dismiss_sheet_btn, {
      transpose_prompt_shown(FALSE)
      file_input_reset(file_input_reset() + 1)
      removeModal()
    })

    observe({
      req(data_loader(), !is.null(data_loader()$get_data()), !is_example_data())
      if (isFALSE(transpose_prompt_shown())) {
        showModal(modalDialog(
          title = "Transpose Table?",
          size = "l",
          tagList(
            "Would you like to transpose the data?",
            tags$br(),
            "For the application to work correctly, samples should be columns and compounds should be rows.",
            tags$br(),
            "Please use the following visual example.",
            tags$br(),
            tags$br(),
            tags$div(
              style = "display: flex; justify-content: center; gap: 10px; flex-wrap: wrap;",
              tags$img(src = "www/transpose_table_correct.png", style = "width:48%; max-width:500px;"),
              tags$img(src = "www/transpose_table_wrong.png", style = "width:48%; max-width:500px;")
            ),
            tags$br(),
            tags$div(
              style = "display: flex; justify-content: center; width: 100%;",
              tags$div(
                style = "
                  max-width: 90%;
                  max-height: 200px;
                  overflow-y: auto;
                  border: 1px solid #ccc;
                  padding: 8px;
                  font-size: 0.85em;
                  background-color: #f9f9f9;
                  margin-top: 10px;
                ",
                tags$strong("Data preview:"),
                tableOutput(ns("head_preview"))
              )
            ),
            tags$br()
          ),
          footer = tagList(
            tags$div(
              style = "display: flex; justify-content: center; gap: 150px; flex-wrap: wrap;",
              actionButton(ns("transpose_cancel"), "Keep as is"),
              actionButton(ns("transpose_btn"), "Transpose")
            )
          ),
          easyClose = FALSE
        ))
      }
    })

    output$head_preview <- renderTable({
      req(data_loader(), data_loader()$get_data())
      df <- head(data_loader()$get_data(), 5)
      if (ncol(df) > 25) {
        df <- df[, 1:25]
      }
      df
    }, rownames = TRUE)

    observeEvent(input$transpose_btn, {
      req(data_loader())
      transposed <- transpose_data(data_loader()$get_data())
      data_loader()$data(transposed)
      transpose_prompt_shown(TRUE)
      removeModal()
      if (!is_example_data()) {
        show_compound_and_metadata_modal(names(transposed), ns)
      }
    })

    observeEvent(input$transpose_cancel, {
      transpose_prompt_shown(TRUE)
      removeModal()

      df <- data_loader()$get_data()
      if (!is_example_data()) {
        show_compound_and_metadata_modal(names(df), ns)
      }
    })

    observeEvent(input$num_groups, {
      val <- suppressWarnings(as.numeric(input$num_groups))

      is_valid <- !is.na(val) && val %% 1 == 0 && val >= 2 && val <= 8

      if (is_valid || !is.null(input$metadata_preview_rows_selected)) {
        shinyjs::enable("confirm_compound_col")
      } else {
        shinyjs::disable("confirm_compound_col")
      }

      if (!is.na(val) && val < 2) {
        updateNumericInput(session, "num_groups", value = 2)
        showNotification("Minimum number of groups is 2.", type = "warning")
      } else if (!is.na(val) && val > 8) {
        updateNumericInput(session, "num_groups", value = 8)
        showNotification("Maximum number of groups is 8.", type = "warning")
      }
    })

    output$metadata_preview <- DT::renderDataTable({
      req(data_loader(), data_loader()$get_data())
      df <- head(data_loader()$get_data(), 5)
      if (ncol(df) > 25) df <- df[, 1:25]
      DT::datatable(df, selection = list(mode = "single", target = "row"), options = list(dom = 't'))
    })

    output$custom_group_names_ui <- renderUI({
      req(input$num_groups)

      num_groups <- min(max(2, input$num_groups), 8)

      palette_fn <- scales::hue_pal()
      palette <- palette_fn(num_groups)

      lapply(seq_len(num_groups), function(i) {
        fluidRow(
          column(6, textInput(ns(paste0("group_name_", i)), paste("Group", i, "name:"), value = paste("Group", LETTERS[i]))),
          column(6, colourpicker::colourInput(ns(paste0("group_color_", i)), "Color", value = palette[i]))
        )
      })
    })

    observeEvent(input$confirm_compound_col, {
      loader <- data_loader()
      loader$set_compound_col(input$compound_col_select)

      if (isTRUE(input$has_metadata)) {
        if (is.null(input$metadata_preview_rows_selected)) {
          showNotification("Please select a group row.", type = "message")
          return()
        }

        loader$set_metadata_info(index = input$metadata_preview_rows_selected)

        data_loader(loader)

        shiny::isolate({
          fix_step("check_non_numeric")
        })

        removeModal()
        return()
      }

      if (!isTRUE(input$has_metadata)) {
        removeModal()
        num_groups <- input$num_groups
        group_names <- sapply(seq_len(num_groups), function(i) input[[paste0("group_name_", i)]])
        show_manual_group_dnd_modal(loader, ns, group_names = group_names)
        return()
      }
    })

    observeEvent(input$confirm_manual_groups, {
      loader <- data_loader()
      num_groups <- input$num_groups

      # Retrieve custom group names and colors
      custom_group_names <- sapply(seq_len(num_groups), function(i) input[[paste0("group_name_", i)]])
      custom_group_colors <- sapply(seq_len(num_groups), function(i) input[[paste0("group_color_", i)]])
      names(custom_group_colors) <- custom_group_names
      color_mapping(custom_group_colors)

      group_inputs <- setNames(
        lapply(seq_len(num_groups), function(i) input[[paste0("group_", i)]]),
        custom_group_names
      )

      # Store excluded and other samples if needed
      excluded_samples <- input$group_exclude
      other_samples <- input$group_other

      # Build group mapping without "Remove"
      sample_to_group <- unlist(lapply(names(group_inputs), function(g) {
        samples <- group_inputs[[g]]
        if (!is.null(samples)) {
          setNames(rep(g, length(samples)), samples)
        }
      }))
      group_mapping(sample_to_group)
      df <- loader$get_data()

      # Store "Other" samples
      other_samples_stored <- df %>%
        dplyr::select(dplyr::all_of(other_samples))
      loader$set_other_samples(other_samples_stored)

      # Filter data to exclude "Remove" and "Other" samples
      df <- df[, setdiff(names(df), excluded_samples)]
      df <- df[, setdiff(names(df), other_samples)]
      loader$set_data(df)
      loader$set_manual_group_mapping(sample_to_group, group_colors = custom_group_colors)
      data_loader(loader)
      removeModal()

      fix_step("check_non_numeric")
    })


    observeEvent(fix_step(), {
      step <- fix_step()
      loader <- data_loader()

      if (step == "check_non_numeric") {
        result <- check_and_handle_non_numeric(data_loader, ns)
        if (!is.null(result)) {
          non_numeric_cols_to_fix(result$names)
          non_numeric_indices_to_fix(result$indices)
          return()
        } else {
          fix_step("check_na")
        }

      } else if (step == "check_na") {
        na_cols <- check_and_handle_nas(data_loader, ns)
        if (length(na_cols) > 0) {
          na_cols_to_fix(na_cols)
          return()
        } else {
          fix_step("check_negative")
        }

      } else if (step == "check_negative") {
        neg_cols <- check_no_negatives(data_loader, ns)
        if (length(neg_cols) > 0) {
          neg_cols_to_fix(neg_cols)
          return()
        } else {
          fix_step("check_duplicates")
        }

      } else if (step == "check_duplicates") {
        dup_names <- check_and_handle_duplicates(data_loader, ns)
        if (!is.null(dup_names) && length(dup_names) > 0) {
          dupli_names_to_fix(dup_names)
          return()
        } else {
          fix_step(NULL)  # done
        }
      }
    })

    observeEvent(input$apply_column_fixes, {
      loader <- data_loader()
      df <- data_loader()$get_data()
      cols <- non_numeric_cols_to_fix()
      group_map <- group_mapping()
      custom_group_colors <- color_mapping()
      req(cols)


      for (col in cols) {
        strategy <- input[[paste0("action_", col)]]
        print(paste("Handling non-numeric col: ", col, "with strategy:", strategy))

        if (strategy == "convert") {
          df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
        } else if (strategy == "remove") {
          group_map_clean <- group_map[!names(group_map) %in% cols]
          group_mapping(group_map_clean)
          loader$set_manual_group_mapping(group_map_clean, group_colors = custom_group_colors)
          data_loader(loader)
          df[[col]] <- NULL
        }
        # Do nothing if strategy is "none"
      }

      data_loader()$set_data(df)
      removeModal()

      fix_step("check_na")
    })

    observeEvent(input$apply_na_fixes, {
      loader <- data_loader()
      df <- data_loader()$get_data()
      cols <- na_cols_to_fix()
      group_map <- group_mapping()
      custom_group_colors <- color_mapping()
      req(cols)

      cols_to_remove <- c()

      for (col in cols) {
        strategy <- input[[paste0("na_action_", col)]]
        print(paste("Handling NA col: ", col, ", with strategy:", strategy))
        if (strategy == "convert_zero") {
          df[[col]][is.na(df[[col]])] <- 0
        } else if (strategy == "remove_col") {
          cols_to_remove <- c(cols_to_remove, col)
        }
      }

      if (!is.null(group_map) && !is.null(custom_group_colors)) {
        group_map_clean <- group_map[!names(group_map) %in% cols_to_remove]
        group_mapping(group_map_clean)
        loader$set_manual_group_mapping(group_map_clean, group_colors = custom_group_colors)
        df <- df[, !names(df) %in% cols_to_remove]
      }

      data_loader()$set_data(df)
      print(sum(is.na(data_loader()$get_data())))
      removeModal()
      fix_step("check_negative")
    })

    observeEvent(input$apply_neg_fixes, {
      loader <- data_loader()
      df <- data_loader()$get_data()
      cols <- neg_cols_to_fix()
      group_map <- group_mapping()
      custom_group_colors <- color_mapping()
      req(cols)

      cols_to_remove <- c()

      for (col in cols) {
        strategy <- input[[paste0("neg_action_", col)]]
        print(paste("Handling neg col: ", col, ", with strategy:", strategy))
        if (strategy == "convert_zero") {
          df[[col]][df[[col]] < 0] <- 0
        } else if (strategy == "remove_col") {
          cols_to_remove <- c(cols_to_remove, col)
        }
      }

      if (!is.null(group_map) && !is.null(custom_group_colors)) {
        group_map_clean <- group_map[!names(group_map) %in% cols_to_remove]
        group_mapping(group_map_clean)
        loader$set_manual_group_mapping(group_map_clean, group_colors = custom_group_colors)
      }

      data_loader()$set_data(df)
      removeModal()
      fix_step("check_duplicates")
    })


    observeEvent(input$apply_dup_renames, {
      loader <- data_loader()
      df <- data_loader()$get_data()
      comp_col <- loader$get_compound_col()
      dup_names <- dupli_names_to_fix()
      print("Duplicate names to fix:")
      print(dup_names)
      req(dup_names)

      comp_names <- df[[comp_col]]
      new_names <- comp_names

      input_counter <- 1
      for (name in dup_names) {
        indices <- which(comp_names == name)
        if (length(indices) < 2) next
        for (j in 2:length(indices)) {
          new_name <- input[[paste0("rename_dup_", input_counter)]]
          if (!is.null(new_name) && new_name != "") {
            new_names[indices[j]] <- new_name
          }
          input_counter <- input_counter + 1
        }
      }

      df[[comp_col]] <- new_names
      loader$set_data(df)

      removeModal()
      fix_step(NULL)
    })

    observeEvent(input$load_example_button, {
      loader <- DataLoader$new()
      loader$load_example()
      is_example_data(TRUE)
      transpose_prompt_shown(TRUE)
      loader$set_compound_col("Compound Name")
      file_input_reset(file_input_reset() + 1)

      other_samples <- c("Mass", "RT")
      df <- loader$get_data()

      other_samples_stored <- df %>%
        dplyr::select(dplyr::all_of(other_samples))
      loader$set_other_samples(other_samples_stored)

      df <- df[, setdiff(names(df), other_samples)]
      loader$set_data(df)
      fixed_group_mapping <- c("K-1" = "Control", "K-10" = "Control", "K-11" = "Control", "K-12" = "Control",
                               "K-13" = "Control", "K-14" = "Control", "K-15" = "Control", "K-16" = "Control",
                               "K-17" = "Control", "K-18" = "Control", "K-19" = "Control", "K-2" = "Control",
                               "K-20" = "Control", "K-21" = "Control", "K-22" = "Control", "K-23" = "Control",
                               "K-24" = "Control", "K-25" = "Control", "K-26" = "Control", "K-27" = "Control",
                               "K-28" = "Control", "K-29" = "Control", "K-3" = "Control", "K-30" = "Control",
                               "K-4" = "Control", "K-5" = "Control", "K-6" = "Control", "K-7" = "Control",
                               "K-8" = "Control", "K-9" = "Control", "16V0" = "Case", "15V0" = "Case",
                               "10V0" = "Case", "17V0" = "Case", "11V0" = "Case", "18V0" = "Case",
                               "19V0" = "Case", "20V0" = "Case", "22V0" = "Case", "14V0" = "Case",
                               "23V0" = "Case", "26V0" = "Case", "12V0B" = "Case", "27V0" = "Case",
                               "29V0" = "Case", "30V0" = "Case", "25V0" = "Case", "33V0" = "Case",
                               "32V0" = "Case", "34V0" = "Case", "35V0" = "Case", "28V0" = "Case",
                               "39V0" = "Case", "38V0" = "Case", "40V0" = "Case", "24V0" = "Case",
                               "13V0" = "Case", "12V0a" = "Case", "36V0" = "Case", QC_3 = "QC",
                               QC_7 = "QC", QC_2 = "QC", QC_12 = "QC", QC_4 = "QC", QC_5 = "QC",
                               QC_6 = "QC", QC_11 = "QC", QC_10 = "QC", QC_8 = "QC", QC_1 = "QC",
                               QC_9 = "QC")
      fixed_colors <- c(Control = "#F8766D", Case = "#00BA38", QC = "#619CFF")
      loader$set_manual_group_mapping(fixed_group_mapping, group_colors = fixed_colors)


      data_loader(loader)
    })

    output$group_metadata <- DT::renderDataTable({
      req(data_loader(), transpose_prompt_shown(), input$has_metadata, input$confirm_compound_col)
      df <- data_loader()$get_group_metadata_df()
      DT::datatable(df,
                    extensions = c('FixedColumns', 'ColReorder'),
                    options = list(dom = 't',
                                   ordering = FALSE,
                                   scrollX = TRUE,
                                   fixedColumns = list(leftColumns = 1, rightColumns = 0),
                                   colReorder = TRUE),
                    rownames = FALSE)
    })

    output$preview <- DT::renderDataTable({
      req(data_loader(), transpose_prompt_shown())
      if (!is_example_data()) {
        req(input$confirm_compound_col)
      }
      df <- data_loader()$get_data_excl_metadata()
      DT::datatable(df,
                    extensions = c('FixedColumns', 'ColReorder'),
                    options = list(
                      pageLength = 10,
                      ordering = TRUE,
                      scrollX = TRUE,
                      fixedColumns = list(leftColumns = 2, rightColumns = 0),
                      colReorder = TRUE),
                    rownames = TRUE)

    })

    observeEvent(input$discard_data, {
      session$reload()
    })

    return(list(
      data_loader = data_loader,
      data_loaded = reactive({ !is.null(data_loader()$get_data_excl_metadata()) })
    ))

  })
}

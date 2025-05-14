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
    non_numeric_checked <- reactiveVal(FALSE)
    non_numeric_cols_to_fix <- reactiveVal(NULL)
    na_checked <- reactiveVal(FALSE)
    na_cols_to_fix <- reactiveVal(NULL)
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

    show_compound_and_metadata_modal <- function(column_choices) {
      showModal(modalDialog(
        title = "Select Compound Column and Metadata",
        tagList(
          selectInput(ns("compound_col_select"), "Which column contains compound names?", choices = column_choices),
          checkboxInput(ns("has_metadata"), "Does your data include group metadata?", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("has_metadata")),
            h5("Click on a row below to set it as group metadata:"),
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
              DT::dataTableOutput(ns("metadata_preview"))
            )
          )
        ),
        footer = tagList(
          actionButton(ns("confirm_compound_col"), "Confirm"),
          modalButton("Cancel")
        ),
        size = "l"
      ))
    }

    output$metadata_preview <- DT::renderDataTable({
      req(data_loader(), data_loader()$get_data())
      df <- head(data_loader()$get_data(), 5)
      if (ncol(df) > 25) df <- df[, 1:25]
      DT::datatable(df, selection = list(mode = "single", target = "row"), options = list(dom = 't'))
    })

    observeEvent(input$transpose_btn, {
      req(data_loader())
      transposed <- transpose_data(data_loader()$get_data())
      data_loader()$data(transposed)
      transpose_prompt_shown(TRUE)
      removeModal()
      if (!is_example_data()) {
        show_compound_and_metadata_modal(names(transposed))
      }
    })

    observeEvent(input$transpose_cancel, {
      transpose_prompt_shown(TRUE)
      removeModal()

      df <- data_loader()$get_data()
      if (!is_example_data()) {
        show_compound_and_metadata_modal(names(df))
      }
    })

    observeEvent(input$confirm_compound_col, {
      loader <- data_loader()

      loader$set_compound_col(input$compound_col_select)

      if (isTRUE(input$has_metadata)) {
        if (is.null(input$metadata_preview_rows_selected)) {
          showNotification("Please select a group row.", type = "message")
          return()
        }

        loader$set_metadata_info(
          index = input$metadata_preview_rows_selected
        )
      }

      data_loader(loader)
      removeModal()
      non_numeric_cols_to_fix(check_and_handle_non_numeric(data_loader, ns))
    })

    observeEvent(input$apply_column_fixes, {
      df <- data_loader()$get_data()
      cols <- non_numeric_cols_to_fix()
      req(cols)

      for (col in cols) {
        strategy <- input[[paste0("action_", col)]]
        print(paste("Handling", col, "with strategy:", strategy))

        if (strategy == "convert") {
          df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
        } else if (strategy == "remove") {
          df[[col]] <- NULL
        }
        # Do nothing if strategy is "none"
      }

      data_loader()$set_data(df)
      removeModal()
      na_cols <- check_and_handle_nas(data_loader, ns)
      na_cols_to_fix(na_cols)
    })

    observeEvent(input$apply_na_fixes, {
      df <- data_loader()$get_data()
      cols <- na_cols_to_fix()
      req(cols)

      for (col in cols) {
        strategy <- input[[paste0("na_action_", col)]]
        if (strategy == "convert_zero") {
          df[[col]][is.na(df[[col]])] <- 0
        } else if (strategy == "remove_row") {
          df <- df[!is.na(df[[col]]), , drop = FALSE]
        } else if (strategy == "remove_col") {
          df[[col]] <- NULL
        }
      }

      data_loader()$set_data(df)
      print(sum(is.na(data_loader()$get_data())))
      removeModal()
    })

    observeEvent(input$load_example_button, {
      loader <- DataLoader$new()
      loader$load_example()
      is_example_data(TRUE)
      transpose_prompt_shown(TRUE)
      loader$set_compound_col("Compound.Name")
      data_loader(loader)
      file_input_reset(file_input_reset() + 1)
    })

    output$group_metadata <- DT::renderDataTable({
      req(data_loader(), transpose_prompt_shown(), input$has_metadata, input$confirm_compound_col)
      df <- data_loader()$get_group_metadata_df()
      DT::datatable(df, options = list(dom = 't'), rownames = FALSE)
    })

    output$preview <- DT::renderDataTable({
      req(data_loader(), transpose_prompt_shown())
      if (!is_example_data()) {
        req(input$confirm_compound_col)
      }
      df <- data_loader()$get_data_excl_metadata()
      DT::datatable(df, options = list(pageLength = 10), rownames = TRUE)
    })
  })
}

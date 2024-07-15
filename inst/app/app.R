MetaboHelpeR <- function(...) {

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(data.table)
library(purrr)
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(gtools)
library(Rfast)
library(outliers)
library(MASS)
library(isotree)
library(pls)
library(dbscan)
library(plotly)
library(sortable)
library(stringr)
library(randomForest)
library(bestNormalize)
library(car)
library(beanplot)
library(shinycssloaders)
library(shinyjs)
library(preprocessCore)
library(dunn.test)

ui <- fluidPage(
  useShinyjs(),
  titlePanel(title = span("MetaboHelpeR", style = "color: #0052cc; font-size: 55px; font-weight: bold; font-family: 'K2D';")),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=K2D:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800&display=swap"),
    tags$script(HTML('
      $(document).ready(function(){
        $(\'[data-toggle="tooltip"]\').tooltip({html: true});
      });
    '))
  ),
  sidebarLayout(
    sidebarPanel(
      uiOutput('file_input'),
      actionButton("discard_data", "Discard Uploaded Data"),
      uiOutput('custom_title'),
      #uiOutput('transpose_button'),
      uiOutput('mass_threshold'),
      uiOutput('ppm_threshold'),
      uiOutput('rt_threshold'),
      uiOutput('duplicates_button'),
      uiOutput('dupli_cv_button'),
      uiOutput('anom_slider'),
      uiOutput('anomalies_button'),
      uiOutput('select_row'),
      uiOutput('remove_row'),
      uiOutput('add_group_button'),
      uiOutput('group_indices'),
      uiOutput('mod_group_indices'),
      uiOutput('group_store_buttons'),
      uiOutput('deselect_button'),
      uiOutput('min_max_cols_check'),
      uiOutput('add_mean_col_check'),
      uiOutput('coeff_of_var_col_check'),
      uiOutput('table_download'),
      uiOutput('plot_height'),
      uiOutput('nudge_selector'),
      uiOutput('mean_point'),
      uiOutput('color_blind'),
      uiOutput('ranges_crit_type'),
      uiOutput('range_seed_setter'),
      uiOutput('range_tree_setter'),
      uiOutput('checkbox_scale_y'),
      uiOutput('checkbox_log2'),
      uiOutput('dropdown_type'),
      uiOutput('dist_xlab'),
      uiOutput('dist_ylab'),
      uiOutput('dropdown_comp'),
      uiOutput('grubbs_transform'),
      uiOutput('grubbs_scale'),
      uiOutput('grubbs_drop'),
      uiOutput('pca_drop'),
      uiOutput('pca_method_drop'),
      uiOutput('pca_dist_drop'),
      uiOutput('pca_interest_check'),
      uiOutput('pca_criterion_drop'),
      uiOutput('quant_val'),
      uiOutput('sd_val'),
      uiOutput('top_compound_real_selector'),
      #uiOutput('top_compound_real_selector_dist'),
      uiOutput("top_compounds_selector"),
      uiOutput('compound_selector'),
      uiOutput('compound_sorter'),
      uiOutput('pca_compound_selector'),
      uiOutput('sample_selector'),
      uiOutput('seed_setter'),
      uiOutput('tree_setter'),
      #uiOutput('normality_button'),
      #uiOutput('vareq_button'),
      uiOutput('norm_group'),
      uiOutput('norm_sort'),
      uiOutput('homo_sort'),
      uiOutput('norm_norm'),
      uiOutput('homo_norm'),
      uiOutput('norm_transf'),
      uiOutput('homo_transf'),
      uiOutput('norm_scale'),
      uiOutput('homo_scale'),
      uiOutput('checkbox_swarm'),
      #uiOutput('parametric_select'),
      uiOutput('para_sort'),
      uiOutput('para_norm'),
      uiOutput('para_transf'),
      uiOutput('para_scale'),
      uiOutput('tukey_checkbox'),
      uiOutput('tukey_download'),
      #uiOutput('non_parametric_select'),
      uiOutput('pair_wise_ui'),
      uiOutput('non_para_sort'),
      uiOutput('non_para_norm'),
      uiOutput('non_para_transf'),
      uiOutput('non_para_scale'),
      uiOutput('non_tukey_checkbox'),
      uiOutput('dunn_download'),
      #uiOutput('checkbox_fdr'),
      uiOutput('corrections_radio'),
      verbatimTextOutput('percent_above_005'),
      verbatimTextOutput('highest_p_val'),
      verbatimTextOutput('lowest_p_val'),
      verbatimTextOutput('levene_percent_above_005'),
      verbatimTextOutput('levene_highest_p_val'),
      verbatimTextOutput('levene_lowest_p_val'),
      verbatimTextOutput('para_percent_above_005'),
      verbatimTextOutput('para_highest_p_val'),
      verbatimTextOutput('para_lowest_p_val'),
      verbatimTextOutput('non_para_percent_above_005'),
      verbatimTextOutput('non_para_highest_p_val'),
      verbatimTextOutput('non_para_lowest_p_val'),
      uiOutput('para_download'),
      uiOutput('non_para_download'),
      width = 3),

# Main panel ------------------------

    mainPanel(
      #includeCSS("R/www/style.css"),
      tags$head(
        tags$style(HTML("
        #hover_histogram_container {
            position: absolute;
            top: 0px;
            left: 700px;
            width: 40%;
        }
        #plot_for_hover {
            position: absolute;
            top: 0px;
            left: 0px;
            width: 60%;
        }
        #hover_beanplot_container {
            position: absolute;
            top: 0px;
            left: 700px;
            width: 40%;
        }
        #levene_for_hover {
            position: absolute;
            top: 0px;
            left: 0px;
            width: 60%;
        }
        #hover_para_container,
        #hover_non_para_container {
            position: absolute;
            top: 0px;
            right: 0px;
            width: 40%;
        }

        #tukey_table_container,
        #dunn_table_container {
            position: absolute;
            top: 760px;
            right: 0px;
            width: 100%;
        }

        #tukey_hover_container,
        #dunn_hover_container {
          position: absolute;
          top: 360px;
          right: 0px;
          width: 40%;
        }

        #para_for_hover,
        #non_para_for_hover {
            position: absolute;
            top: 0px;
            left: 0px;
            width: 60%;
        }
      "))
      ),
      tabsetPanel(id = "tabsetPanelID",
        tabPanel(id = 'processy', 'Pre-process',
                 withSpinner(DT::DTOutput('process_data')),
                 div(id = "spinner", class = "loader", style = "display:none;"),
                 DT::DTOutput("duplicate_info"),
                 DT::DTOutput("anomaly_info")),
        tabPanel(id = 'tably', 'Table',
                 withSpinner(DT::DTOutput('data_table'))),
        tabPanel(id = 'rangy', 'Ranges',
                 withSpinner(plotly::plotlyOutput('plot_data'))),
                 #DT::DTOutput('plot_data')),
        tabPanel(id = 'changey', '% of change',
                 withSpinner(plotly::plotlyOutput('change_data'))),
              #   DT::DTOutput('change_data')),
        tabPanel(id = "foldy", 'Fold change',
                # DT::DTOutput('fold_data')),
                withSpinner(plotly::plotlyOutput('fold_data'))),
        tabPanel(id = 'disty', 'Distribution plots',
                 withSpinner(plotly::plotlyOutput('dist_data'))),
        tabPanel(id = 'pca', 'Outlier detection',
                #DT::DTOutput('pca_data')
                div(style = "height: 550px; overflow-y: scroll;",
                    withSpinner(plotly::plotlyOutput('pca_data'))),
                hr(),
                plotOutput('outliers_table')
                ),
        tabPanel(id = 'gini', 'RF Gini index',
                 withSpinner(plotly::plotlyOutput('gini_data')),
                 DT::DTOutput("conf_matrix"),
                 DT::DTOutput("mean_dec_acc"),
                 textOutput("oob_err")),
        tabPanel(id = 'staty', 'Normality',
                 uiOutput("prev_page_button"),
                 uiOutput("next_page_button"),
                 uiOutput("page_info_text"),
                 div(
                   style = "position:relative",
                   div(id = "plot_for_hover", plotOutput('stats_data', hover = hoverOpts(id = "plot_hover", delay = 50))),
                   div(id = "hover_histogram_container", plotOutput("hover_histogram"))
                 )
                 ),
        tabPanel(id = 'leve', 'Homoscedasticity',
                 uiOutput("leve_prev_page_button"),
                 uiOutput("leve_next_page_button"),
                 uiOutput("leve_page_info_text"),
                 withSpinner(div(
                   style = "position:relative",
                   div(id = "levene_for_hover", plotOutput('levene_data', hover = hoverOpts(id = "levene_hover", delay = 50))),
                   div(id = "hover_beanplot_container", plotOutput("hover_beanplot")))
                   )
                 ),
        tabPanel(id = 'pary', 'Parametric tests',
                 uiOutput("para_prev_page_button"),
                 uiOutput("para_next_page_button"),
                 uiOutput("para_page_info_text"),
                 #DT::DTOutput('para_data')
                 #plotOutput('para_data')
                 withSpinner(div(
                   style = "position:relative",
                   div(id = "para_for_hover", plotOutput('para_data', hover = hoverOpts(id = "para_hover", delay = 50))),
                   div(id = "hover_para_container", plotOutput("hover_para_hist")),
                   div(id = "tukey_hover_container", plotOutput("tukey_results")),
                   div(id = "tukey_table_container", DT::DTOutput('tukey_results_table'))
                 ))
                 ),
        tabPanel(id = 'nonpary', 'Non-Parametric tests',
                 uiOutput("non_para_prev_page_button"),
                 uiOutput("non_para_next_page_button"),
                 uiOutput("non_para_page_info_text"),
                 #DT::DTOutput('non_para_data')
                 #plotOutput('non_para_data')
                 withSpinner(div(
                   style = "position:relative",
                   div(id = "non_para_for_hover", plotOutput('non_para_data', hover = hoverOpts(id = "non_para_hover", delay = 50))),
                   div(id = "hover_non_para_container", plotOutput("hover_non_para_hist")),
                   div(id = "dunn_hover_container", plotOutput("non_tukey_results")),
                   div(id = "dunn_table_container", DT::DTOutput('dunn_results_table'))
                 ))
                 ),
        ),
      width = 9)
    )
  )

server <- function(input, output, session) {

  my_data <- reactiveVal(NULL)
  updated_data <- reactiveVal(NULL)
  transposed_table_stored <- reactiveVal(NULL)
  processed_data_with_stats <- reactiveVal(NULL)

  observe({
    updated_data(my_data())
  })

  group_data <- reactiveVal(list())
  common_column <- reactiveVal(NULL)
  merged_data <- reactiveVal(NULL)
  title_value <- reactiveVal(NULL)
  active_tab <- reactiveVal(NULL)
  num_of_group <- reactiveVal(NULL)
  rv <- reactiveValues(near_duplicate_data = NULL)

  observeEvent(input$discard_data, {
    session$reload()
  })

  observeEvent(input$tabsetPanelID, {
    active_tab(input$tabsetPanelID)
   # print(paste("Active tab:", active_tab()))
  })

  observeEvent(input$group_num, {
    num_of_group(input$group_num)
  })

  observe({
    data_df <- my_data()
    if (!is.null(data_df)) {
      common_column(names(data_df)[1])
    }
    #print(paste("Common column:", common_column()))
  })

  read_my_data <- function(file_path) {
    extension <- tools::file_ext(file_path)
    if (extension %in% c("csv", "txt")) {
      data <- readr::read_delim(file_path, show_col_types = FALSE)
    } else if (extension %in% c("xls", "xlsx")) {
      data <- readxl::read_excel(file_path)
    } else {
      warning("Unsupported file type. Please upload CSV or Excel files.")
      return(NULL)
    }
    return(data)
  }


  # File input
  # output$file_input <- renderUI({
  #   if (is.null(my_data())) {
  #     fileInput(
  #       inputId = "users_path",
  #       label = "Select data:",
  #       multiple = FALSE,
  #       accept = c(".csv", ".xlsx")
  #     )
  #   }
  # })

  output$file_input <- renderUI({
    if (is.null(my_data())) {
      tagList(
        fileInput(
          inputId = "users_path",
          label = "Select data:",
          multiple = FALSE,
          accept = c(".csv", ".xlsx")
        ),
        actionButton(
          inputId = "load_example_data",
          label = "Load Example Data"
        ),
        hr()
      )
    }
  })

  observeEvent(input$users_path, {
    req(input$users_path)
    file_path <- input$users_path$datapath
    data <- read_my_data(file_path)
    if (!is.null(data)) {
      my_data(data)
    } else {
      my_data(NULL)
    }
  })

  observeEvent(input$load_example_data, {
    example_file_path <- system.file("extdata", "example_data.csv", package = "MetaboHelpeR")
    example_data <- read_my_data(example_file_path)
    if (!is.null(example_data)) {
      my_data(example_data)
    } else {
      my_data(NULL)
    }
  })

# Store groups logic -------------------

  group_counter <- reactiveVal(1)
  selected_columns_by_group <- reactiveVal(list())

  # observe({
  #   print(selected_columns_by_group())
  # })

  # Function to render store button for each group
  render_store_button <- function(group_num) {
    actionButton(paste0("store_button_", group_num), paste("Store and Modify Columns for Group", group_num))
  }

  # Render store buttons for each group
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$group_store_buttons <- renderUI({
        buttons <- lapply(1:group_counter(), function(i) {
          render_store_button(i)
        })
        do.call(tagList, buttons)
      })
    } else {
      output$group_store_buttons <- renderUI({
        div()
      })
    }
  })


  # Observe store button clicks for each group
  observe({
    lapply(1:group_counter(), function(i) {
      observeEvent(input[[paste0("store_button_", i)]], {
        selected_cols <- selected_columns()
        if (!is.null(selected_cols)) {
          data_df <- my_data()
          selected_col_names <- names(data_df)[selected_cols]
          current_selections <- selected_columns_by_group()
          current_selections[[i]] <- selected_col_names
          selected_columns_by_group(current_selections)
        }
      })
    })
  })


  # Render group indices
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$group_indices <- renderUI({
        tagList(
          lapply(1:group_counter(), function(i) {
            verbatimTextOutput(paste0('indices_', i))
          })
        )
      })
    } else {
      output$group_indices <- renderUI({
        div()
      })
    }
  })

  # Render modified group indices
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$mod_group_indices <- renderUI({
        tagList(
          lapply(1:group_counter(), function(i) {
            verbatimTextOutput(paste0('mod_indices_', i))
          })
        )
      })
    } else {
      output$mod_group_indices <- renderUI({
        div()
      })
    }
  })


  # Function to increment group counter
  increment_group_counter <- function() {
    group_counter(group_counter() + 1)
  }

  # Render button to add another group
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$add_group_button <- renderUI({
        actionButton("add_group_button", "Add Another Group")
      })
    } else {
      output$add_group_button <- renderUI({
        div()
      })
    }
  })


  # Observe click event on add group button
  observeEvent(input$add_group_button, {
    increment_group_counter()
  })


  # Render deselect columns button
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$deselect_button <- renderUI({
        tagList(
          hr(),
          actionButton("deselect_all", "Deselect All Columns")
        )
      })
    } else {
      output$deselect_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$select_row <- renderUI({
        textInput("row_remove_sel", "Select rows to remove (comma separated):", "")
      })
    } else {
      output$select_row <- renderUI({
        div()
      })
    }
  })

  # Render remove selected row button
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$remove_row <- renderUI({
        tagList(
          actionButton("row_remover", "Remove selected row"),
          hr()
        )
      })
    } else {
      output$remove_row <- renderUI({
        div()
      })
    }
  })

  observeEvent(input$row_remover, {
    req(updated_data())
    if (!is.null(input$row_remove_sel)) {
      rows_to_remove <- as.numeric(strsplit(input$row_remove_sel, ",\\s*")[[1]])
      rows_to_remove <- rows_to_remove[rows_to_remove > 0 & rows_to_remove <= nrow(updated_data())]
      if (length(rows_to_remove) > 0) {
        updated_rows <- updated_data()[-rows_to_remove, ]
        updated_data(updated_rows)
      }
    }
  })


  # Function to parse compound names
  pars_compounds <- function(data) {
    masses <- vector("numeric", length = nrow(data))
    retention_times <- vector("numeric", length = nrow(data))
    for (i in 1:nrow(data)) {
      compound_parts <- strsplit(as.character(data[i, 1]), "@")[[1]]
      if (length(compound_parts) == 2) {
        masses[i] <- as.numeric(compound_parts[1])
        retention_times[i] <- as.numeric(compound_parts[2])
      } else {
        cat("Incorrect format for compound at row", i, ":", data[i, 1], "\n")
      }
    }
    data$New_Masses <- masses
    data$New_Retention_Time <- retention_times
    return(data)
  }


  # Function to find near duplicates both absolute and relative (ppm)
  find_near_duplicates <- function(data, mass_threshold, rt_threshold, ppm_threshold) {
    is_near_duplicate <- rep(FALSE, nrow(data))
    delta_ppm <- rep(NA, nrow(data))

    for (i in 1:(nrow(data) - 1)) {
      for (j in (i + 1):nrow(data)) {
        mass_diff_abs <- abs(data$New_Masses[i] - data$New_Masses[j])
        rt_diff <- abs(data$New_Retention_Time[i] - data$New_Retention_Time[j])

        ref_mass <- data$New_Masses[i]
        rel_mass <- data$New_Masses[j]
        mass_diff_ppm <- (abs(ref_mass - rel_mass) / ref_mass) * 1000000

        if ((mass_diff_abs <= mass_threshold || mass_diff_ppm <= ppm_threshold) && rt_diff <= rt_threshold) {
          is_near_duplicate[i] <- TRUE
          is_near_duplicate[j] <- TRUE
          delta_ppm[i] <- mass_diff_ppm
          delta_ppm[j] <- mass_diff_ppm
        }
      }
    }

    data$Near_Duplicate <- is_near_duplicate
    data$Delta_PPM <- round(delta_ppm, 6)
    return(data)
  }


  # Render check duplicates button
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$duplicates_button <- renderUI({
        tagList(
          actionButton("check_duplicates", "Check for Duplicates"),
          br(),
          br()
        )
      })
    } else {
      output$duplicates_button <- renderUI({
        div()
      })
    }
  })

  observeEvent(input$check_duplicates, {
    req(updated_data())
    data <- updated_data()
    data$Original_Index <- seq_len(nrow(data))
    data <- pars_compounds(data)

    mass_threshold <- switch(input$mass_thresh,
                             "Exact" = 0,
                             "None" = Inf,
                             as.numeric(input$mass_thresh))

    rt_threshold <- switch(input$rt_thresh,
                           "Exact" = 0,
                           "None" = Inf,
                           as.numeric(input$rt_thresh))

    ppm_threshold <- switch(input$ppm_thresh,
                            "Exact" = 0,
                            "None" = Inf,
                            as.numeric(input$ppm_thresh))

    # Show spinner
    runjs('$("#spinner").show();')

    data_with_near_duplicates <- find_near_duplicates(data, mass_threshold, rt_threshold, ppm_threshold)

    rv$near_duplicate_data <- data_with_near_duplicates[data_with_near_duplicates$Near_Duplicate == TRUE, ]

    output$duplicate_info <- DT::renderDT({
      # Hide spinner
      runjs('$("#spinner").hide();')

      DT::datatable(rv$near_duplicate_data, options = list(ordering = FALSE, scrollX = TRUE), selection = list(target = 'column'), rownames = TRUE)
    })
  })

  # Render mass duplicates threshold
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$mass_threshold <- renderUI({
        tagList(
          hr(),
          selectInput("mass_thresh", "Set mass (Dalton) threshold:", c("Exact", "0.00001", "0.0001", "0.001", "0.01", "0.1", "None"))
        )
      })
    } else {
      output$mass_threshold <- renderUI({
        div()
      })
    }
  })

  # Render RT duplicates threshold
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$rt_threshold <- renderUI({
        selectInput("rt_thresh", "Set RT threshold:", c("Exact", "0.001", "0.01", "0.1", "None"))
      })
    } else {
      output$rt_threshold <- renderUI({
        div()
      })
    }
  })

  # Render ppm duplicates threshold
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$ppm_threshold <- renderUI({
        selectInput("ppm_thresh", "Set mass (ppm) threshold:", c("Exact", "1", "5", "10", "50", "100", "None"))
      })
    } else {
      output$ppm_threshold <- renderUI({
        div()
      })
    }
  })

  # Render check coefficient of variance button
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$dupli_cv_button <- renderUI({
        tagList(
          actionButton("calc_cv", "Calculate CV for Selected Columns"),
          br(),
          br()
        )
      })
    } else {
      output$dupli_cv_button <- renderUI({
        div()
      })
    }
  })

  observeEvent(input$calc_cv, {
    req(rv$near_duplicate_data)
    data <- rv$near_duplicate_data
    data <- select(data, -Near_Duplicate)
    selected_columns <- input$duplicate_info_columns_selected
    selected_columns <- selected_columns[selected_columns != "Original_Index"]

    if (length(selected_columns) > 0) {
      cv_values <- apply(data[, selected_columns, drop = FALSE], 1, function(row) {
        mean_val <- mean(as.numeric(row), na.rm = TRUE)
        sd_val <- sd(as.numeric(row), na.rm = TRUE)
        if (mean_val == 0 || is.nan(mean_val)) return(NA)
        return((sd_val / mean_val) * 100)
      })
      data$CV <- cv_values
    } else {
      data$CV <- rep(NA, nrow(data))
    }

    output$duplicate_info <- DT::renderDT({
      DT::datatable(data, extensions = c('FixedColumns', 'ColReorder'), options = list(ordering = FALSE, scrollX = TRUE, fixedColumns = list(leftColumns = 2, rightColumns = 0)), selection = list(target = 'row'), rownames = TRUE)
    })
  })

  # Render button to transpose data
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$transpose_button <- renderUI({
        tagList(
        br(),
        actionButton("t_button", "Transpose Data")
        )
      })
    } else {
      output$transpose_button <- renderUI({
        div()
      })
    }
  })

  # Render anomaly zero values slider
  observe({
    if (!is.null(processed_data()) && active_tab() == "Pre-process") {
      output$anom_slider <- renderUI({
        sliderInput(
          'anom_slide_value', 'Percentage of 0/NA values:',
          min = 1, max = 100, value = 90, step = 1
        )
      })
    } else {
      output$anom_slider <- renderUI({
        div()
      })
    }
  })

  # Render check other anomalies
  observe({
    if (!is.null(my_data()) && active_tab() == "Pre-process") {
      output$anomalies_button <- renderUI({
        tagList(
          actionButton("check_anomalies", "Check for anomalies"),
          hr()
        )
      })
    } else {
      output$anomalies_button <- renderUI({
        div()
      })
    }
  })

  # Function to find anomalies in data
  find_anomalies <- function(data, percent) {
    numeric_data <- data[sapply(data, is.numeric)]
    has_negative <- apply(numeric_data, 1, function(row) any(row < 0))
    mostly_zeros_nas <- apply(numeric_data, 1, function(row) mean(row == 0 | is.na(row)) > percent)

    anomaly_indices <- which(has_negative | mostly_zeros_nas)
    if(length(anomaly_indices) > 0){
      anomaly_data <- data[anomaly_indices, ]
      anomaly_data$Anomaly_Type <- ifelse(has_negative[anomaly_indices], "Negative Values", "Mostly Zeros/NAs")
      if ("Compound Name" %in% colnames(data)) {
        anomaly_data$Reference = data$`Compound Name`[anomaly_indices]
      } else {
        anomaly_data$Reference = anomaly_indices
      }

      return(anomaly_data)
    } else {
      return(NULL)
    }
  }


  observeEvent(input$check_anomalies, {
    req(updated_data(), input$anom_slide_value)
    data <- updated_data()
    percent <- as.numeric(input$anom_slide_value) * 0.01
    data$Original_Index <- seq_len(nrow(data))
    anomaly_data <- find_anomalies(data, percent)
    rv$anomaly_data <- anomaly_data

    output$anomaly_info <- DT::renderDT({
      req(rv$anomaly_data)
      DT::datatable(rv$anomaly_data, options = list(ordering = TRUE, scrollX = TRUE),
                    selection = 'none', rownames = FALSE)
    })
  })


  # Render custom title
    observe({
    title_value(input$title_value)
  })

  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Ranges", "% of change", "Fold change", "Distribution plots", "Grubbs test", "Outlier detection")) {
      output$custom_title <- renderUI({
        tagList(
          hr(),
          textInput("title_value", "Custom title:")
        )
      })
    } else {
      output$custom_title <- renderUI({
        div()
      })
    }
  })

  observe({
    title_value(input$title_value)
  })

  # Render plot height slider
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Ranges", "% of change", "Fold change", "Distribution plots", "Grubbs test")) {
      output$plot_height <- renderUI({
        sliderInput(
          'height_value', 'Adjust plot height',
          min = 200, max = 1000, value = 600, step = 1
        )
      })
    } else {
      output$plot_height <- renderUI({
        div()
      })
    }
  })


  # Render nudge selector
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Ranges", "% of change", "Fold change", "Grubbs test", "Outlier detection")) {
      output$nudge_selector <- renderUI({
        sliderInput(
          'nudge_value', 'Adjust position',
          min = -0.5, max = 0, value = -0.25, step = 0.01
        )
      })
    } else {
      output$nudge_selector <- renderUI({
        div()
      })
    }
  })

  # Render distplot x lab slider
  observe({
    if (!is.null(processed_data()) && active_tab() == "Distribution plots") {
      output$dist_xlab <- renderUI({
        sliderInput(
          'xlab_value', 'Adjust x lab position',
          min = -10.0, max = 10.0, value = 0, step = 0.1
        )
      })
    } else {
      output$dist_xlab <- renderUI({
        div()
      })
    }
  })

  # Render distplot y lab slider
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("% of change", "Distribution plots")) {
      output$dist_ylab <- renderUI({
        sliderInput(
          'ylab_value', 'Adjust y lab position',
          min = -10.0, max = 10.0, value = 0, step = 0.1
        )
      })
    } else {
      output$dist_ylab <- renderUI({
        div()
      })
    }
  })

  # Render mean_point checkbox
  observe({
    if (!is.null(processed_data()) && active_tab() == "Ranges") {
      output$mean_point <- renderUI({
        checkboxInput('checkbox_mean_point', 'Add mean point', value = FALSE)
      })
    } else {
      output$mean_point <- renderUI({
        div()
      })
    }
  })

  # Render color-blind palette checkbox
  observe({
    if (!is.null(processed_data()) && active_tab() == "Ranges") {
      output$color_blind <- renderUI({
        checkboxInput('checkbox_blind', 'Color-blind friendly palette', value = FALSE)
      })
    } else {
      output$color_blind <- renderUI({
        div()
      })
    }
  })

  #  Render ranges criterium dropdown
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Ranges", "Distribution plots") && nrow(processed_data()) > 30) {
      output$ranges_crit_type <- renderUI({
        selectInput("top_ranges_crit", "Select top ranges criterium:", c("Top MAX/MIN Amplitude", "Statistically significant (parametric)", "Statistically significant (non-parametric)", "Top RF Gini Index"))
      })
    } else {
      output$ranges_crit_type <- renderUI({
        div()
      })
    }
  })

 # Render scale checkbox
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("% of change", "Fold change", "Distribution plots")) {
      output$checkbox_scale_y <- renderUI({
        checkboxInput('checkbox_scale_value', 'Fixed scale', value = FALSE)
      })
    } else {
      output$checkbox_scale_y <- renderUI({
        div()
      })
    }
  })

  #  Render log checkbox
  observe({
    if (!is.null(processed_data()) && active_tab() == "Fold change") {
      output$checkbox_log2 <- renderUI({
        checkboxInput('checkbox_fold2log', 'Log2 fold', value = FALSE)
      })
    } else {
      output$checkbox_log2 <- renderUI({
        div()
      })
    }
  })

  #  Render distplot dropdown
  observe({
    if (!is.null(processed_data()) && active_tab() == "Distribution plots") {
      output$dropdown_type <- renderUI({
        selectInput("dist_plot_type", "Type of distribution plot:", c("Box", "Violin", "Smooth"))
      })
    } else {
      output$dropdown_type <- renderUI({
        div()
      })
    }
  })

  # Render Grubbs transform check
  observe({
    if (!is.null(processed_data()) && active_tab() == "Grubbs test") {
      output$grubbs_scale <- renderUI({
        selectInput("checkbox_scale", "Select type of transformation:", c("None", "sqrt normalization", "log normalization", "Yeo-Johnson", "Ordered Quantile normalization"))
        #checkboxInput('checkbox_scale', 'Perform Yeo-Johnson transformation', value = FALSE)
      })
    } else {
      output$grubbs_scale <- renderUI({
        div()
      })
    }
  })

  # Render Grubbs test dropdown
  observe({
    if (!is.null(processed_data()) && active_tab() == "Grubbs test") {
      output$grubbs_drop <- renderUI({
        selectInput("test_type_dropdown", "Select Grubbs test type:", c("Max", "Second Max", "Third Max", "Fourth Max", "Fifth Max", "Min"))
      })
    } else {
      output$grubbs_drop <- renderUI({
        div()
      })
    }
  })

  # Render PCA search dropdown
  observe({
    if (!is.null(processed_data()) && active_tab() == "Outlier detection") {
      options <- if (nrow(processed_data()) < 30) {
        c("Row", "Column")
      } else {
        c("Row", "Column")
      }

      output$pca_drop <- renderUI({
        selectInput("pca_dropdown", "Search outliers by:", choices = options)
      })
    } else {
      output$pca_drop <- renderUI({
        div()
      })
    }
  })

  # Render detection method
  observe({
    if (!is.null(processed_data()) && active_tab() == "Outlier detection") {
      output$pca_method_drop <- renderUI({
        selectInput("method_dropdown", "Method:", c("PCA", "PLS"))
      })
    } else {
      output$pca_method_drop <- renderUI({
        div()
      })
    }
  })

  # Render PCA distance
  observe({
    if (!is.null(processed_data()) && active_tab() == "Outlier detection") {
      output$pca_dist_drop <- renderUI({
        selectInput("distance_dropdown", "Statistic:", c("Euclidean distance", "Mahalanobis distance", "Hotelling T2"))
      })
    } else {
      output$pca_dist_drop <- renderUI({
        div()
      })
    }
  })

  # Render points of interest
  observe({
    req(input$pca_dropdown)
    if (!is.null(processed_data()) && active_tab() == "Outlier detection" && input$pca_dropdown == "Row") {
      output$pca_interest_check <- renderUI({
        div(
          style = "display: flex; align-items: start;",
          checkboxInput('interest_check', 'Show points of interest', value = FALSE),
          span(
            "?",
            style = "cursor: pointer; color: blue; margin-left: 5px; vertical-align: baseline;",
            `data-toggle` = "tooltip",
            title = "Scores compounds based on the sum of scaled mean and scaled RSD."
          )
        )
      })
    } else {
      output$pca_interest_check <- renderUI({
        div()
      })
    }
  })

  # Render PCA criterion dropdown
  observe({
    if (!is.null(processed_data()) && active_tab() == "Outlier detection") {
      output$pca_criterion_drop <- renderUI({
        selectInput("criterion_dropdown", "Define outlier criterion:", c("Quantile", "Mean + Sd"))
      })
    } else {
      output$pca_criterion_drop <- renderUI({
        div()
      })
    }
  })

  # Render quantile threshold
  observe({
    req(input$criterion_dropdown)
    if (!is.null(processed_data()) && active_tab() == "Outlier detection" && input$criterion_dropdown == "Quantile" && nrow(processed_data()) < 30) {
      output$quant_val <- renderUI({
        numericInput("quant", "Quantile threshold:", 0.95, min = 0.9, max = 0.99, step = 0.01)
      })
    } else if (!is.null(processed_data()) && active_tab() == "Outlier detection" && input$criterion_dropdown == "Quantile" && nrow(processed_data()) >= 30) {
      output$quant_val <- renderUI({
        numericInput("quant", "Quantile threshold:", 0.99, min = 0.9, max = 0.999, step = 0.001)
      })
    } else {
      output$quant_val <- renderUI({
        div()
      })
    }
  })

  # Render sd multiplier
  observe({
    req(input$criterion_dropdown)
    if (!is.null(processed_data()) && active_tab() == "Outlier detection" && input$criterion_dropdown == "Mean + Sd") {
      output$sd_val <- renderUI({
        numericInput("sd_n", "Sd multiplier:", 6, min = 2, max = 10)
      })
    } else {
      output$sd_val <- renderUI({
        div()
      })
    }
  })


  #  Render selector dropdown
  observe({
    if (!is.null(processed_data()) && active_tab() == "Grubbs test") {
      output$dropdown_comp <- renderUI({
        selectInput("selector_dropdown", "Select a compound:", c(unique(processed_data()[[common_column()]])))
      })
    } else {
      output$dropdown_comp <- renderUI({
        div()
      })
    }
  })

  # Render top compound selector for Ranges and Distplot
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Ranges", "Distribution plots") && nrow(processed_data()) > 30) {
      output$top_compound_real_selector <- renderUI({
        checkboxGroupInput(
          'top_compound_range', 'Select compound',
          choices = top_compounds_reactive(),
          selected = top_compounds_reactive()
          )
      })
    } else {
      output$top_compound_real_selector <- renderUI({
        div()
      })
    }
  })


  # Render top compounds sorter for Ranges and Distplot
  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Ranges", "Distribution plots") && nrow(processed_data()) > 30) {
      output$top_compounds_selector <- renderUI({
        compound_choices <- top_compounds_reactive()
        if (length(compound_choices) > 0) {
          rank_list(
            text = "Drag the items in any desired order",
            labels = compound_choices,
            input_id = "rank_list_top"
          )
        } else {
          div()
        }
      })
    } else {
      output$top_compounds_selector <- renderUI({
        div()
      })
    }
  })

  # Render compound selector
  output$compound_selector <- renderUI({
    if (!is.null(processed_data()) && active_tab() %in% c("Table", "Ranges", "% of change", "Fold change", "Distribution plots")) {
      checkboxGroupInput(
        'compound_range', 'Select compound',
        choices = unique(processed_data()[[common_column()]]),
        selected = unique(processed_data()[[common_column()]])
      )
    } else if (nrow(processed_data()) > 30) {
      output$compound_selector <- renderUI({
        div()
      })
    }
  })

  # Render compound sorter
  output$compound_sorter <- renderUI({
    if (!is.null(processed_data()) && active_tab() %in% c("Table", "Ranges", "% of change", "Fold change", "Distribution plots")) {
      selected_compounds <- processed_data()[[common_column()]]
      selected_compounds <- selected_compounds[selected_compounds %in% input$compound_range]

      rank_list(
        text = "Drag the items in any desired order",
        labels = selected_compounds,
        input_id = "rank_list_basic"
      )
    } else if (nrow(processed_data()) > 30) {
      output$compound_sorter <- renderUI({
        div()
      })
    }
  })

  # Render pca compound selector
  observe({
  req(input$pca_dropdown)
    if (!is.null(processed_data()) && active_tab() == "Outlier detection" && input$pca_dropdown == "Row" && nrow(processed_data()) < 30) {
      output$pca_compound_selector <- renderUI({
        checkboxGroupInput(
          'pca_compound_range', 'Select compound',
          choices = unique(processed_data()[[common_column()]]),
          selected = unique(processed_data()[[common_column()]])
        )
       })
    } else {
      output$pca_compound_selector <- renderUI({
        div()
      })
    }
  })

  # Render sample selector
  observe({
  req(input$pca_dropdown)
    if (!is.null(processed_data()) && active_tab() == "Outlier detection" && input$pca_dropdown == "Column" && nrow(processed_data()) < 30) {
      output$sample_selector <- renderUI({
        checkboxGroupInput(
          'pca_sample_range', 'Select sample',
          choices = colnames(processed_data())[-1],
          selected = colnames(processed_data())[-1]
        )
      })
    } else {
      output$sample_selector <- renderUI({
        div()
      })
    }
  })

  # Render range seed setter
  observe({
    req(input$top_ranges_crit)
    if (!is.null(processed_data()) && active_tab() == "Ranges" && input$top_ranges_crit == "Top RF Gini Index") {
      output$range_seed_setter <- renderUI({
        numericInput("range_set_seed", "Set seed:", 1, min = 1, max = 10000, step = 1)
      })
    } else {
      output$range_seed_setter <- renderUI({
        div()
      })
    }
  })

  # Render seed setter
  observe({
    if (!is.null(processed_data()) && active_tab() == "RF Gini index") {
      output$seed_setter <- renderUI({
        numericInput("set_seed", "Set seed:", 1, min = 1, max = 10000, step = 1)
      })
    } else {
      output$seed_setter <- renderUI({
        div()
      })
    }
  })


  # Render range num of tree input
  observe({
    req(input$top_ranges_crit)
    if (!is.null(processed_data()) && active_tab() == "Ranges" && input$top_ranges_crit == "Top RF Gini Index") {
      output$range_tree_setter <- renderUI({
        numericInput("range_tree_num", "Number of trees:", 2000, min = 100, max = 10000, step = 100)
      })
    } else {
      output$range_tree_setter <- renderUI({
        div()
      })
    }
  })

  # Render num of tree input
  observe({
    if (!is.null(processed_data()) && active_tab()  == "RF Gini index") {
      output$tree_setter <- renderUI({
        numericInput("tree_num", "Number of trees:", 2000, min = 100, max = 10000, step = 100)
      })
    } else {
      output$tree_setter <- renderUI({
        div()
      })
    }
  })

  # Page buttons
  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$prev_page_button <- renderUI({
        tagList(br(),
          actionButton("prev_page", "Previous 50 Compounds"),
          br()
        )
      })
    } else {
      output$prev_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$next_page_button <- renderUI({
        tagList(
          actionButton("next_page", "Next 50 Compounds")
        )
      })
    } else {
      output$next_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$page_info_text <- renderUI({
        tagList(textOutput("page_info"),
                br()
        )
      })
    } else {
      output$page_info_text <- renderUI({
        div()
      })
    }
  })

  # Render check normality (Shapiro-Wilk test), homoscedasticity (Levene's test) and independence (Pearson for normal distribution/Spearman's Rank Correlation for non-normal) check
  # observe({
  #   if (!is.null(processed_data()) && active_tab() == "Normality") {
  #     output$normality_button <- renderUI({
  #       tagList(
  #         hr(),
  #         actionButton("check_normality", "Check normality")
  #       )
  #     })
  #   } else {
  #     output$normality_button <- renderUI({
  #       div()
  #     })
  #   }
  # })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$norm_group <- renderUI({
        selectInput("normality_grouping", "Perform test on:", c("whole data", "grouped data"))
      })
    } else {
      output$norm_group <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$norm_sort <- renderUI({
        selectInput("normality_sort", "Sort data:", c("None", "Min", "Max"))
      })
    } else {
      output$norm_sort <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$norm_norm <- renderUI({
        selectInput("normality_norm", "Normalize samples:", c("None", "by Sum", "by Median", "Quantile"))
      })
    } else {
      output$norm_norm <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$norm_transf <- renderUI({
        selectInput("normality_trans", "Transform data:", c("None", "Log(5)", "Log(10)", "Square root", "Cube root", "Square", "Cube"))
      })
    } else {
      output$norm_transf <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$norm_scale <- renderUI({
        selectInput("normality_scale", "Scale data:", c("None", "Center on Mean", "Auto"))
      })
    } else {
      output$norm_scale <- renderUI({
        div()
      })
    }
  })


  # equal variances
  # observe({
  #   if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
  #     output$vareq_button <- renderUI({
  #       tagList(
  #         br(),
  #         actionButton("check_homoscedasticity", "Check equality of variances")
  #       )
  #
  #     })
  #   } else {
  #     output$vareq_button <- renderUI({
  #       div()
  #     })
  #   }
  # })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$homo_sort <- renderUI({
        selectInput("homoscedas_sort", "Sort data:", c("None", "Min", "Max"))
      })
    } else {
      output$homo_sort <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$homo_norm <- renderUI({
        selectInput("homoscedas_norm", "Normalize data:", c("None", "by Sum", "by Median", "Quantile"))
      })
    } else {
      output$homo_norm <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$homo_transf <- renderUI({
        selectInput("homoscedas_trans", "Transform data:", c("None", "Log(5)", "Log(10)", "Square root", "Cube root", "Square", "Cube"))
      })
    } else {
      output$homo_transf <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$homo_scale <- renderUI({
        selectInput("homoscedas_scale", "Scale data:", c("None", "Center on Mean", "Auto"))
      })
    } else {
      output$homo_scale <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$checkbox_swarm <- renderUI({
        checkboxInput('swarm_check', 'Bean/Beeswarm Plot', value = FALSE)
      })
    } else {
      output$checkbox_swarm <- renderUI({
        div()
      })
    }
  })


  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$parametric_select <- renderUI({
        selectInput("para_select", "Choose test:", c("t-test", "ANOVA"))
      })
    } else {
      output$parametric_select <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Parametric tests", "Non-Parametric tests")) {
      output$checkbox_fdr <- renderUI({
        checkboxInput('fdr_check', 'FDR', value = FALSE)
      })
    } else {
      output$checkbox_fdr <- renderUI({
        div()
      })
    }
  })

  corrections <- c("None", "Benjamini & Hochberg", "Bonferroni")

  observe({
    if (!is.null(processed_data()) && active_tab() %in% c("Parametric tests", "Non-Parametric tests")) {
      output$corrections_radio <- renderUI({
        radioButtons("correction_check", "Apply correction:", corrections)
      })
    } else {
      output$corrections_radio <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_parametric_select <- renderUI({
        selectInput("non_para_select", "Choose test:", c("U Test", "Kruskal-Wallis test"))
      })
    } else {
      output$non_parametric_select <- renderUI({
        div()
      })
    }
  })

# Pre-process -------------------

  selected_columns <- reactiveVal(NULL)
  modified_columns <- reactiveVal(NULL)

  # Render pre-process
  pre_process_table <- reactive({
    req(updated_data())
    data_df <- updated_data()

    # if (input$t_button) {
    #   t_data_df <- t(data_df)
    #   new_names <- t(data_df[,1])
    #   print(new_names)
    #   rownames(t_data_df) <- colnames(data_df)
    #   colnames(t_data_df) <- new_names
    #   data_df <- t_data_df
    #   transposed_table_stored(data_df)
    #   data_df
    # } else {
    #   data_df <- data_df
    # }

    if (input$deselect_all > 0) {
      DT::datatable(data_df, options = list(ordering = FALSE, scrollX = TRUE), selection = list(target = 'column', selected = NULL))
    } else {
      DT::datatable(data_df, options = list(ordering = FALSE, scrollX = TRUE), selection = list(target = 'column'))
    }
  })


  output$process_data = DT::renderDataTable(
    pre_process_table())


  observeEvent(input$process_data_columns_selected, {
    selected_columns(input$process_data_columns_selected)
  })

  # Adjust the Display of Modified Indices
  observe({
    lapply(1:group_counter(), function(i) {
      output[[paste0('mod_indices_', i)]] <- renderPrint({
        modified_cols_list <- selected_columns_by_group()
        if (is.list(modified_cols_list) && length(modified_cols_list) >= i && !is.null(modified_cols_list[[i]])) {
          modified_cols <- modified_cols_list[[i]]
          if (length(modified_cols) > 0) {
            cat('Group', i, 'columns:\n\n')
            cat(modified_cols, sep = ', ')
          }
        } else {
          cat('Group', i, 'has no selected columns yet.')
        }
      })
    })
  })

  processed_data <- reactive({
    original_data_df <- req(updated_data())
    new_data_df <- original_data_df[, 1, drop = FALSE]
    selected_cols_list <- req(selected_columns_by_group())

    # Loop through each group and add its selected columns to the new data frame
    for (i in seq_along(selected_cols_list)) {
      cols_for_group <- selected_cols_list[[i]]
      if (!is.null(cols_for_group) && length(cols_for_group) > 0) {
        if (all(cols_for_group %in% names(original_data_df))) {
          new_group_cols <- original_data_df[, cols_for_group, drop = FALSE]
          colnames(new_group_cols) <- paste0("group", i, "_", colnames(new_group_cols))
          new_data_df <- cbind(new_data_df, new_group_cols)
        }
      }
    }
    new_data_df[, -1] <- lapply(new_data_df[, -1], as.numeric)
    return(new_data_df)
  })

  coefficient_of_variance <- function(x) {
    sd(x) / mean(x)
  }

  processed_data_with_stats <- reactive({
    processed_data <- req(processed_data())
    processed_data_with_stats <- processed_data %>%
      rowwise() %>%
      mutate(
        Min = min(c_across(-1), na.rm = TRUE),
        Max = max(c_across(-1), na.rm = TRUE),
        Mean = mean(c_across(-1), na.rm = TRUE),
        CV = coefficient_of_variance(c_across(-1))
      ) %>%
      ungroup()

    processed_data_with_stats <- processed_data_with_stats %>%
      mutate(
        Standardized_Mean = scale(Mean),
        Standardized_CV = scale(CV)
      )

    processed_data_with_stats <- processed_data_with_stats %>%
      mutate(
        Combined_Score = Standardized_Mean + Standardized_CV
      )

    processed_data_with_stats
  })


  # Render the data table --------------

  observe({
    if (!is.null(processed_data()) && active_tab() == "Table") {
      output$min_max_cols_check <- renderUI({
        checkboxInput('mx_cols_check', 'Add min/max columns', value = FALSE)
      })
    } else {
      output$min_max_cols_check <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Table") {
      output$add_mean_col_check <- renderUI({
        checkboxInput('mean_col_check', 'Add mean column', value = FALSE)
      })
    } else {
      output$add_mean_col_check <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Table") {
      output$coeff_of_var_col_check <- renderUI({
        checkboxInput('cv_col_check', 'Add CV column', value = FALSE)
      })
    } else {
      output$coeff_of_var_col_check <- renderUI({
        div()
      })
    }
  })

  table_stored <- reactiveVal(NULL)

  observe({
    if (!is.null(processed_data()) && active_tab() == "Table") {
      output$table_download <- renderUI({
        tagList(
          br(),
          tags$p("Download your processed data"),
          downloadHandler(
            filename = function() {
              current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
              paste0("metabo_table_", current_datetime, ".csv")
            },
            content = function(file) {
              write.csv(table_stored(), file)
            }
          ))
        })
    } else {
      output$table_download <- renderUI({
        div()
      })
    }
  })

  output$data_table <- DT::renderDT({

    data_df <- req(processed_data())
    data_df_stats <- req(processed_data_with_stats())

    mx_cols <- input$mx_cols_check
    mean_col <- input$mean_col_check
    cv_col <- input$cv_col_check

    update_mx <- function(data_df, data_df_stats, mx_cols) {
      if (mx_cols) {
        if (!all(c("Min", "Max") %in% colnames(data_df))) {
          data_df <- cbind(data_df, data_df_stats[, c("Min", "Max")])
        }
      } else {
        data_df <- data_df[, !colnames(data_df) %in% c("Min", "Max")]
      }
      return(data_df)
    }

    update_mean <- function(data_df, data_df_stats, mean_col) {
      if (mean_col) {
        if (!all(c("Mean") %in% colnames(data_df))) {
          data_df <- cbind(data_df, data_df_stats[, c("Mean")])
        }
      } else {
        data_df <- data_df[, !colnames(data_df) %in% c("Mean")]
      }
      return(data_df)
    }

    update_cv <- function(data_df, data_df_stats, cv_col) {
      if (cv_col) {
        if (!all(c("CV") %in% colnames(data_df))) {
          data_df <- cbind(data_df, data_df_stats[, c("CV")])
        }
      } else {
        data_df <- data_df[, !colnames(data_df) %in% c("CV")]
      }
      return(data_df)
    }

    data_df <- update_mx(data_df, data_df_stats, mx_cols)
    data_df <- update_mean(data_df, data_df_stats, mean_col)
    data_df <- update_cv(data_df, data_df_stats, cv_col)

    if(nrow(data_df) < 30) {
      data_df <- data_df %>%
        filter(.data[[common_column()]] %in% input$compound_range)
      data_df <- data_df[order(match(data_df[[common_column()]], input$rank_list_basic)), ]
      table_stored(data_df)
    } else {
      data_df <- data_df
      table_stored(data_df)
    }

    DT::datatable(data_df,
                  extensions = c('FixedColumns', 'ColReorder'),
                  #filter = 'top',
                  options = list(ordering = FALSE,
                                 #dom = 't',
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 2, rightColumns = 0),
                                 colReorder = TRUE
                                 #autoWidth = TRUE
                                 ))
  })


  top_compounds_reactive <- reactiveVal(NULL)

  # Render ranges plot
  plot_lineranges <- reactive({
    req(processed_data())
    height_value <- input$height_value
    nudge_value <- input$nudge_value
    mean_check_value <- input$checkbox_mean_point
    blind <- input$checkbox_blind
    top_criterium <- input$top_ranges_crit
    real_data_merged <- processed_data()
    common_column_value <- common_column()
    top_compound_values <- input$top_compound_range
    long_data <- real_data_merged %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "_")

    summary_data <- long_data %>%
      group_by(.data[[common_column_value]], group) %>%
      dplyr::summarize(
        min_value = min(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        mean_value = mean(value, na.rm = TRUE),
        max_minus_min = max_value - min_value
      ) %>%
      ungroup()

    if(nrow(real_data_merged) <= 30) {
      summary_data <- summary_data %>%
        filter(.data[[common_column()]] %in% input$compound_range)
      summary_data <- summary_data[order(match(summary_data[[common_column()]], input$rank_list_basic)), ]
    } else {
      # Filtering based on selected criterium
      if (top_criterium == "Top MAX/MIN Amplitude") {
        top_compounds <- summary_data %>%
          group_by(.data[[common_column_value]]) %>%
          dplyr::summarize(max_max_minus_min = max(max_minus_min, na.rm = TRUE)) %>%
          ungroup() %>%
          arrange(desc(max_max_minus_min)) %>%
          slice_max(order_by = max_max_minus_min, n = 30) %>%
          select(.data[[common_column_value]])

      } else if (top_criterium == "Statistically significant (parametric)") {
        short_data <- processed_data()
        long_data <- short_data %>%
          pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
          separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

        num_groups <- long_data %>%
          distinct(group) %>%
          nrow()

        if (num_groups > 2) {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = anova(lm(value ~ group))$"Pr(>F)"[1]) %>%
            arrange(p_value)
        } else {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = t.test(value ~ group)$p.value) %>%
            arrange(p_value)
        }

        results_df <- data.frame(results)
        results_df$p_value <- as.numeric(results_df$p_value)

        top_compounds <- results_df %>%
          filter(p_value < 0.05) %>%
          select(-c(2))
        colnames(top_compounds)[1] = common_column_value

      } else if (top_criterium == "Statistically significant (non-parametric)") {
        short_data <- processed_data()
        long_data <- short_data %>%
          pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
          separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

        num_groups <- long_data %>%
          distinct(group) %>%
          nrow()

        if (num_groups > 2) {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = kruskal.test(value ~ group)$p.value) %>%
            arrange(p_value)
        } else {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = wilcox.test(value ~ group)$p.value) %>%
            arrange(p_value)
        }

        results_df <- data.frame(results)
        results_df$p_value <- as.numeric(results_df$p_value)

        top_compounds <- results_df %>%
          filter(p_value < 0.05) %>%
          select(-c(2))
        colnames(top_compounds)[1] = common_column_value

      } else if (top_criterium == "Top RF Gini Index") {
        gini_data <- processed_data()
        new_row <- rep(NA, ncol(gini_data))
        for (i in 2:ncol(gini_data)) {
          prefix <- strsplit(colnames(gini_data)[i], "_")[[1]][1]
          new_row[i] <- sub("group([1-9][0-9]*)", "group\\1", prefix)
          if (prefix == "group1") {
            new_row[i] <- "control_group"
          }
        }
        new_row[1] <- "Label"
        new_df <- rbind(new_row, gini_data)
        rownames(new_df) <- NULL
        t_data <- t(new_df)
        colnames(t_data) <- t_data[1,]
        data <- t_data[-1,]
        data[,2:ncol(data)] <- sapply(data[,2:ncol(data)],as.numeric)

        modified_colnames <- paste0('X', gsub("[.@]", "_", colnames(data)[-1]))
        original_colnames <- colnames(data)[-1]
        name_mapping <- setNames(modified_colnames, original_colnames)
        inverted_name_mapping <- setNames(names(name_mapping), name_mapping)

        colnames(data) <- str_replace_all(colnames(data), "[[:punct:]]", " ")
        colnames(data) <- gsub(" ", "_", colnames(data))
        colnames(data) <- paste0('X', colnames(data))
        set.seed(input$range_set_seed %||% 1)
        model <- randomForest(as.factor(XLabel) ~ ., data = data,
                              ntree = input$range_tree_num %||% 2000, mtry = 2,
                              importance = TRUE)
        results <- data.frame(Gini=sort(importance(model, type=2)[,], decreasing=T))
        results <- data.frame(rownames(results), results, row.names = NULL)
        colnames(results)[1] <- common_column_value
        top_compounds <- head(results, 30)

        if (!is.null(top_compounds[[common_column_value]])) {
          mapped_names <- sapply(as.character(top_compounds[[common_column_value]]), function(x) inverted_name_mapping[x])
          top_compounds[[common_column_value]] <- mapped_names
        }

        top_compounds
      }
      top_compounds_reactive(top_compounds[[common_column_value]])
      summary_data <- summary_data %>%
        filter(.data[[common_column_value]] %in% top_compounds[[common_column_value]])

      summary_data <- summary_data[order(match(summary_data[[common_column()]], input$rank_list_top)), ]

      summary_data <- summary_data %>%
        filter(.data[[common_column_value]] %in% top_compound_values)
    }

    num_groups <- length(unique(summary_data$group))
    equal_spacing_nudge <- nudge_value * (num_groups - 1) / 2

    plot <- ggplot()

    for (i in seq_along(unique(summary_data$group))) {
      position_y <- (i - 1) * nudge_value - equal_spacing_nudge
      plot <- plot +
        geom_linerange(
          data = summary_data[summary_data$group == unique(summary_data$group)[i], ],
          aes(
            y = .data[[common_column()]],
            xmin = min_value,
            xmax = max_value,
            color = factor(group)
          ),
          linetype = 1,
          position = position_nudge(y = position_y)
        )

      if (mean_check_value) {
        plot <- plot +
          geom_point(
            data = summary_data[summary_data$group == unique(summary_data$group)[i], ],
            aes(
              y = .data[[common_column()]],
              x = mean_value,
              color = factor(group)
            ),
            position = position_nudge(y = position_y)
          )
      }
    }

    range_y_limits <- if (nrow(real_data_merged) < 30) {
      rev(input$rank_list_basic)
    } else {
      unique_compounds_after_filtering <- unique(summary_data[[common_column()]])
      rev(unique_compounds_after_filtering)
    }

    my_colors <- c("#F84F44", "#19a8c5", "#E69F00", "#1B9E77", "#7B3294","#0000c2", "#ff8fc2", "#F0E442", "#6f1c00", "#126200")
    blind_colors <- c("#882255", "#0072B2", "#E69F00", "#009E73", "#CC79A7", "#F0E442", "#D55E00", "#56B4E9", "#999933", "#661100")

    if (blind) {
      chosen_palette <- blind_colors
    } else {
      chosen_palette <- my_colors
    }

    plot <- plot +
      labs(x = "Range", y = "Compounds") +
      scale_color_manual(name = "Groups", values = chosen_palette) +
      #scale_color_manual(name = "Groups", values = rainbow(length(unique(summary_data$group)))) +
      ggtitle(title_value()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_discrete(limits = range_y_limits)

    ggplotly(plot, height = height_value)
   })

  # output$plot_data <- DT::renderDT(
  #   DT::datatable(
  #   plot_lineranges()
  #  ))

  output$plot_data <- plotly::renderPlotly({
    req(processed_data())
    req(!is.null(input$checkbox_mean_point))
    plot_lineranges()
  })

  # Render % of change plot
  plot_changes <- reactive({
    req(processed_data())
    common_column_value <- common_column()
    height_value <- input$height_value
    nudge_value <- input$nudge_value
    scale_check <- input$checkbox_scale_value
    real_data_merged <- processed_data()

    long_data <- real_data_merged %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "_")
    summary_data <- long_data %>%
      group_by(.data[[common_column()]], group) %>%
      dplyr::summarize(
        avg_value = mean(value, na.rm = TRUE)
      )

    if(nrow(real_data_merged) < 30) {
      summary_data <- summary_data %>%
        filter(.data[[common_column()]] %in% input$compound_range)
    }

    percent_change_data <- merge(
      summary_data %>% filter(group == "group1") %>% select(.data[[common_column()]], avg_value),
      summary_data %>% filter(group != "group1") %>% select(.data[[common_column()]], avg_value, group),
      by = common_column_value,
      suffixes = c("_control", "_test")
    ) %>%
      mutate(
        percent_change = ((avg_value_test - avg_value_control) / avg_value_control) * 100
      ) %>%
      mutate(
        control_mean = avg_value_control
      ) %>%
      select(.data[[common_column()]], control_mean, group, percent_change)

    percent_change_data <- percent_change_data[percent_change_data$control_mean != 0, ]

    if (nrow(real_data_merged) > 25) {

      # Sort the data within each group separately to get highest and lowest values
      percent_change_data <- percent_change_data %>%
        group_by(group) %>%
        arrange(desc(percent_change)) %>%
        slice(c(1:25, (n() - 24):n())) %>%
        ungroup()

      percent_change_data <- percent_change_data %>%
        mutate(ordered_compound_names = reorder(!!sym(common_column()), percent_change))

      plot <- ggplot(percent_change_data, aes(x = ordered_compound_names, y = percent_change)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Compound", y = "Percent Change") +
        ggtitle(title_value()) +
        facet_wrap(~group, scales = "free_y", ncol = 1) +
        coord_flip() +
        theme(axis.title.y = element_text(margin = ggplot2::margin(r = 10 + input$ylab_value)),
              plot.title = element_text(hjust = 0.5))

      ggplotly(plot, height = height_value)


    } else {
      percent_change_data <- spread(percent_change_data, key = "group", value = "percent_change")
      if(nrow(real_data_merged) < 30) {
        percent_change_data <- percent_change_data[order(match(percent_change_data[[common_column()]], input$rank_list_basic)), ]
      }

      plot <- ggplot(gather(percent_change_data, key = "Group", value = "PercentChange", -.data[[common_column()]], -control_mean),
             aes(x = .data[[common_column()]], y = PercentChange, fill = ifelse(PercentChange >= 0, "red", "green"))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_text(aes(label = sprintf("%.1f", PercentChange),
                      y = ifelse(PercentChange >= 0, PercentChange - (nudge_value*4), PercentChange + (nudge_value*4))),
                  position = position_dodge(width = 0.9), vjust = 0, size = 3) +
        labs(x = "Compound", y = "Percent Change") +
        ggtitle(title_value()) +
        scale_x_discrete(limits = input$rank_list_basic) +
        facet_wrap(~Group, scales = if (scale_check == FALSE) "free_y" else "fixed", ncol = 1) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_text(margin = ggplot2::margin(t = 20)),
              axis.title.y = element_text(margin = ggplot2::margin(r = 10 + input$ylab_value)),
              plot.title = element_text(hjust = 0.5),
              panel.spacing = unit(1.5, "lines")
        ) +
        guides(fill = FALSE)
      ggplotly(plot, height = height_value)
    }

  })

  output$change_data <- plotly::renderPlotly({
    req(processed_data())
    req(!is.null(input$checkbox_scale_value))
    plot_changes()
  })

  # output$change_data <- DT::renderDT({
  #   req(merged_data())
  #   plot_changes()
  # })

  # Render fold change plot
  plot_fold <- reactive({
    req(processed_data())
    common_column_value <- common_column()
    height_value <- input$height_value
    nudge_value <- input$nudge_value
    scale_check <- input$checkbox_scale_value
    log_check <- input$checkbox_fold2log
    real_data_merged <- processed_data()
    long_data <- real_data_merged %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "_")
    summary_data <- long_data %>%
      group_by(.data[[common_column()]], group) %>%
      dplyr::summarize(
        avg_value = mean(value, na.rm = TRUE)
      ) %>%
      ungroup()

    if(nrow(real_data_merged) < 30) {
      summary_data <- summary_data %>%
        filter(.data[[common_column()]] %in% input$compound_range)
    }

    fold_change_data <- merge(
      summary_data %>% filter(group == "group1") %>% select(.data[[common_column()]], avg_value),
      summary_data %>% filter(group != "group1") %>% select(.data[[common_column()]], avg_value, group),
      by = common_column_value,
      suffixes = c("_control", "_test")
    ) %>%
      mutate(
        control_mean = avg_value_control,
        fold_change = if (log_check == FALSE) abs(foldchange(avg_value_control, avg_value_test)) else foldchange2logratio(abs(foldchange(avg_value_control, avg_value_test)))
      ) %>%
      select(.data[[common_column()]], control_mean, group, fold_change)

    fold_change_data <- fold_change_data[fold_change_data$control_mean != 0, ]

    if (nrow(real_data_merged) > 25) {
      fold_change_data <- fold_change_data[order(-fold_change_data$fold_change), ][1:25, ]
    } else {
      fold_change_data <- spread(fold_change_data, key = "group", value = "fold_change")
    }

    fold_change_data <- fold_change_data[order(match(fold_change_data[[common_column()]], input$rank_list_basic)), ]

    if (nrow(real_data_merged) > 25) {

      fold_change_data <- fold_change_data %>%
        arrange(desc(fold_change)) %>%
        mutate(ordered_compound_names = reorder(!!sym(common_column()), fold_change))

      plot <- ggplot(fold_change_data, aes(x = ordered_compound_names, y = fold_change, fill = group)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = round(fold_change, 2)), vjust = 0, size = 3) +
        labs(x = common_column_value, y = if (log_check == FALSE) "Fold change" else "Log2 fold change") +
        coord_flip()
      ggplotly(plot, height = height_value)
    } else {
        plot <- ggplot(gather(fold_change_data, key = "Group", value = "FoldChange", -.data[[common_column()]], -control_mean),
           aes(x = .data[[common_column()]], y = FoldChange)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_text(aes(label = round(FoldChange, 2), y = FoldChange + abs(nudge_value/4)),
                position = position_dodge(width = 0.9), vjust = 0, size = 3) +
          labs(x = common_column_value, y = if (log_check == FALSE) "Fold change" else "Log2 fold change") +
          ggtitle(title_value()) +
          scale_x_discrete(limits = input$rank_list_basic) +
          facet_wrap(~Group, scales = if (scale_check == FALSE) "free_y" else "fixed", ncol = 1) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_text(margin = ggplot2::margin(t = 20)),
            plot.title = element_text(hjust = 0.5),
            panel.spacing = unit(1.5, "lines")
          ) +
          guides(fill = FALSE)

        ggplotly(plot, height = height_value)
    }
  })

  output$fold_data <- plotly::renderPlotly({
   req(processed_data())
   req(!is.null(input$checkbox_fold2log))
   req(!is.null(input$checkbox_scale_value))
   plot_fold()
  })

  # Render distribution plots
  plot_dist <- reactive({
    req(input$dist_plot_type, processed_data(), input$xlab_value, input$ylab_value)
    common_column_value <- common_column()
    top_criterium <- input$top_ranges_crit
    scale_check <- input$checkbox_scale_value
    height_value <- input$height_value
    type_check <- input$dist_plot_type
    top_compound_values <- input$top_compound_range
    real_data_merged <- processed_data()
    long_data <- real_data_merged %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "_")

   if(nrow(real_data_merged) < 30) {
      summary_data <- long_data %>%
        group_by(.data[[common_column()]], group) %>%
        ungroup()
   }
   else {
      big_summary_data <- long_data %>%
        group_by(.data[[common_column_value]], group) %>%
        dplyr::summarize(
          min_value = min(value, na.rm = TRUE),
          max_value = max(value, na.rm = TRUE),
          mean_value = mean(value, na.rm = TRUE),
          max_minus_min = max_value - min_value
        ) %>%
        ungroup()
   }


    if(nrow(real_data_merged) < 30) {
      summary_data <- summary_data %>%
        filter(.data[[common_column()]] %in% input$compound_range)
    } else {
      if (top_criterium == "Top MAX/MIN Amplitude") {
      top_compounds <- big_summary_data %>%
        group_by(.data[[common_column_value]]) %>%
        dplyr::summarize(max_max_minus_min = max(max_minus_min, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(desc(max_max_minus_min)) %>%
        slice_max(order_by = max_max_minus_min, n = 30) %>%
        select(.data[[common_column_value]])
      } else if (top_criterium == "Statistically significant (parametric)") {
        short_data <- processed_data()
        long_data <- short_data %>%
          pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
          separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

        num_groups <- long_data %>%
          distinct(group) %>%
          nrow()

        if (num_groups > 2) {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = anova(lm(value ~ group))$"Pr(>F)"[1]) %>%
            arrange(p_value)
        } else {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = t.test(value ~ group)$p.value) %>%
            arrange(p_value)
        }

        results_df <- data.frame(results)
        results_df$p_value <- as.numeric(results_df$p_value)

        top_compounds <- results_df %>%
          filter(p_value < 0.05) %>%
          select(-c(2))
        colnames(top_compounds)[1] = common_column_value

      } else if (top_criterium == "Statistically significant (non-parametric)") {
        short_data <- processed_data()
        long_data <- short_data %>%
          pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
          separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

        num_groups <- long_data %>%
          distinct(group) %>%
          nrow()

        if (num_groups > 2) {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = kruskal.test(value ~ group)$p.value) %>%
            arrange(p_value)
        } else {
          results <- long_data %>%
            group_by(.data[[common_column_value]]) %>%
            summarise(p_value = wilcox.test(value ~ group)$p.value) %>%
            arrange(p_value)
        }

        results_df <- data.frame(results)
        results_df$p_value <- as.numeric(results_df$p_value)

        top_compounds <- results_df %>%
          filter(p_value < 0.05) %>%
          select(-c(2))
        colnames(top_compounds)[1] = common_column_value

      } else if (top_criterium == "Top RF Gini Index") {
        gini_data <- processed_data()
        new_row <- rep(NA, ncol(gini_data))
        for (i in 2:ncol(gini_data)) {
          prefix <- strsplit(colnames(gini_data)[i], "_")[[1]][1]
          new_row[i] <- sub("group([1-9][0-9]*)", "group\\1", prefix)
          if (prefix == "group1") {
            new_row[i] <- "control_group"
          }
        }
        new_row[1] <- "Label"
        new_df <- rbind(new_row, gini_data)
        rownames(new_df) <- NULL
        t_data <- t(new_df)
        colnames(t_data) <- t_data[1,]
        data <- t_data[-1,]
        data[,2:ncol(data)] <- sapply(data[,2:ncol(data)],as.numeric)

        modified_colnames <- paste0('X', gsub("[.@]", "_", colnames(data)[-1]))
        original_colnames <- colnames(data)[-1]
        name_mapping <- setNames(modified_colnames, original_colnames)
        inverted_name_mapping <- setNames(names(name_mapping), name_mapping)

        colnames(data) <- str_replace_all(colnames(data), "[[:punct:]]", " ")
        colnames(data) <- gsub(" ", "_", colnames(data))
        colnames(data) <- paste0('X', colnames(data))
        set.seed(input$range_set_seed %||% 1)
        model <- randomForest(as.factor(XLabel) ~ ., data = data,
                              ntree = input$range_tree_num %||% 2000, mtry = 2,
                              importance = TRUE)
        results <- data.frame(Gini=sort(importance(model, type=2)[,], decreasing=T))
        results <- data.frame(rownames(results), results, row.names = NULL)
        colnames(results)[1] <- common_column_value
        top_compounds <- head(results, 30)

        if (!is.null(top_compounds[[common_column_value]])) {
          mapped_names <- sapply(as.character(top_compounds[[common_column_value]]), function(x) inverted_name_mapping[x])
          top_compounds[[common_column_value]] <- mapped_names
        }

        top_compounds
      }

      top_compounds_reactive(top_compounds[[common_column_value]])

      dist_filtered_data <- big_summary_data %>%
        filter(.data[[common_column_value]] %in% top_compounds[[common_column_value]])

      dist_filtered_data <- dist_filtered_data[order(match(dist_filtered_data[[common_column()]], input$rank_list_top)), ]
      summary_data <- long_data %>%
        group_by(.data[[common_column()]], group) %>%
        ungroup() %>%
        filter(.data[[common_column()]] %in% top_compounds_reactive()) %>%
        filter(.data[[common_column_value]] %in% top_compound_values)
    }

    range_x_limits <- if (nrow(real_data_merged) < 30) {
      input$rank_list_basic
    } else {
      input$rank_list_top
    }

    if (type_check == "Violin") {
      plot <- ggplot(summary_data, aes(x = .data[[common_column()]], y = value)) +
        geom_violin() +
        labs(x = "Compound", y = "Value") +
        scale_x_discrete(limits = range_x_limits)
    } else if (type_check == "Smooth") {
      plot <- ggplot(summary_data, aes(value, fill = .data[[common_column()]], color = .data[[common_column()]])) +
        geom_density(alpha = 0.3) +
        labs(x = "Value", y = "Density")
    } else {
      plot <- ggplot(summary_data, aes(x = .data[[common_column()]], y = value)) +
        geom_boxplot() +
        labs(x = "Compound", y = "Value") +
        scale_x_discrete(limits = range_x_limits)
    }

    plot <- plot +
      ggtitle(title_value()) +
      facet_wrap(~group, scales = if (scale_check == FALSE) "free_y" else "fixed", ncol = 1) +
      #scale_x_discrete(limits = range_x_limits) +
      theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(margin = ggplot2::margin(t = ifelse(nrow(real_data_merged) < 30, 15, 30) + input$xlab_value)),
            axis.title.y = element_text(margin = ggplot2::margin(r = 10 + input$ylab_value))
            )

    ggplotly(plot, height = height_value)
  })

  output$dist_data <- plotly::renderPlotly({
    req(input$dist_plot_type)
    req(!is.null(input$checkbox_scale_value))
    plot_dist()
  })

  # output$dist_data <- DT::renderDT(
  #  DT::datatable(
  #   plot_dist()
  # ))

  # Render Grubbs test outlier detection plot
  plot_outlier <- reactive({
    req(input$selector_dropdown, input$test_type_dropdown, processed_data())
    selected_comp <- input$selector_dropdown
    nudge_value <- input$nudge_value
    test_type <- input$test_type_dropdown
    data_scale_check <- input$checkbox_scale
    height_value <- input$height_value
    common_column_value <- common_column()
    real_data_merged <- processed_data()


    if (data_scale_check == "Yeo-Johnson") {
      numeric_data <- real_data_merged[, -1]
      norm_obj <- apply(numeric_data, 1, function(x) yeojohnson(x))
      normalized_list <- vector("list", nrow(numeric_data))
      for (i in 1:nrow(numeric_data)) {
        normalized_list[[i]] <- as.data.frame(t(norm_obj[[i]]$x.t))
      }
      normalized_df <- do.call(rbind, normalized_list)
      real_data_merged <- cbind(setNames(real_data_merged[common_column_value], common_column_value), normalized_df)
    } else if (data_scale_check == "Ordered Quantile normalization") {
      numeric_data <- real_data_merged[, -1]
      norm_obj <- apply(numeric_data, 1, function(x) orderNorm(x))
      normalized_list <- vector("list", nrow(numeric_data))
      for (i in 1:nrow(numeric_data)) {
        normalized_list[[i]] <- as.data.frame(t(norm_obj[[i]]$x.t))
      }
      normalized_df <- do.call(rbind, normalized_list)
      real_data_merged <- cbind(setNames(real_data_merged[common_column_value], common_column_value), normalized_df)
    } else if (data_scale_check == "sqrt normalization") {
      numeric_data <- real_data_merged[, -1]
      norm_obj <- apply(numeric_data, 1, function(x) sqrt_x(x))
      normalized_list <- vector("list", nrow(numeric_data))
      for (i in 1:nrow(numeric_data)) {
        normalized_list[[i]] <- as.data.frame(t(norm_obj[[i]]$x.t))
      }
      normalized_df <- do.call(rbind, normalized_list)
      real_data_merged <- cbind(setNames(real_data_merged[common_column_value], common_column_value), normalized_df)
    } else if (data_scale_check == "log normalization") {
      numeric_data <- real_data_merged[, -1]
      norm_obj <- apply(numeric_data, 1, function(x) log_x(x))
      normalized_list <- vector("list", nrow(numeric_data))
      for (i in 1:nrow(numeric_data)) {
        normalized_list[[i]] <- as.data.frame(t(norm_obj[[i]]$x.t))
      }
      normalized_df <- do.call(rbind, normalized_list)
      real_data_merged <- cbind(setNames(real_data_merged[common_column_value], common_column_value), normalized_df)
    } else {
      real_data_merged <- real_data_merged
    }


    long_data <- real_data_merged %>%
      filter(.data[[common_column()]] == selected_comp) %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "_")

    calculate_grubbs_pvalue <- function(x) {
      result_max <- grubbs.test(x)
      result_second_max <- grubbs.test(x[-which.max(x)])
      result_third_max <- grubbs.test(sort(x, decreasing = TRUE)[-1:-2])
      result_fourth_max <- grubbs.test(sort(x, decreasing = TRUE)[-1:-3])
      result_fifth_max <- grubbs.test(sort(x, decreasing = TRUE)[-1:-4])
      result_min <- grubbs.test(x, opposite = TRUE)
      if (length(x) > 30) {
        result_two_max <- grubbs.test(x)
        result_two_min <- grubbs.test(x)
      } else {
        result_two_max <- grubbs.test(x, type = 20)
        result_two_min <- grubbs.test(x, type = 20, opposite = TRUE)
      }
      return(list(
        p_value = result_max$p.value,
        max_value = max(x, na.rm = TRUE),
        p_value_min = result_min$p.value,
        min_value = min(x, na.rm = TRUE),
        p_value_second_max = result_second_max$p.value,
        second_max_value = max(x[-which.max(x)], na.rm = TRUE),
        p_value_third_max = result_third_max$p.value,
        third_max_value = max(sort(x, decreasing = TRUE)[-1:-2], na.rm = TRUE),
        p_value_fourth_max = result_fourth_max$p.value,
        fourth_max_value = max(sort(x, decreasing = TRUE)[-1:-3], na.rm = TRUE),
        p_value_fifth_max = result_fifth_max$p.value,
        fifth_max_value = max(sort(x, decreasing = TRUE)[-1:-4], na.rm = TRUE),
        p_value_two_max = result_two_max$p.value,
        p_value_two_min = result_two_min$p.value
      ))
    }

    grubbs_results <- long_data %>%
      group_by(.data[[common_column()]], group) %>%
      summarise(
        p_value_max = calculate_grubbs_pvalue(value)$p_value,
        max_value = calculate_grubbs_pvalue(value)$max_value,
        p_value_min = calculate_grubbs_pvalue(value)$p_value_min,
        min_value = calculate_grubbs_pvalue(value)$min_value,
        p_value_second_max = calculate_grubbs_pvalue(value)$p_value_second_max,
        second_max_value = calculate_grubbs_pvalue(value)$second_max_value,
        p_value_third_max = calculate_grubbs_pvalue(value)$p_value_third_max,
        third_max_value = calculate_grubbs_pvalue(value)$third_max_value,
        p_value_fourth_max = calculate_grubbs_pvalue(value)$p_value_fourth_max,
        fourth_max_value = calculate_grubbs_pvalue(value)$fourth_max_value,
        p_value_fifth_max = calculate_grubbs_pvalue(value)$p_value_fifth_max,
        fifth_max_value = calculate_grubbs_pvalue(value)$fifth_max_value,
        p_value_two_max = calculate_grubbs_pvalue(value)$p_value_two_max,
        p_value_two_min = calculate_grubbs_pvalue(value)$p_value_two_min
      ) %>%
      ungroup()

    if (test_type == "Max") {
      out_plot <- ggplot(long_data, aes(x = group, y = value, fill = group)) +
        scale_fill_viridis_d( option = "D") +
        geom_violin(alpha=0.4, position = position_dodge(width = .75),size=1,color="black") +
        geom_point(shape = 21,size=2, position = position_jitterdodge(dodge.width = .75), color="black",alpha=1) +
        geom_text(
        data = grubbs_results,
        aes(x = group, y = max_value + (max_value*.025), label = sprintf("p-val: %.3f", p_value_max)),
        nudge_x = abs(nudge_value),
        vjust = -0.5,
        size = 3
      )
    } else if (test_type == "Second Max") {
      out_plot <- ggplot() +
        scale_fill_viridis_d( option = "D") +
        geom_violin(data = long_data %>%
                      group_by(group) %>%
                      mutate(subset = ifelse(value == max(value, na.rm = TRUE), NA, value)),
                    aes(x = group, y = subset, fill = group),
                    alpha = 0.4, position = position_dodge(width = .75), size = 1, color = "black") +
        geom_point(data = long_data %>%
                     group_by(group) %>%
                     mutate(opacity = ifelse(value == max(value, na.rm = TRUE), 0.25, 1)),
                   aes(x = group, y = value, alpha = opacity, fill = group),
                   shape = 21,
                   size = 2,
                   position = position_jitterdodge(dodge.width = 0.75),
                   color = "black") +
        geom_text(
          data = grubbs_results,
          aes(x = group, y = second_max_value + (second_max_value*.025), label = sprintf("p-val: %.3f", p_value_second_max)),
          nudge_x = abs(nudge_value),
          vjust = -0.5,
          size = 3)


    } else if (test_type == "Third Max") {
      out_plot <- ggplot() +
        scale_fill_viridis_d( option = "D") +
        geom_violin(data = long_data %>%
                      group_by(group) %>%
                      mutate(subset = ifelse(value == max(value, na.rm = TRUE) | value == sort(value, decreasing = TRUE)[2], NA, value)),
                    aes(x = group, y = subset, fill = group),
                    alpha = 0.4, position = position_dodge(width = .75), size = 1, color = "black") +
        geom_point(data = long_data %>%
                     group_by(group) %>%
                     mutate(opacity = ifelse(value == max(value, na.rm = TRUE) | value == sort(value, decreasing = TRUE)[2], 0.25, 1)),
                   aes(x = group, y = value, alpha = opacity, fill = group),
                   shape = 21,
                   size = 2,
                   position = position_jitterdodge(dodge.width = 0.75),
                   color = "black") +
        geom_text(
          data = grubbs_results,
          aes(x = group, y = third_max_value + (third_max_value*.025), label = sprintf("p-val: %.3f", p_value_third_max)),
          nudge_x = abs(nudge_value),
          vjust = -0.5,
          size = 3
        )
    } else if (test_type == "Fourth Max") {
      out_plot <- ggplot() +
        scale_fill_viridis_d( option = "D") +
        geom_violin(data = long_data %>%
                      group_by(group) %>%
                      mutate(subset = ifelse(value == max(value, na.rm = TRUE) | value == sort(value, decreasing = TRUE)[2] | value == sort(value, decreasing = TRUE)[3], NA, value)),
                    aes(x = group, y = subset, fill = group),
                    alpha = 0.4, position = position_dodge(width = .75), size = 1, color = "black") +
        geom_point(data = long_data %>%
                     group_by(group) %>%
                     mutate(opacity = ifelse(value == max(value, na.rm = TRUE) | value == sort(value, decreasing = TRUE)[2] | value == sort(value, decreasing = TRUE)[3], 0.25, 1)),
                   aes(x = group, y = value, alpha = opacity, fill = group),
                   shape = 21,
                   size = 2,
                   position = position_jitterdodge(dodge.width = 0.75),
                   color = "black") +
        geom_text(
          data = grubbs_results,
          aes(x = group, y = fourth_max_value + (fourth_max_value*.025), label = sprintf("p-val: %.3f", p_value_fourth_max)),
          nudge_x = abs(nudge_value),
          vjust = -0.5,
          size = 3
        )
    } else if (test_type == "Fifth Max") {
      out_plot <- ggplot() +
        scale_fill_viridis_d( option = "D") +
        geom_violin(data = long_data %>%
                      group_by(group) %>%
                      mutate(subset = ifelse(value == max(value, na.rm = TRUE) | value == sort(value, decreasing = TRUE)[2] | value == sort(value, decreasing = TRUE)[3] | value == sort(value, decreasing = TRUE)[4], NA, value)),
                    aes(x = group, y = subset, fill = group),
                    alpha = 0.4, position = position_dodge(width = .75), size = 1, color = "black") +
        geom_point(data = long_data %>%
                     group_by(group) %>%
                     mutate(opacity = ifelse(value == max(value, na.rm = TRUE) | value == sort(value, decreasing = TRUE)[2] | value == sort(value, decreasing = TRUE)[3] | value == sort(value, decreasing = TRUE)[4], 0.25, 1)),
                   aes(x = group, y = value, alpha = opacity, fill = group),
                   shape = 21,
                   size = 2,
                   position = position_jitterdodge(dodge.width = 0.75),
                   color = "black") +
        geom_text(
          data = grubbs_results,
          aes(x = group, y = fifth_max_value + (fifth_max_value*.025), label = sprintf("p-val: %.3f", p_value_fifth_max)),
          nudge_x = abs(nudge_value),
          vjust = -0.5,
          size = 3
        )
    } else {
      out_plot <- ggplot(long_data, aes(x = group, y = value, fill = group)) +
        scale_fill_viridis_d( option = "D") +
        geom_violin(alpha=0.4, position = position_dodge(width = .75),size=1,color="black") +
        geom_point(shape = 21,size=2, position = position_jitterdodge(dodge.width = .75), color="black",alpha=1) +
        geom_text(
        data = grubbs_results,
        aes(x = group, y = min_value - (min_value*.025),label = sprintf("p-val: %.3f", p_value_min)),
        nudge_x = abs(nudge_value),
        vjust = -0.5,
        size = 3
      )
    }

    plot <- out_plot +
      ggtitle(title_value()) +
      labs(x = selected_comp, y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))

    ggplotly(plot, height = height_value)
  })

  output$out_data <- plotly::renderPlotly({
    req(input$selector_dropdown, input$test_type_dropdown)
    plot_outlier()
  })

   #   output$out_data <- DT::renderDT(
   #    DT::datatable(
   #    plot_outlier()
   # ))

  # Render PCA outlier detection plot
  stored_full_df <- reactiveVal(NULL)
  stored_outliers_df <- reactiveVal(NULL)

  plot_pca <- reactive({
    tryCatch({
    req(input$pca_dropdown, input$criterion_dropdown, processed_data(), input$pca_dropdown, input$method_dropdown, input$distance_dropdown)
    data_df_stats <- req(processed_data_with_stats())
    pca_type <- input$pca_dropdown
    pca_crit <- input$criterion_dropdown
    quant_val <- input$quant
    pca_compounds <- input$pca_compound_range
    pca_samples <- input$pca_sample_range
    sd_val <- input$sd_n
    height_value <- input$height_value
    nudge_value <- input$nudge_value
    out_method <- input$method_dropdown
    out_distance <- input$distance_dropdown

    real_data_merged <- processed_data()

    filtered_data <- processed_data() %>%
      filter(.data[[common_column()]] %in% pca_compounds)

    if (nrow(filtered_data) == 0) {
      filtered_data <- real_data_merged
    }

    filt_dat <- filtered_data[,-1]

    # Check for constant columns
    constant_cols <- try(sapply(filt_dat, function(x) length(unique(x)) == 1), silent = TRUE)
    if (inherits(constant_cols, "try-error")) {
      stop("Computation failed. Please check your data.")
    }

    if (any(constant_cols)) {
      return(NULL)
    }

    filt_dat_imputed <- try(t(apply(filt_dat, 1, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))), silent = TRUE)
    if (inherits(filt_dat_imputed, "try-error")) {
      stop("Computation failed. Please check your data.")
    }

    filt_data_imputed <- cbind(filtered_data[, 1], filt_dat_imputed)
    colnames(filt_data_imputed) <- c(common_column(), colnames(filt_dat_imputed))

    get_group_info <- function(col_names) {
      groups <- gsub("^group(\\d+)_.*", "group\\1", col_names)
      return(groups)
    }

    to_remove <- get_group_info(names(filt_data_imputed)[-1])
    new_row <- c("group", to_remove)
    filt_data_imputed <- rbind(new_row, filt_data_imputed)

    trans_data <- t(filt_data_imputed)

    new_names <- t(filt_data_imputed[,1])
    rownames(trans_data) <- colnames(filt_data_imputed)
    colnames(trans_data) <- new_names
    trans_data <- trans_data[-1,-1]
    trans_dat <- trans_data
    rownames(trans_dat) <- NULL
    trans_dat <- data.frame(trans_dat)
    trans_dat <- trans_dat %>%
      mutate_if(is.character, as.numeric)

    # Row outliers
    if (out_method == "PCA") {
      pca_dat <- try(prcomp(filt_dat_imputed, scale. = TRUE, rank. = 5), silent = TRUE)
      if (inherits(pca_dat, "try-error")) {
        stop("PCA computation failed. Please check your data.")
      }
      pc_scores <- pca_dat$x
    } else if (out_method == "PLS") {
      set.seed(2024)
      response <- rnorm(nrow(filt_dat_imputed))
      pls_model <- plsr(response ~ filt_dat_imputed, scale = TRUE, validation = "CV")

      pc_scores <- scores(pls_model)
      pls_residuals <- residuals(pls_model)
    }

    if (out_distance == "Euclidean distance") {
      distances <- apply(pc_scores, 1, function(x) sqrt(sum(x^2)))
    } else if (out_distance == "Mahalanobis distance") {
      mean_vector <- colMeans(pc_scores)
      cov_matrix <- cov(pc_scores)
      inv_cov_matrix <- tryCatch(solve(cov_matrix), error = function(e) ginv(cov_matrix))
      distances <- apply(pc_scores, 1, function(x) {
        diff <- x - mean_vector
        sqrt(t(diff) %*% inv_cov_matrix %*% diff)
      })
    } else if (out_distance == "Hotelling T2") {
      mean_vector <- colMeans(pc_scores)
      cov_matrix <- cov(pc_scores)
      inv_cov_matrix <- tryCatch(solve(cov_matrix), error = function(e) ginv(cov_matrix))
      distances <- apply(pc_scores, 1, function(x) {
        diff <- x - mean_vector
        t(diff) %*% inv_cov_matrix %*% diff
      })
    }

    if (out_method == "PLS") {
      residual_distances <- apply(pls_residuals, 1, function(x) sqrt(sum(x^2)))
    } else if (out_method == "PCA") {
      residual_distances <- NULL
    }

    if (pca_crit == "Quantile") {
      threshold <- quantile(distances, quant_val)
      if (out_method == "PLS") {
        residual_threshold <- quantile(residual_distances, quant_val)
      } else if (out_method == "PCA") {
        residual_threshold <- NULL
      }
    } else {
      threshold <- mean(distances) + sd_val * sd(distances)
      if (out_method == "PLS") {
        residual_threshold <- mean(residual_distances) + sd_val * sd(residual_distances)
      } else if (out_method == "PCA") {
        residual_threshold <- NULL
      }
    }

    if (out_method == "PLS") {
      outliers_scores <- which(distances > threshold)
      outliers_residuals <- which(residual_distances > residual_threshold)
      outliers <- unique(c(outliers_scores, outliers_residuals))
    } else {
      outliers <- which(distances > threshold)
    }


    full_df <- data.frame(
      PC1 = pc_scores[, 1],
      PC2 = pc_scores[, 2],
      Distance = distances,
      Compound = filtered_data[[common_column()]]
    )

    outliers_df <- data.frame(
      PC1 = pc_scores[outliers, 1],
      PC2 = pc_scores[outliers, 2],
      Index = outliers,
      Distance = distances[outliers],
      Compound = filtered_data[[common_column()]][outliers]
    )

    stored_full_df(full_df)
    stored_outliers_df(outliers_df)

    # Column outliers
    if (out_method == "PCA") {
      pca_trans_dat <- try(prcomp(trans_dat, scale. = TRUE, rank. = 5), silent = TRUE)
      if (inherits(pca_trans_dat, "try-error")) {
        stop("PCA computation failed. Please check your data.")
      }
      sample_pc_scores <- pca_trans_dat$x
    } else if (out_method == "PLS") {
      set.seed(2024)
      trans_response <- rnorm(nrow(trans_dat))
      trans_pls_model <- plsr(trans_response ~ ., data = trans_dat, scale = TRUE, validation = "CV")

      sample_pc_scores <- scores(trans_pls_model)
      sample_pls_residuals <- residuals(trans_pls_model)
    }

    if (out_distance == "Euclidean distance") {
      sample_distances <- apply(sample_pc_scores, 1, function(x) sqrt(sum(x^2)))
      sample_mean_distance <- mean(sample_distances)
      sample_sd_distance <- sd(sample_distances)
    } else if (out_distance == "Mahalanobis distance") {
      sample_mean_vector <- colMeans(sample_pc_scores)
      sample_cov_matrix <- cov(sample_pc_scores)
      sample_inv_cov_matrix <- tryCatch(solve(sample_cov_matrix), error = function(e) ginv(sample_cov_matrix))
      sample_distances <- apply(sample_pc_scores, 1, function(x) {
        sample_diff <- x - sample_mean_vector
        sqrt(t(sample_diff) %*% sample_inv_cov_matrix %*% sample_diff)
      })
      sample_mean_distance <- mean(sample_distances)
      sample_sd_distance <- sd(sample_distances)
    } else if (out_distance == "Hotelling T2") {
      sample_mean_vector <- colMeans(sample_pc_scores)
      sample_cov_matrix <- cov(sample_pc_scores)
      sample_inv_cov_matrix <- tryCatch(solve(sample_cov_matrix), error = function(e) ginv(sample_cov_matrix))
      sample_distances <- apply(sample_pc_scores, 1, function(x) {
        sample_diff <- x - sample_mean_vector
        t(sample_diff) %*% sample_inv_cov_matrix %*% sample_diff
      })
      sample_mean_distance <- mean(sample_distances)
      sample_sd_distance <- sd(sample_distances)
    }

    if (out_method == "PLS") {
      sample_residual_distances <- apply(sample_pls_residuals, 1, function(x) sqrt(sum(x^2)))
    } else if (out_method == "PCA") {
      sample_residual_distances <- NULL
    }

    if (pca_crit == "Quantile") {
      sample_threshold <- quantile(sample_distances, quant_val)
      if (out_method == "PLS") {
        sample_residual_threshold <- quantile(sample_residual_distances, quant_val)
      } else if (out_method == "PCA") {
        sample_residual_threshold <- NULL
      }
    } else {
      sample_threshold <- sample_mean_distance + sd_val * sample_sd_distance
      if (out_method == "PLS") {
        sample_residual_threshold <- mean(sample_residual_distances) + sd_val * sd(sample_residual_distances)
      } else if (out_method == "PCA") {
        sample_residual_threshold <- NULL
      }
    }

    if (out_method == "PLS") {
      sample_outliers_scores <- which(sample_distances > sample_threshold)
      sample_outliers_residuals <- which(sample_residual_distances > sample_residual_threshold)
      sample_outliers <- unique(c(sample_outliers_scores, sample_outliers_residuals))
    } else {
      sample_outliers <- which(sample_distances > sample_threshold)
    }

    sample_outliers_df <- data.frame(
      PC1 = sample_pc_scores[sample_outliers, 1],
      PC2 = sample_pc_scores[sample_outliers, 2],
      Index = sample_outliers,
      Distance = sample_distances[sample_outliers],
      Sample = rownames(trans_data)[sample_outliers]
      )

    int_threshold <- quantile(data_df_stats$Combined_Score, 0.99)
    point_of_int <- data_df_stats %>%
      filter(Combined_Score > int_threshold)

    interesting_points <- full_df %>%
      filter(Compound %in% pull(point_of_int, .data[[common_column()]]))

    if (pca_type == "Column") {
      plot <- try(ggplot() +
        geom_point(data = data.frame(PC1 = sample_pc_scores[, 1], PC2 = sample_pc_scores[, 2]), aes(x = PC1, y = PC2)) +
        geom_text(data = sample_outliers_df, aes(label = paste(Sample, "\n", "Dist:", round(Distance, 2)), x = PC1, y = PC2), color = "#b32323", size = 3.5, nudge_y = abs(nudge_value))
        , silent = TRUE)
        if (inherits(plot, "try-error")) {
          stop("Computation failed. Please check your data.")
        }
      } else {
      plot <- try(ggplot() +
        geom_point(data = data.frame(PC1 = pc_scores[, 1], PC2 = pc_scores[, 2]), aes(x = PC1, y = PC2)) +
        geom_text(data = outliers_df, aes(label = paste(Compound, "\n", "Dist:", round(Distance, 2)), x = PC1, y = PC2), color = "#b32323", size = 3.5, nudge_y = abs(nudge_value))
        , silent = TRUE)
        if (inherits(plot, "try-error")) {
          stop("Computation failed. Please check your data.")
        }
        if (input$interest_check) {
          plot <- plot +
            geom_point(data = interesting_points, aes(x = PC1, y = PC2), color = "#19a8c5", size = 1)
        }
      }

    plot <- plot +
      ggtitle(title_value()) +
      labs(x = ifelse(out_method == "PLS", "PLS1", "PC1"),
           y = ifelse(out_method == "PLS", "PLS2", "PC2")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))

    ggplotly(source = "sub_pca", plot, height = 540)
    }, error = function(e) {
      showNotification("An error occurred during computation: Please check your data for negative values.", duration = NULL, type = "error")
      NULL
    })
  })


 #   output$pca_data <- DT::renderDT(
 #    DT::datatable(
 #    plot_pca()
 # ))

    output$pca_data <- plotly::renderPlotly({
      req(input$pca_dropdown, input$criterion_dropdown)
      plot_pca()
    })

    output$outliers_table <- renderPlot({
      req(processed_data(), stored_full_df(), common_column(), processed_data_with_stats(), input$pca_dropdown)
      data <- processed_data()
      stats <- processed_data_with_stats()
      cc <- common_column()
      outliers_df <- stored_full_df()

      plot_event <- event_data("plotly_click", source = "sub_pca")

      if (is.null(plot_event)) {
        plot_data <- data.frame(
          x = c(1),
          y = c(1),
          label = "Please click on any point to see the data."
        )

        ggplot(plot_data, aes(x = x, y = y)) +
          geom_text(aes(label = label), size = 8) +
          theme_void() +
          theme(
            plot.title = element_text(hjust = 0.5)
          ) +
          ggtitle("No Data Selected")
      } else {
        plot_data <- plot_event %>%
          left_join(outliers_df, by = c("x" = "PC1")) %>%
          left_join(data, by = c("Compound" = cc))
        stats_data <- plot_event %>%
          left_join(outliers_df, by = c("x" = "PC1")) %>%
          left_join(stats, by = c("Compound" = cc))


      display_compound <- plot_data$Compound
      comp_cv <- stats_data$CV
      comp_mean <- stats_data$Mean
      plot_data <- plot_data[,-1:-7]
      col_names <- colnames(plot_data)
      plot_data[2,] <- colnames(plot_data)
      plot_data <- setNames(data.frame(t(plot_data)), plot_data[ , 2])
      colnames(plot_data) <- c("vals", "names")
      plot_data$vals <- as.numeric(plot_data$vals)

      get_group <- function(name) {
        if (grepl("^group1_", name)) {
          return("group1")
        } else if (grepl("^group2_", name)) {
          return("group2")
        } else if (grepl("^group3_", name)) {
          return("group3")
        } else if (grepl("^group4_", name)) {
          return("group4")
        } else if (grepl("^group5_", name)) {
          return("group5")
        } else if (grepl("^group6_", name)) {
          return("group6")
        } else if (grepl("^group7_", name)) {
          return("group7")
        } else if (grepl("^group8_", name)) {
          return("group8")
        } else if (grepl("^group9_", name)) {
          return("group9")
        } else if (grepl("^group10_", name)) {
          return("group10")
        } else {
          return("other")
        }
      }

      plot_data$group <- sapply(plot_data$names, get_group)

      pca_color_palette <- c("group1" = "#19a8c5", "group2" = "#F84F44", "group3" = "#E69F00",
                         "group4" = "#7B3294", "group5" = "#1B9E77", "group6" = "#0000c2",
                         "group7" = "#ff68ad", "group8" = "#F0E442", "group9" = "#6f1c00")

      if (input$pca_dropdown == "Row") {
        ggplot(plot_data, aes(x = names, y = vals, fill = group)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = pca_color_palette) +
          labs(title = paste("Original data of ", display_compound, ", Mean = ", comp_mean, ", CV = ", comp_cv), x = "Sample", y = "Value") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

      } else {
        plot_data <- data.frame(
          x = c(1),
          y = c(1),
          label = "Original data visualization only avaliable when searching by row."
        )

        ggplot(plot_data, aes(x = x, y = y)) +
          geom_text(aes(label = label), size = 8) +
          theme_void() +
          theme(
            plot.title = element_text(hjust = 0.5)
          )
      }
      }
      })

    # output$outliers_table <- renderTable({
    #   event_data("plotly_click", source = "sub_pca")$points %>%
    #     left_join(outliers_df, by = c("pointNumber" = "Index"))
    # })





# Render Gini index plot
    processed_transposed_data <- reactive({
      req(processed_data())
      gini_data <- processed_data()
      new_row <- rep(NA, ncol(gini_data))
      for (i in 2:ncol(gini_data)) {
        prefix <- strsplit(colnames(gini_data)[i], "_")[[1]][1]
        new_row[i] <- ifelse(prefix == "group1", "control_group", sub("group([1-9][0-9]*)", "group\\1", prefix))
      }
      new_row[1] <- "Label"
      new_df <- rbind(new_row, gini_data)
      rownames(new_df) <- NULL
      t_data <- t(new_df)
      colnames(t_data) <- t_data[1,]
      data <- t_data[-1,]
      data[,2:ncol(data)] <- sapply(data[,2:ncol(data)], as.numeric)
      modified_colnames <- paste0('X', gsub("[.@]", "_", colnames(data)[-1]))
      original_colnames <- colnames(data)[-1]
      name_mapping <- setNames(modified_colnames, original_colnames)
      inverted_name_mapping <- setNames(names(name_mapping), name_mapping)
      colnames(data) <- str_replace_all(colnames(data), "[[:punct:]]", " ")
      colnames(data) <- gsub(" ", "_", colnames(data))
      colnames(data) <- paste0('X', colnames(data))
      list(data = data, inverted_name_mapping = inverted_name_mapping)
    })

    trained_model <- reactive({
      data <- processed_transposed_data()$data
      processed_transposed_data <- processed_transposed_data()$inverted_name_mapping

      set.seed(input$set_seed %||% 1)
      model <- randomForest(as.factor(XLabel) ~ ., data = data,
                            ntree = input$tree_num %||% 2000, mtry = 2,
                            importance = TRUE)

      list(model = model, data = data)
    })

    plot_gini <- reactive({
      req(trained_model())
      common_column_value <- common_column()
      inverted_name_mapping <- processed_transposed_data()$inverted_name_mapping
      model <- trained_model()$model
      data <- trained_model()$data

      results <- data.frame(Gini=sort(importance(model, type=2)[,], decreasing=T))
      results <- data.frame(Compound = rownames(results), results, row.names = NULL)
      top_results <- head(results, 30)

      mapped_names <- sapply(as.character(top_results$Compound), function(x) inverted_name_mapping[x])
      top_results$Compound <- mapped_names

      plot <- ggplot(top_results, aes(x = reorder(Compound, Gini), y = Gini)) +
        geom_segment(aes(x = Compound, xend = Compound, y = 0, yend = Gini), color = "skyblue") +
        geom_point(aes(color = "skyblue"), size = 2) +
        coord_flip() +
        labs(x = "Compounds", y = "Gini Index") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(plot, height = 400)
    })

  output$gini_data <- plotly::renderPlotly({
    req(plot_gini())
    plot_gini()
  })

  output$conf_matrix <- DT::renderDT({
    req(trained_model())
    DT::datatable(trained_model()$model$confusion)
  })


  m_d_a <- reactive({
    inverted_name_mapping <- processed_transposed_data()$inverted_name_mapping
    mda_data <- data.frame(mean_decrease_acc = sort(importance(trained_model()$model, type=1)[,], decreasing=T))
    mda_data <- data.frame(Names = rownames(mda_data), mda_data, row.names = NULL)
    mapped_names <- sapply(as.character(mda_data$Names), function(x) inverted_name_mapping[x])
    mda_data$Names <- mapped_names
    mda_data
  })

  output$mean_dec_acc <- DT::renderDT({
    req(trained_model())
    DT::datatable(
      m_d_a()
    )
  })


  output$oob_err <- renderText({
    req(trained_model())
    paste("Out-of-bag error rate:", trained_model()$model$err.rate[nrow(trained_model()$model$err.rate), "OOB"])
  })

  quantile_normalize <- function(df) {
    col_names <- colnames(df)
    mat <- as.matrix(df[, -1])
    normalized_mat <- preprocessCore::normalize.quantiles(mat)
    normalized_df <- data.frame(df[,1], normalized_mat)
    colnames(normalized_df) <- col_names
    return(normalized_df)
  }

  # Render Shapiro test
  shapiro_data_stored <- reactiveVal(NULL)
  current_page <- reactiveVal(1)
  items_per_page <- 50

  total_pages <- reactive(ceiling(nrow(shapiro_data_stored()) / items_per_page))

  observeEvent(input$prev_page, {
    current_page(max(1, current_page() - 1))
  })
  observeEvent(input$next_page, {
    current_page(min(total_pages(), current_page() + 1))
  })
  output$page_info <- renderText({
    paste("Page", current_page(), "of", total_pages())
  })

  stats_func <- reactive({
    req(processed_data(),
      current_page(), common_column(), input$normality_grouping, input$normality_norm, input$normality_trans, input$normality_scale, input$normality_sort)
    isolate({
      stat_data <- processed_data()
      common_column_value <- common_column()
      long_data <- stat_data %>%
        pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
        separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

      if (input$normality_norm == 'by Sum') {
        long_data <- long_data %>%
          group_by(sample) %>%
          mutate(sum_value = sum(value, na.rm = TRUE),
                 value = ifelse(value == 0 | is.na(value), value, value / sum_value)) %>%
          select(-sum_value)
      } else if (input$normality_norm == 'by Median') {
        long_data <- long_data %>%
          group_by(sample) %>%
          mutate(median_value = median(value, na.rm = TRUE),
                 value = ifelse(value == 0 | is.na(value), value, value / median_value)) %>%
          select(-median_value)
      } else if (input$normality_norm == 'Quantile') {
        output_data <- quantile_normalize(stat_data)
        long_data <- output_data %>%
          pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
          separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")
      } else if (input$normality_norm == 'None') {
        long_data <- long_data
      }

      if (input$normality_trans == 'Log(5)') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 5)))
      } else if (input$normality_trans == 'Log(10)') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 10)))
      } else if (input$normality_trans == 'Square') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, value^2))
      } else if (input$normality_trans == 'Cube') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, value^3))
      } else if (input$normality_trans == 'Square root') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, sqrt(value)))
      } else if (input$normality_trans == 'Cube root') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, value^(1/3)))
      } else {
        long_data <- long_data
      }
      stat_data <- long_data %>%
        unite(sample, group, sample, sep = "_") %>%
        pivot_wider(names_from = sample, values_from = value)

      if (input$normality_scale == 'Center on Mean') {
        centered_data <- stat_data[,-1]
        for (i in 1:nrow(centered_data)) {
          centered_data[i, ] <- centered_data[i, ] - rowMeans(centered_data[i, ], na.rm = TRUE)
        }
        stat_data <- cbind(stat_data[1], centered_data)
      } else if (input$normality_scale == 'Auto') {
        centered_data <- stat_data[,-1]
        for (i in 1:nrow(centered_data)) {
          row_mean <- mean(as.numeric(centered_data[i, ]), na.rm = TRUE)
          row_sd <- sd(as.numeric(centered_data[i, ]), na.rm = TRUE)
          centered_data[i, ] <- (centered_data[i, ] - row_mean) / row_sd
        }
        stat_data <- cbind(stat_data[1], centered_data)
      } else if (input$normality_scale == 'None') {
        stat_data <- stat_data
      }

      original_order <- setNames(seq_along(stat_data[[common_column_value]]), stat_data[[common_column_value]])
      results_list <- list()

      get_group_info <- function(col_names) {
        groups <- gsub("^group(\\d+)_.*", "group\\1", col_names)
        return(groups)
      }

      add_first_column <- function(df, first_column) {
        df_with_first_column <- cbind(first_column, df)
        return(df_with_first_column)
      }

      add_first_column_list <- function(df, first_column, prefix) {
        df <- as.data.frame(df)
        df[, 1] <- paste0(prefix, "_", first_column)
        return(df)
      }

      if (input$normality_grouping == "whole data") {
        for (i in 1:nrow(stat_data)) {
          compound_name <- as.character(stat_data[i, 1])
          measurements <- as.numeric(stat_data[i, -1])
          if (length(unique(measurements)) > 1 && any(measurements != 0)) {
            test_result <- shapiro.test(measurements)
            results_list[[compound_name]] <- list(
              Compound = compound_name,
              W = test_result$statistic,
              P_Value = test_result$p.value
            )
          } else {
            results_list[[compound_name]] <- list(
              Compound = compound_name,
              W = NA,
              P_Value = NA
            )
          }
        }
      } else if (input$normality_grouping == "grouped data") {
        dat <- stat_data[,-1]
        group_names <- get_group_info(names(stat_data)[-1])

        ss <- unique(group_names)
        splitted <- lapply(setNames(ss, ss), function(x) dat[, grep(x, colnames(dat))])

        # Convert each element of `splitted` to data frame explicitly
        splitted <- lapply(splitted, function(x) {
          if (!is.data.frame(x)) {
            x <- as.data.frame(x)
          }
          return(x)
        })

        splitted <- lapply(splitted, add_first_column, stat_data[, 1])

        for (i in seq_along(splitted)) {
          current_df <- splitted[[i]]
          current_group <- names(splitted)[i]
          for (j in 1:nrow(current_df)) {
            compound_name <- as.character(current_df[j, 1])
            measurements <- as.numeric(current_df[j, -1])
            if (length(measurements) >= 3 && any(measurements != 0)) {
              test_result <- shapiro.test(measurements)
              results_list[[length(results_list) + 1]] <- list(
                Group = current_group,
                Compound = compound_name,
                W = test_result$statistic,
                P_Value = test_result$p.value
              )
            } else {
              results_list[[length(results_list) + 1]] <- list(
                Group = current_group,
                Compound = compound_name,
                W = NA,
                P_Value = NA
              )
            }
          }
        }
      }

      if (input$normality_grouping == "whole data") {
        results_df <- do.call(rbind, lapply(results_list, function(result) {
          data_frame_result <- data.frame(matrix(unlist(result), nrow=1, byrow=T), stringsAsFactors = FALSE)
          colnames(data_frame_result) <- c("Compound", "W", "P_Value")
          return(data_frame_result)
        }))
      } else if (input$normality_grouping == "grouped data") {
        results_df <- do.call(rbind, lapply(results_list, function(result) {
          data_frame_result <- data.frame(matrix(unlist(result), nrow=1, byrow=T), stringsAsFactors = FALSE)
          colnames(data_frame_result) <- c("Group", "Compound", "W", "P_Value")
          data_frame_result <- unite(data_frame_result, Compound, Group, Compound, sep = "_", remove = FALSE)
          data_frame_result <- data_frame_result %>%
            select(Compound, W, P_Value)
          return(data_frame_result)
        }))
      }

      results_df$num_W <- as.numeric(results_df[, 2])
      results_df$P_Val <- as.numeric(results_df[, 3])

      if (input$normality_grouping == "whole data") {
        results_df$Order <- original_order[results_df$Compound]
      } else if (input$normality_grouping == "grouped data") {
        to_sort <- results_df$Compound
        other_part <- substring(to_sort, 8)
        sorted_indices <- order(other_part)
        sorted_order <- to_sort[sorted_indices]
        sorted_indices <- match(sorted_order, results_df$Compound)
        results_df <- results_df[sorted_indices, ]
        results_df <- mutate(results_df, Order = row_number())
      }

      results_df <- results_df[order(results_df$Order), ]
      results_df <- results_df %>%
        mutate(Max_Order = rank(desc(P_Val)))

      results_df <- results_df %>%
        dplyr::select(Order, Max_Order, Compound, num_W, P_Val)

      if (input$normality_sort == 'Max') {
        results_df <- results_df %>%
          arrange(desc(P_Val))
        results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order, decreasing = TRUE)])
      } else if (input$normality_sort == 'Min') {
        results_df <- results_df %>%
          arrange(P_Val)
        results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order)])
      } else if (input$normality_sort == 'None') {
        results_df$Compound <- factor(results_df$Compound, levels = rev(results_df$Compound[order(results_df$Order)]))
      }

      start <- (current_page() - 1) * items_per_page + 1
      end <- min(nrow(results_df), current_page() * items_per_page)
      display_data <- results_df[start:end, ]

      if (input$normality_grouping == "whole data") {
        joined_data <- left_join(results_df, stat_data, by = c("Compound" = common_column_value))
      } else if (input$normality_grouping == "grouped data") {
        group_names <- get_group_info(names(stat_data)[-1])
        ss <- unique(group_names)
        splitted <- lapply(setNames(ss, ss), function(x) stat_data[, grep(x, colnames(stat_data))])

        splitted <- lapply(splitted, add_first_column, stat_data[, 1])

        splitted <- lapply(splitted, function(x) {
          if (!is.data.frame(x)) {
            x <- as.data.frame(x)
          }
          return(x)
        })

        #splitted <- mapply(add_first_column_list, splitted, stat_data[, 1], names(splitted))

        splitted <- mapply(function(df, prefix) {
          df[, 1] <- paste0(prefix, "_", df[, 1])
          return(df)
        }, splitted, names(splitted), SIMPLIFY = FALSE)

        combined_df <- bind_rows(splitted, .id = "group_name")
        stat_data <- combined_df[,-1]
        joined_data <- left_join(results_df, stat_data, by = c("Compound" = common_column_value))
      }

      shapiro_data_stored(joined_data)

      whole_plot <- ggplot(display_data, aes(x = Compound, y = P_Val)) +
        geom_segment(aes(xend = Compound, y = 0, yend = P_Val), color = "skyblue") +
        geom_point(color = "skyblue", size = 3) +
        coord_flip() +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkred") +
        labs(y = "P-Value", x = "Compound", title = "Shapiro-Wilk Normality Test P-Values") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 11),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              plot.title = element_text(size = 17, hjust = 0.5))

      group_color_palette <- c("group1" = "#19a8c5", "group2" = "#F84F44", "group3" = "#E69F00",
                         "group4" = "#7B3294", "group5" = "#1B9E77", "group6" = "#0000c2",
                         "group7" = "#ff8fc2", "group8" = "#F0E442", "group9" = "#6f1c00")


      group_plot <- ggplot(display_data, aes(x = Compound, y = P_Val, color = factor(substr(Compound, 1, 6)))) +
        geom_segment(aes(xend = Compound, y = 0, yend = P_Val)) +
        geom_point(size = 3) +
        coord_flip() +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkred") +
        labs(y = "P-Value", x = "Compound", title = "Shapiro-Wilk Normality Test P-Values") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 11),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              plot.title = element_text(size = 17, hjust = 0.5)) +
        scale_color_manual(values = group_color_palette, guide = FALSE)

      if (input$normality_grouping == "whole data") {
        whole_plot
      } else if (input$normality_grouping == "grouped data") {
        group_plot
      }
    })
  })

  # output$stats_data <- DT::renderDT({
  #   req(stats_func())
  #   DT::datatable(
  #     stats_func(),
  #     options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })


  output$stats_data <- renderPlot({
    if (!is.null(stats_func())) {
      return(stats_func())
    }
    NULL
  }, height = 550)



  output$hover_histogram <- renderPlot({
    req(input$plot_hover, current_page(), shapiro_data_stored(), input$normality_sort,  input$normality_norm, input$normality_trans, input$normality_scale, input$normality_grouping)
    hover <- input$plot_hover
    shapiro_data <- shapiro_data_stored()
    plot_data <- shapiro_data %>%
      dplyr::select(6:ncol(.))

    items_per_page <- 50
    page_offset <- (current_page() - 1) * items_per_page
    index_on_page <- items_per_page - round(hover$y) + 1
    index <- page_offset + index_on_page

    if (index < 1 || index > nrow(shapiro_data)) return()

    hovered_compound <- shapiro_data[index, "Compound"]
    shapiro_p_value <- shapiro_data[index, "P_Val"]
    hovered_measurements <- as.numeric(plot_data[index, -1])

    fill_color_palette <- c("group1" = "#79daee", "group2" = "#f9736a", "group3" = "#ffd370",
                             "group4" = "#c48bd8", "group5" = "#80e9ca", "group6" = "#7474ff",
                             "group7" = "#ff8fc2", "group8" = "#f9f4af", "group9" = "#ff936f")

    group_color_palette <- c("group1" = "#19a8c5", "group2" = "#F84F44", "group3" = "#E69F00",
                             "group4" = "#7B3294", "group5" = "#1B9E77", "group6" = "#0000c2",
                             "group7" = "#ff68ad", "group8" = "#F0E442", "group9" = "#6f1c00")

    if (!is.null(hovered_measurements) && length(hovered_measurements) > 0) {
      dens <- density(hovered_measurements, na.rm = TRUE)
      plot(dens, main = paste("Density Plot for", hovered_compound, "\nP-value:", format(shapiro_p_value, digits = 4)), xlab = "Measurements", ylab = "Density", col = 'blue')
      polygon(dens, col = ifelse(input$normality_grouping == "grouped data", fill_color_palette[substr(hovered_compound, 1, 6)], "skyblue"), border = ifelse(input$normality_grouping == "grouped data", group_color_palette[substr(hovered_compound, 1, 6)], "darkblue"))
    } else {
      plot.new()
      text(0.5, 0.5, "No measurements available", cex = 1.5)
    }
  })


  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$percent_above_005 <- renderText({
        req(shapiro_data_stored(), input$normality_norm, input$normality_trans, input$normality_scale)
        joined_data <- shapiro_data_stored()
        percent_above_005 <- mean(joined_data$P_Val > 0.05, na.rm = TRUE) * 100
        paste0("% of P-Values Above 0.05: ", round(percent_above_005, 6), " %")
      })
    } else {
      output$percent_above_005 <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$highest_p_val <- renderText({
        req(shapiro_data_stored(), input$normality_norm, input$normality_trans, input$normality_scale)
        joined_data <- shapiro_data_stored()
        high_p_val <- joined_data %>%
          filter(P_Val == max(P_Val, na.rm = TRUE)) %>%
          dplyr::select(Compound, P_Val)
        paste0("Highest p-value: ", as.character(high_p_val$Compound), ", ", round(as.numeric(high_p_val$P_Val), 6))
      })
    } else {
      output$highest_p_val <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Normality") {
      output$lowest_p_val <- renderText({
        req(shapiro_data_stored(), input$normality_norm, input$normality_trans, input$normality_scale)
        joined_data <- shapiro_data_stored()
        low_p_val <- joined_data %>%
          filter(P_Val == min(P_Val, na.rm = TRUE)) %>%
          dplyr::select(Compound, P_Val)
        paste0("Lowest p-value: ", as.character(low_p_val$Compound), ", ", round(as.numeric(low_p_val$P_Val), 6))
      })
    } else {
      output$lowest_p_val <- renderText({
        ""
      })
    }
  })

  # Levene's page buttons
  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$leve_prev_page_button <- renderUI({
        tagList(br(),
                actionButton("leve_prev_page", "Previous 50 Compounds"),
                br()
        )
      })
    } else {
      output$leve_prev_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$leve_next_page_button <- renderUI({
        tagList(
          actionButton("leve_next_page", "Next 50 Compounds")
        )
      })
    } else {
      output$leve_next_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$leve_page_info_text <- renderUI({
        tagList(textOutput("leve_page_info"),
                br()
        )
      })
    } else {
      output$leve_page_info_text <- renderUI({
        div()
      })
    }
  })

  # Render Levene's test
  leve_current_page <- reactiveVal(1)
  leve_items_per_page <- 50

  leve_total_pages <- reactive(ceiling(nrow(processed_data()) / leve_items_per_page))

  observeEvent(input$leve_prev_page, {
    leve_current_page(max(1, leve_current_page() - 1))
  })
  observeEvent(input$leve_next_page, {
    leve_current_page(min(leve_total_pages(), leve_current_page() + 1))
  })
  output$leve_page_info <- renderText({
    paste("Page", leve_current_page(), "of", leve_total_pages())
  })

  levene_data_stored <- reactiveVal(NULL)

  levene_func <- reactive({
    req(processed_data(), leve_current_page(), common_column(), input$homoscedas_norm, input$homoscedas_trans, input$homoscedas_scale, input$homoscedas_sort)
    isolate({
      stat_data <- processed_data()
      common_column_value <- common_column()
      original_order <- setNames(seq_along(stat_data$Compound), stat_data$Compound)

      long_data <- stat_data %>%
        pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
        separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

      if (input$homoscedas_norm == 'by Sum') {
        long_data <- long_data %>%
          group_by(sample) %>%
          mutate(sum_value = sum(value, na.rm = TRUE),
                 value = ifelse(value == 0 | is.na(value), value, value / sum_value)) %>%
          select(-sum_value)
      } else if (input$homoscedas_norm == 'by Median') {
        long_data <- long_data %>%
          group_by(sample) %>%
          mutate(median_value = median(value, na.rm = TRUE),
                 value = ifelse(value == 0 | is.na(value), value, value / median_value)) %>%
          select(-median_value)
      } else if (input$homoscedas_norm == 'Quantile') {
        output_data <- quantile_normalize(stat_data)
        long_data <- output_data %>%
          pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
          separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")
      } else if (input$homoscedas_norm == 'None') {
        long_data <- long_data
      }

      if (input$homoscedas_trans == 'Log(5)') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 5)))
      } else if (input$homoscedas_trans == 'Log(10)') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 10)))
      } else if (input$homoscedas_trans == 'Square') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, value^2))
      } else if (input$homoscedas_trans == 'Cube') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, value^3))
      } else if (input$homoscedas_trans == 'Square root') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, sqrt(value)))
      } else if (input$homoscedas_trans == 'Cube root') {
        long_data <- long_data %>%
          mutate(value = ifelse(value == 0 | is.na(value), value, value^(1/3)))
      } else {
        long_data <- long_data
      }

      stat_data <- long_data %>%
        unite(sample, group, sample, sep = "_") %>%
        pivot_wider(names_from = sample, values_from = value)

      if (input$homoscedas_scale == 'Center on Mean') {
        centered_data <- stat_data[,-1]
        for (i in 1:nrow(centered_data)) {
          centered_data[i, ] <- centered_data[i, ] - rowMeans(centered_data[i, ], na.rm = TRUE)
        }
        stat_data <- cbind(stat_data[1], centered_data)
      } else if (input$homoscedas_scale == 'Auto') {
        centered_data <- stat_data[, -1]
        for (i in 1:nrow(centered_data)) {
          row_sd <- sd(as.numeric(centered_data[i, ]), na.rm = TRUE)
          if (row_sd != 0) {
            row_mean <- mean(as.numeric(centered_data[i, ]), na.rm = TRUE)
            centered_data[i, ] <- (centered_data[i, ] - row_mean) / row_sd
          } else {
            centered_data[i, ] <- 0
          }
        }
        stat_data <- cbind(stat_data[, 1], centered_data)
      } else if (input$homoscedas_scale == 'None') {
        stat_data <- stat_data
      }

      original_order <- setNames(seq_along(stat_data[[common_column_value]]), stat_data[[common_column_value]])
      results_list <- list()

      long_data <- stat_data %>%
        pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
        separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

      unique_compounds <- unique(long_data[[common_column()]])

      for (compound_name in unique_compounds) {
        compound_data <- long_data[long_data[[common_column()]] == compound_name,]

        if (nrow(compound_data) > 2 && length(unique(compound_data$group)) > 1) {

          test_result <- leveneTest(value ~ as.factor(group), data = compound_data)
          f_value <- ifelse(!is.null(test_result$`F value`[1]), test_result$`F value`[1], NA)
          pr <- ifelse(!is.null(test_result$`Pr(>F)`[1]), test_result$`Pr(>F)`[1], NA)

          results_list[[compound_name]] <- list(
            Compound = compound_name,
            F_Value = f_value,
            Pr_greater_than_F = pr
          )
        } else {
          results_list[[compound_name]] <- list(
            Compound = compound_name,
            F_Value = NA,
            Pr_greater_than_F = NA
          )
        }
      }

      results_df <- do.call(rbind, lapply(results_list, function(item) {
        temp_df <- as.data.frame(t(unlist(item)), stringsAsFactors = FALSE)
        colnames(temp_df) <- c("Compound", "F_Value", "Pr_greater_than_F")
        return(temp_df)
      }))

      results_df$Compound <- as.character(results_df$Compound)
      results_df$F_Value <- as.numeric(as.character(results_df$F_Value))
      results_df$Pr_greater_than_F <- as.numeric(as.character(results_df$Pr_greater_than_F))

      results_df$Order <- original_order[results_df$Compound]
      results_df <- results_df[order(results_df$Order), ]
      results_df <- results_df %>%
        mutate(Max_Order = rank(desc(Pr_greater_than_F)))

      results_df <- results_df %>%
        dplyr::select(Order, Max_Order, Compound, F_Value, Pr_greater_than_F)

      if (input$homoscedas_sort == 'Max') {
        results_df <- results_df %>%
          arrange(desc(Pr_greater_than_F))
        results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order, decreasing = TRUE)])
      } else if (input$homoscedas_sort == 'Min') {
        results_df <- results_df %>%
          arrange(Pr_greater_than_F)
        results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order)])
      } else if (input$homoscedas_sort == 'None') {
        results_df$Compound <- factor(results_df$Compound, levels = rev(results_df$Compound[order(results_df$Order)]))
      }

      start <- (leve_current_page() - 1) * items_per_page + 1
      end <- min(nrow(results_df), leve_current_page() * items_per_page)
      display_data <- results_df[start:end, ]

      joined_data <- left_join(results_df, stat_data, by = c("Compound" = common_column_value))
      levene_data_stored(joined_data)

      plot <- ggplot(display_data, aes(x = Compound, y = Pr_greater_than_F)) +
        geom_segment(aes(xend = Compound, y = 0, yend = Pr_greater_than_F), color = "skyblue") +
        geom_point(color = "skyblue", size = 3) +
        coord_flip() +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkred") +
        labs(y = "P-Value", x = "Compound", title = "Levene's Homoscedasticity Test P-Values") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 11),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              plot.title = element_text(size = 17, hjust = 0.5))

      plot
    })
  })

  output$levene_data <- renderPlot({
    levene_func()
  }, height = 550)

  # output$levene_data <- DT::renderDT({
  #   req(levene_func())
  #   DT::datatable(
  #     levene_func(),
  #     options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })

  output$hover_beanplot <- renderPlot({
    req(input$levene_hover, levene_data_stored(), leve_current_page(), processed_data(), input$homoscedas_norm, input$homoscedas_trans, input$homoscedas_scale, input$homoscedas_sort)
    hover <- input$levene_hover
    levene_data <- levene_data_stored()
    plot_data <- levene_data %>%
      dplyr::select(Compound, 6:ncol(.))

    items_per_page <- 50
    page_offset <- (leve_current_page() - 1) * items_per_page
    index_on_page <- items_per_page - round(hover$y) + 1
    index <- page_offset + index_on_page
    if (index < 1 || index > nrow(levene_data)) return()

    hovered_compound <- levene_data[index, "Compound"]
    levene_data_p_value <- levene_data[index, "Pr_greater_than_F"]
    hovered_measurements <- plot_data[index,]

    long_data <- hovered_measurements %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

    if (nrow(long_data) > 0) {
      o <- ggplot(long_data, aes(x = group, y = value, color = group)) +
        geom_boxplot(width = 0.2) +
        geom_beeswarm() +
        labs(title = paste("Beeswarm Plot for", hovered_compound)) +
        theme_minimal()

      p <- beanplot(value ~ group, data = long_data,
                    main = paste("Beanplot for", hovered_compound, "\nP-value:", format(levene_data_p_value, digits = 4)),
                    xlab = "Groups",
                    ylab = "Values",
                    beanlines = "median")

      if (input$swarm_check == FALSE) {
          print(p)
        } else {
          print(o)
        }

    } else {
      plot.new()
      text(0.5, 0.5, "No measurements available", cex = 1.5)
    }
  })




  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$levene_percent_above_005 <- renderText({
        req(levene_data_stored(), input$homoscedas_norm, input$homoscedas_trans, input$homoscedas_scale)
        joined_data <- levene_data_stored()
        percent_above_005 <- mean(joined_data$Pr_greater_than_F > 0.05, na.rm = TRUE) * 100
        paste0("% of P-Values Above 0.05: ", round(percent_above_005, 6), " %")
      })
    } else {
      output$levene_percent_above_005 <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$levene_highest_p_val <- renderText({
        req(levene_data_stored(), input$homoscedas_norm, input$homoscedas_trans, input$homoscedas_scale)
        joined_data <- levene_data_stored()
        high_p_val <- joined_data %>%
          filter(Pr_greater_than_F == max(Pr_greater_than_F, na.rm = TRUE)) %>%
          dplyr::select(Compound, Pr_greater_than_F)
        paste0("Highest p-value: ", as.character(high_p_val$Compound), ", ", round(as.numeric(high_p_val$Pr_greater_than_F), 6))
      })
    } else {
      output$levene_highest_p_val <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Homoscedasticity") {
      output$levene_lowest_p_val <- renderText({
        req(levene_data_stored(), input$homoscedas_norm, input$homoscedas_trans, input$homoscedas_scale)
        joined_data <- levene_data_stored()
        low_p_val <- joined_data %>%
          filter(Pr_greater_than_F == min(Pr_greater_than_F, na.rm = TRUE)) %>%
          dplyr::select(Compound, Pr_greater_than_F)
        paste0("Lowest p-value: ", as.character(low_p_val$Compound), ", ", round(as.numeric(low_p_val$Pr_greater_than_F), 6))
      })
    } else {
      output$levene_lowest_p_val <- renderText({
        ""
      })
    }
  })



  # Parametric test's dropbox & buttons

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_sort <- renderUI({
        selectInput("para_sorting", "Sort data:", c("None", "Min", "Max"))
      })
    } else {
      output$para_sort <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_norm <- renderUI({
        selectInput("para_normalize", "Normalize data:", c("None", "by Sum", "by Median", "Quantile"))
      })
    } else {
      output$para_norm <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_transf <- renderUI({
        selectInput("para_trans", "Transform data:", c("None", "Log(5)", "Log(10)", "Square root", "Cube root", "Square", "Cube"))
      })
    } else {
      output$para_transf <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_scale <- renderUI({
        selectInput("para_sca", "Scale data:", c("None", "Center on Mean", "Auto"))
      })
    } else {
      output$para_scale <- renderUI({
        div()
      })
    }
  })


  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests" && group_counter() > 2) {
      output$tukey_checkbox <- renderUI({
        checkboxInput('tukey_check', 'Tukey HSD Test', value = FALSE)
      })
    } else {
      output$tukey_checkbox <- renderUI({
        div()
      })
    }
  })


  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_prev_page_button <- renderUI({
        tagList(br(),
                actionButton("para_prev_page", "Previous 50 Compounds"),
                br()
        )
      })
    } else {
      output$para_prev_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_next_page_button <- renderUI({
        tagList(
          actionButton("para_next_page", "Next 50 Compounds")
        )
      })
    } else {
      output$para_next_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_page_info_text <- renderUI({
        tagList(textOutput("para_page_info"),
                br()
        )
      })
    } else {
      output$para_page_info_text <- renderUI({
        div()
      })
    }
  })

  # Parametric tests
  para_current_page <- reactiveVal(1)
  para_items_per_page <- 50

  para_total_pages <- reactive(ceiling(nrow(processed_data()) / para_items_per_page))

  observeEvent(input$para_prev_page, {
    para_current_page(max(1, para_current_page() - 1))
  })
  observeEvent(input$para_next_page, {
    para_current_page(min(para_total_pages(), para_current_page() + 1))
  })
  output$para_page_info <- renderText({
    paste("Page", para_current_page(), "of", para_total_pages())
  })

  para_data_stored <- reactiveVal(NULL)


  observe({
    if (!is.null(para_data_stored()) && active_tab() == "Parametric tests") {
      output$para_download <- renderUI({
        tagList(
          br(),
          tags$p("Download as data table"),
          downloadHandler(
            filename = function() {
              current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
              paste0("metabo_parametric_", current_datetime, ".csv")
            },
            content = function(file) {
              write.csv(para_data_stored(), file)
            }
          ))
      })
    } else {
      output$para_download <- renderUI({
        div()
      })
    }
  })

  tukey_results_list_stored <- reactiveVal(NULL)
  tukey_results_df_download <- reactiveVal(NULL)
  tukey_summary_df_download <- reactiveVal(NULL)

  observe({
    if (!is.null(processed_data()) && !is.null(tukey_results_df_download()) && active_tab() == "Parametric tests" && input$tukey_check == TRUE) {
      output$tukey_download <- renderUI({
        tagList(
          tags$p("Download Tukey test results"),
          downloadHandler(
            filename = function() {
              current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
              paste0("tukey_results_", current_datetime, ".csv")
            },
            content = function(file) {
              write.csv(tukey_results_df_download(), file)
            }
          ),
          br())
      })
    } else {
      output$tukey_download <- renderUI({
        div()
      })
    }
  })


  para_func <- reactive({
    req(processed_data(), para_current_page(), input$para_normalize, input$para_trans, input$para_sca, input$para_sorting)
    short_data <- processed_data()
    original_order <- setNames(seq_along(short_data$Compound), short_data$Compound)
    common_column_value <- common_column()
    #para_check <- input$para_select
    #fdr <- input$fdr_check

    long_data <- short_data %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

    if (input$para_normalize == 'by Sum') {
      long_data <- long_data %>%
        group_by(sample) %>%
        mutate(sum_value = sum(value, na.rm = TRUE),
               value = ifelse(value == 0 | is.na(value), value, value / sum_value)) %>%
        select(-sum_value)
    } else if (input$para_normalize == 'by Median') {
      long_data <- long_data %>%
        group_by(sample) %>%
        mutate(median_value = median(value, na.rm = TRUE),
               value = ifelse(value == 0 | is.na(value), value, value / median_value)) %>%
        select(-median_value)
    } else if (input$para_normalize == 'Quantile') {
      output_data <- quantile_normalize(short_data)
      long_data <- output_data %>%
        pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
        separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")
    } else if (input$para_normalize == 'None') {
      long_data <- long_data
    }

    if (input$para_trans == 'Log(5)') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 5)))
    } else if (input$para_trans == 'Log(10)') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 10)))
    } else if (input$para_trans == 'Square') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, value^2))
    } else if (input$para_trans == 'Cube') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, value^3))
    } else if (input$para_trans == 'Square root') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, sqrt(value)))
    } else if (input$para_trans == 'Cube root') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, value^(1/3)))
    } else if (input$para_trans == 'None') {
      long_data <- long_data
    }

    short_data <- long_data %>%
      unite(sample, group, sample, sep = "_") %>%
      pivot_wider(names_from = sample, values_from = value)

    if (input$para_sca == 'Center on Mean') {
      centered_data <- short_data[,-1]
      for (i in 1:nrow(centered_data)) {
        centered_data[i, ] <- centered_data[i, ] - rowMeans(centered_data[i, ], na.rm = TRUE)
      }
      short_data <- cbind(short_data[1], centered_data)
    } else if (input$para_sca == 'Auto') {
      centered_data <- short_data[, -1]
      for (i in 1:nrow(centered_data)) {
        row_sd <- sd(as.numeric(centered_data[i, ]), na.rm = TRUE)
        if (row_sd != 0) {
          row_mean <- mean(as.numeric(centered_data[i, ]), na.rm = TRUE)
          centered_data[i, ] <- (centered_data[i, ] - row_mean) / row_sd
        } else {
          centered_data[i, ] <- 0
        }
      }
      short_data <- cbind(short_data[, 1], centered_data)
    } else if (input$para_sca == 'None') {
      short_data <- short_data
    }

    long_data <- short_data %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

    num_groups <- long_data %>%
      distinct(group) %>%
      nrow()

    if (num_groups > 2) {
      results <- long_data %>%
        group_by(.data[[common_column_value]]) %>%
        summarise(p_value = anova(lm(value ~ group))$"Pr(>F)"[1]) %>%
        arrange(p_value) %>%
        mutate(p_value = {
          if (input$correction_check == "None") {
            p.adjust(p_value, method = "none")
          } else if (input$correction_check == "Benjamini & Hochberg") {
            p.adjust(p_value, method = "fdr")
          } else if (input$correction_check == "Bonferroni") {
            p.adjust(p_value, method = "bonferroni")
          } else {
            p.adjust(p_value, method = "none")
          }
        })
    } else {
      results <- long_data %>%
        group_by(.data[[common_column_value]]) %>%
        summarise(p_value = t.test(value ~ group)$p.value) %>%
        arrange(p_value) %>%
        mutate(p_value = {
          if (input$correction_check == "None") {
            p.adjust(p_value, method = "none")
          } else if (input$correction_check == "Benjamini & Hochberg") {
            p.adjust(p_value, method = "fdr")
          } else if (input$correction_check == "Bonferroni") {
            p.adjust(p_value, method = "bonferroni")
          } else {
            p.adjust(p_value, method = "none")
          }
        })
    }

    results_df <- data.frame(results)

    names(results_df)[1] <- "Compound"
    results_df$p_value <- as.numeric(results_df$p_value)

    process_tukey_results <- function(tukey_results_list) {
      results <- lapply(names(tukey_results_list), function(compound) {
        result <- tukey_results_list[[compound]]$group
        result_df <- as.data.frame(result)
        result_df$Comparison <- rownames(result_df)
        result_df$Compound <- compound
        return(result_df)
      })
      combined_results <- bind_rows(results)
      return(combined_results)
    }

    if (num_groups > 2 && input$tukey_check == TRUE) {
      tukey_results_list <- list()
      for (compound in results_df$Compound) {
        compound_data <- long_data %>% filter(.data[[common_column_value]] == compound)
        anova_model <- aov(value ~ group, data = compound_data)
        tukey_result <- TukeyHSD(anova_model)
        tukey_results_list[[compound]] <- tukey_result
      }

    tukey_results_df <- process_tukey_results(tukey_results_list)
    rownames(tukey_results_df) <- NULL

    tukey_results_df <- tukey_results_df %>%
      select(Compound, diff, lwr, upr, `p adj`, Comparison)

    tukey_results_df_download(tukey_results_df)

      tukey_summary_df <- tukey_results_df %>%
        group_by(Comparison) %>%
        summarise(
          count_significant_p_adj = sum(`p adj` < 0.05),
          percent_p_adj_below_0_05 = mean(`p adj` < 0.05) * 100,
          mean_diff = mean(diff),
          mean_lwr = mean(lwr),
          mean_upr = mean(upr),
          mean_p_adj = mean(`p adj`)
        )

    tukey_summary_df_download(tukey_summary_df)
    } else {
      tukey_results_list <- list()
    }

    tukey_results_list_stored(tukey_results_list)


    results_df$Order <- original_order[results_df$Compound]
    results_df <- results_df[order(results_df$Order), ]
    results_df <- results_df %>%
      mutate(Max_Order = dense_rank(desc(p_value)))

    results_df <- results_df %>%
      dplyr::select(Order, Max_Order, Compound, p_value)

    if (input$para_sorting == 'Max') {
      results_df <- results_df %>%
        arrange(desc(p_value))
      results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order, decreasing = TRUE)])
    } else if (input$para_sorting == 'Min') {
      results_df <- results_df %>%
        arrange(p_value)
      results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order)])
    } else if (input$para_sorting == 'None') {
      results_df$Compound <- factor(results_df$Compound, levels = rev(results_df$Compound[order(results_df$Order)]))
    }

    start <- (para_current_page() - 1) * para_items_per_page + 1
    end <- min(nrow(results_df), para_current_page() * para_items_per_page)
    display_data <- results_df[start:end, ]

    joined_data <- left_join(results_df, short_data, by = c("Compound" = common_column_value))
    para_data_stored(joined_data)

    if (num_groups > 2) {
      para_title <- "ANOVA Test Results"
    } else {
      para_title <- "T-Test Results"
    }

    plot <- ggplot(display_data, aes(x = factor(Compound, levels = rev(display_data$Compound)), y = p_value)) +
      geom_segment(aes(xend = factor(Compound, levels = rev(display_data$Compound)), yend = 0), color = "skyblue") +
      geom_point(color = "skyblue", size = 3) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkred") +
      coord_flip() +
      labs(y = "P-Value", x = "Compound", title = para_title) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 11),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size = 17, hjust = 0.5))
    plot
  })

  # output$para_data <- DT::renderDT({
  #   req(para_func(), input$para_select)
  #   DT::datatable(
  #     para_func(),
  #     options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })

  output$para_data <- renderPlot({
    req(processed_data(), para_current_page(), input$para_normalize)
    if (!is.null(para_func())) {
      return(para_func())
    }
    NULL
  }, height = 550)

  output$hover_para_hist <- renderPlot({
    req(input$para_hover, processed_data(), para_data_stored(), para_current_page(), input$para_trans, input$para_normalize, input$para_sca, input$para_sorting)
    hover <- input$para_hover
    stat_data <- processed_data()
    para_data <- para_data_stored()
    plot_data <- para_data %>%
      dplyr::select(Compound, 5:ncol(.))

    para_items_per_page <- 50
    page_offset <- (para_current_page() - 1) * para_items_per_page
    index_on_page <- para_items_per_page - round(hover$y) + 1
    index <- page_offset + index_on_page
    if (index < 1 || index > nrow(para_data)) return()

    hovered_compound <- para_data[index, "Compound"]
    para_data_p_value <- para_data[index, "p_value"]
    hovered_measurements <- plot_data[index,]

    long_data <- hovered_measurements %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

    if (!is.null(hovered_measurements) && length(hovered_measurements) > 0) {
      dummy <- long_data %>%
        group_by(group) %>%
        summarize(mean = mean(value))

      ggplot(long_data, aes(x = value, fill = group)) +
        geom_density(alpha = 0.5) +
        geom_vline(data = dummy, aes(xintercept = mean, color = group), linetype = "longdash") +
        labs(title = paste("Density Plot for", hovered_measurements$Compound, "\nP-value:", format(para_data_p_value, digits = 4)),
             x = "Measurements",
             y = "Density") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No measurements available", cex = 1.5)
    }
  })


  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_percent_above_005 <- renderText({
        req(para_data_stored(), input$para_trans, input$para_normalize, input$para_sca)
        joined_data <- para_data_stored()
        percent_above_005 <- mean(joined_data$p_value > 0.05, na.rm = TRUE) * 100
        paste0("% of P-Values Above 0.05: ", round(percent_above_005, 6), " %")
      })
    } else {
      output$para_percent_above_005 <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_highest_p_val <- renderText({
        req(para_data_stored(), input$para_trans, input$para_normalize, input$para_sca)
        joined_data <- para_data_stored()
        high_p_val <- joined_data %>%
          filter(p_value == max(p_value, na.rm = TRUE)) %>%
          dplyr::select(Compound, p_value)
        paste0("Highest p-value: ", as.character(high_p_val$Compound), ", ", round(as.numeric(high_p_val$p_value), 6))
      })
    } else {
      output$para_highest_p_val <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Parametric tests") {
      output$para_lowest_p_val <- renderText({
        req(para_data_stored(), input$para_trans, input$para_normalize, input$para_sca)
        joined_data <- para_data_stored()
        low_p_val <- joined_data %>%
          filter(p_value == min(p_value, na.rm = TRUE)) %>%
          dplyr::select(Compound, p_value)
        paste0("Lowest p-value: ", as.character(low_p_val$Compound), ", ", round(as.numeric(low_p_val$p_value), 6))
      })
    } else {
      output$para_lowest_p_val <- renderText({
        ""
      })
    }
  })


    output$tukey_results_table <- DT::renderDT({
      req(tukey_summary_df_download(), input$tukey_check)
        DT::datatable(
          tukey_summary_df_download(),
          options = list(pageLength = 10, scrollX = TRUE))
      })



  observe({
    req(input$tukey_check)
    if (!is.null(processed_data()) && active_tab() == "Parametric tests" && input$tukey_check == TRUE) {
      output$tukey_results <- renderPlot({
        req(tukey_results_list_stored(), input$para_hover, processed_data(), para_data_stored(), para_current_page(), input$para_trans, input$para_normalize, input$para_sca, input$para_sorting)

        extract_tukey_results <- function(tukey_list) {
          tukey_df <- do.call(rbind, lapply(names(tukey_list), function(compound) {
            tukey_result <- tukey_list[[compound]]$group
            tukey_df <- as.data.frame(tukey_result)
            tukey_df$Comparison <- rownames(tukey_result)
            tukey_df$Compound <- compound
            return(tukey_df)
          }))
          rownames(tukey_df) <- NULL
          return(tukey_df)
        }

        if (is.null(tukey_results_list_stored()) || length(tukey_results_list_stored()) == 0) return()

        tukey_results_df <- extract_tukey_results(tukey_results_list_stored())

        hover <- input$para_hover
        stat_data <- processed_data()
        para_data <- para_data_stored()
        plot_data <- para_data %>%
          dplyr::select(Compound, 5:ncol(.))

        para_items_per_page <- 50
        page_offset <- (para_current_page() - 1) * para_items_per_page
        index_on_page <- para_items_per_page - round(hover$y) + 1
        index <- page_offset + index_on_page
        if (index < 1 || index > nrow(para_data)) return()

        hovered_compound <- para_data[index, "Compound"]

        if (nrow(tukey_results_df) == 0) return()

        tukey_results_df <- tukey_results_df %>%
          dplyr::filter(Compound == hovered_compound)

        if (nrow(tukey_results_df) == 0) return()

        ggplot(tukey_results_df, aes(x = Comparison, y = diff)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_text(aes(label = paste("p-value: ", round(`p adj`, 4))), vjust = -0.5) +
          labs(title = "Tukey HSD Test: Mean Differences Between Groups",
               x = "Comparison", y = "Mean Difference") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          expand_limits(y = max(tukey_results_df$diff) * 1.1)
      })
    } else if (input$tukey_check == FALSE) {
      output$tukey_results <- renderUI({
        div()
      })
    }
  })


  # Non-parametric test's dropbox & buttons

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_sort <- renderUI({
        selectInput("non_para_sorting", "Sort data:", c("None", "Min", "Max"))
      })
    } else {
      output$non_para_sort <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_norm <- renderUI({
        selectInput("non_para_norm", "Normalize data:", c("None", "by Sum", "by Median", "Quantile"))
      })
    } else {
      output$non_para_norm <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_transf <- renderUI({
        selectInput("non_para_trans", "Transform data:", c("None", "Log(5)", "Log(10)", "Square root", "Cube root", "Square", "Cube"))
      })
    } else {
      output$non_para_transf <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_scale <- renderUI({
        selectInput("non_para_sca", "Scale data:", c("None", "Center on Mean", "Auto"))
      })
    } else {
      output$non_para_scale <- renderUI({
        div()
      })
    }
  })


  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_prev_page_button <- renderUI({
        tagList(br(),
                actionButton("non_para_prev_page", "Previous 50 Compounds"),
                br()
        )
      })
    } else {
      output$non_para_prev_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_next_page_button <- renderUI({
        tagList(
          actionButton("non_para_next_page", "Next 50 Compounds")
        )
      })
    } else {
      output$non_para_next_page_button <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_page_info_text <- renderUI({
        tagList(textOutput("non_para_page_info"),
                br()
        )
      })
    } else {
      output$non_para_page_info_text <- renderUI({
        div()
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests" && group_counter() > 2) {
      output$non_tukey_checkbox <- renderUI({
        checkboxInput('non_tukey_check', 'Dunn Test', value = FALSE)
      })
    } else {
      output$non_tukey_checkbox <- renderUI({
        div()
      })
    }
  })

  # Non-parametric tests
  non_para_current_page <- reactiveVal(1)
  non_para_items_per_page <- 50

  non_para_total_pages <- reactive(ceiling(nrow(processed_data()) / non_para_items_per_page))

  observeEvent(input$non_para_prev_page, {
    non_para_current_page(max(1, non_para_current_page() - 1))
  })
  observeEvent(input$non_para_next_page, {
    non_para_current_page(min(non_para_total_pages(), non_para_current_page() + 1))
  })
  output$non_para_page_info <- renderText({
    paste("Page", non_para_current_page(), "of", non_para_total_pages())
  })

  non_para_data_stored <- reactiveVal(NULL)

  observe({
    if (!is.null(non_para_data_stored()) && active_tab() == "Non-Parametric tests") {
      output$non_para_download <- renderUI({
        tagList(
          br(),
          tags$p("Download as data table"),
          downloadHandler(
            filename = function() {
              current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
              paste0("metabo_non_parametric_", current_datetime, ".csv")
            },
            content = function(file) {
              write.csv(non_para_data_stored(), file)
            }
          ))
      })
    } else {
      output$non_para_download <- renderUI({
        div()
      })
    }
  })

  non_tukey_results_list_stored <- reactiveVal(NULL)
  dunn_results_df_download <- reactiveVal(NULL)
  dunn_summary_df_download <- reactiveVal(NULL)

  observe({
    if (!is.null(processed_data()) && !is.null(dunn_results_df_download()) && active_tab() == "Non-Parametric tests" && input$non_tukey_check == TRUE) {
      output$dunn_download <- renderUI({
        tagList(
          tags$p("Download Dunn test results"),
          downloadHandler(
            filename = function() {
              current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
              paste0("dunn_results_", current_datetime, ".csv")
            },
            content = function(file) {
              write.csv(dunn_results_df_download(), file)
            }
          ),
          br())
      })
    } else {
      output$dunn_download <- renderUI({
        div()
      })
    }
  })


  non_para_func <- reactive({
    req(processed_data(), non_para_current_page(), input$non_para_norm, input$non_para_trans, input$non_para_sca, input$non_para_sorting)
    short_data <- processed_data()
    original_order <- setNames(seq_along(short_data$Compound), short_data$Compound)
    common_column_value <- common_column()
    #non_para_check <- input$non_para_select
    #fdr <- input$fdr_check

    long_data <- short_data %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")


    if (input$non_para_norm == 'by Sum') {
      long_data <- long_data %>%
        group_by(sample) %>%
        mutate(sum_value = sum(value, na.rm = TRUE),
               value = ifelse(value == 0 | is.na(value), value, value / sum_value)) %>%
        select(-sum_value)
    } else if (input$non_para_norm == 'by Median') {
      long_data <- long_data %>%
        group_by(sample) %>%
        mutate(median_value = median(value, na.rm = TRUE),
               value = ifelse(value == 0 | is.na(value), value, value / median_value)) %>%
        select(-median_value)
    } else if (input$non_para_norm == 'Quantile') {
      output_data <- quantile_normalize(short_data)
      long_data <- output_data %>%
        pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
        separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")
    } else if (input$non_para_norm == 'None') {
      long_data <- long_data
    }

    if (input$non_para_trans == 'Log(5)') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 5)))
    } else if (input$non_para_trans == 'Log(10)') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, log(value, base = 10)))
    } else if (input$non_para_trans == 'Square') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, value^2))
    } else if (input$non_para_trans == 'Cube') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, value^3))
    } else if (input$non_para_trans == 'Square root') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, sqrt(value)))
    } else if (input$non_para_trans == 'Cube root') {
      long_data <- long_data %>%
        mutate(value = ifelse(value == 0 | is.na(value), value, value^(1/3)))
    } else {
      long_data <- long_data
    }

    short_data <- long_data %>%
      unite(sample, group, sample, sep = "_") %>%
      pivot_wider(names_from = sample, values_from = value)

    if (input$non_para_sca == 'Center on Mean') {
      centered_data <- short_data[,-1]
      for (i in 1:nrow(centered_data)) {
        centered_data[i, ] <- centered_data[i, ] - rowMeans(centered_data[i, ], na.rm = TRUE)
      }
      short_data <- cbind(short_data[1], centered_data)
    } else if (input$non_para_sca == 'Auto') {
      centered_data <- short_data[, -1]
      for (i in 1:nrow(centered_data)) {
        row_sd <- sd(as.numeric(centered_data[i, ]), na.rm = TRUE)
        if (row_sd != 0) {
          row_mean <- mean(as.numeric(centered_data[i, ]), na.rm = TRUE)
          centered_data[i, ] <- (centered_data[i, ] - row_mean) / row_sd
        } else {
          centered_data[i, ] <- 0
        }
      }
      short_data <- cbind(short_data[, 1], centered_data)
    } else if (input$non_para_sca == 'None') {
      short_data <- short_data
    }

    long_data <- short_data %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

    num_groups <- long_data %>%
      distinct(group) %>%
      nrow()

    if (num_groups > 2) {
      results <- long_data %>%
        group_by(.data[[common_column_value]]) %>%
        summarise(p_value = kruskal.test(value ~ group)$p.value) %>%
        arrange(p_value) %>%
        mutate(p_value = {
          if (input$correction_check == "None") {
            p.adjust(p_value, method = "none")
          } else if (input$correction_check == "Benjamini & Hochberg") {
            p.adjust(p_value, method = "fdr")
          } else if (input$correction_check == "Bonferroni") {
            p.adjust(p_value, method = "bonferroni")
          } else {
            p.adjust(p_value, method = "none")
          }
        })
    } else {
      results <- long_data %>%
        group_by(.data[[common_column_value]]) %>%
        summarise(p_value = wilcox.test(value ~ group)$p.value) %>%
        arrange(p_value) %>%
        mutate(p_value = {
          if (input$correction_check == "None") {
            p.adjust(p_value, method = "none")
          } else if (input$correction_check == "Benjamini & Hochberg") {
            p.adjust(p_value, method = "fdr")
          } else if (input$correction_check == "Bonferroni") {
            p.adjust(p_value, method = "bonferroni")
          } else {
            p.adjust(p_value, method = "none")
          }
        })
    }

    results_df <- data.frame(results)
    names(results_df)[1] <- "Compound"
    results_df$p_value <- as.numeric(results_df$p_value)

    if (num_groups > 2 && input$non_tukey_check == TRUE) {
      dunn_results_list <- list()

      for (compound in results_df$Compound) {
        compound_data <- long_data %>% filter(.data[[common_column_value]] == compound)
        kruskal_result <- kruskal.test(value ~ group, data = compound_data)
        dunn_result <- dunn.test(compound_data$value, compound_data$group, method = "bonferroni")

        dunn_df <- data.frame(
          Comparison = paste(dunn_result$comparisons),
          Z = dunn_result$Z,
          P.adj = dunn_result$P.adjusted,
          Compound = compound,
          stringsAsFactors = FALSE
        )

        dunn_results_list[[compound]] <- dunn_df
      }

      combined_dunn_df <- do.call(rbind, dunn_results_list)

      combined_dunn_df$Comparison <- as.character(combined_dunn_df$Comparison)
      combined_dunn_df$P.adj <- as.numeric(combined_dunn_df$P.adj)
      combined_dunn_df$Z <- as.numeric(combined_dunn_df$Z)
      combined_dunn_df$Compound <- as.character(combined_dunn_df$Compound)

      dunn_summary_df <- combined_dunn_df %>%
        group_by(Comparison) %>%
        summarise(
          count_significant_p_adj = sum(P.adj < 0.05),
          percent_p_adj_below_0_05 = mean(P.adj < 0.05) * 100,
          mean_Z = mean(Z),
          mean_p_adj = mean(P.adj)
        )

      dunn_summary_df_download(dunn_summary_df)
      dunn_results_df_download(combined_dunn_df)
      non_tukey_results_list_stored(dunn_results_list)

    } else {
      non_tukey_results_list_stored(list())
    }


    results_df$Order <- original_order[results_df$Compound]
    results_df <- results_df[order(results_df$Order), ]
    results_df <- results_df %>%
      mutate(Max_Order = rank(desc(p_value)))

    results_df <- results_df %>%
      dplyr::select(Order, Max_Order, Compound, p_value)

    if (input$non_para_sorting == 'Max') {
      results_df <- results_df %>%
        arrange(desc(p_value))
      results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order, decreasing = TRUE)])
    } else if (input$non_para_sorting == 'Min') {
      results_df <- results_df %>%
        arrange(p_value)
      results_df$Compound <- factor(results_df$Compound, levels = results_df$Compound[order(results_df$Max_Order)])
    } else if (input$non_para_sorting == 'None') {
      results_df$Compound <- factor(results_df$Compound, levels = rev(results_df$Compound[order(results_df$Order)]))
    }

    start <- (non_para_current_page() - 1) * non_para_items_per_page + 1
    end <- min(nrow(results_df), non_para_current_page() * non_para_items_per_page)
    display_data <- results_df[start:end, ]

    joined_data <- left_join(results_df, short_data, by = c("Compound" = common_column_value))
    non_para_data_stored(joined_data)

    if (num_groups > 2) {
      non_para_title <- "Kruskal-Wallis Test Results"
    } else {
      non_para_title <- "Mann–Whitney U-Test Results"
    }

    plot <- ggplot(display_data, aes(x = factor(Compound, levels = rev(display_data$Compound)), y = p_value)) +
      geom_segment(aes(xend = factor(Compound, levels = rev(display_data$Compound)), yend = 0), color = "skyblue") +
      geom_point(color = "skyblue", size = 3) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkred") +
      coord_flip() +
      labs(y = "P-Value", x = "Compound", title = non_para_title) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 11),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size = 17, hjust = 0.5))
    plot
  })

  # output$non_para_data <- DT::renderDT({
  #   req(non_para_func(), input$non_para_select)
  #   DT::datatable(
  #     non_para_func(),
  #     options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })

  output$non_para_data <- renderPlot({
    if (!is.null(non_para_func())) {
      return(non_para_func())
    }
    NULL
  }, height = 550)

  output$hover_non_para_hist <- renderPlot({
    req(input$non_para_hover, processed_data(), non_para_data_stored(), input$non_para_norm, input$non_para_trans, input$non_para_sca, input$non_para_sorting)
    hover <- input$non_para_hover
    stat_data <- processed_data()
    common_column_value <- common_column()
    non_para_data <- non_para_data_stored()
    plot_data <- non_para_data %>%
      dplyr::select(Compound, 5:ncol(.))

    non_para_items_per_page <- 50
    page_offset <- (non_para_current_page() - 1) * non_para_items_per_page
    index_on_page <- non_para_items_per_page - round(hover$y) + 1
    index <- page_offset + index_on_page
    if (index < 1 || index > nrow(non_para_data)) return()

    hovered_compound <- non_para_data[index, "Compound"]
    non_para_data_p_value <- non_para_data[index, "p_value"]
    hovered_measurements <- plot_data[index,]

    long_data <- hovered_measurements %>%
      pivot_longer(cols = -1, names_to = "sample", values_to = "value") %>%
      separate(sample, into = c("group", "sample"), sep = "(?<=group\\d{1})_(?=\\w)")

    if (!is.null(hovered_measurements) && length(hovered_measurements) > 0) {
      dummy <- long_data %>%
        group_by(group) %>%
        summarize(mean = mean(value))

      ggplot(long_data, aes(x = value, fill = group)) +
        geom_density(alpha = 0.5) +
        geom_vline(data = dummy, aes(xintercept = mean, color = group), linetype = "longdash") +
        labs(title = paste("Density Plot for", hovered_measurements$Compound, "\nP-value:", format(non_para_data_p_value, digits = 4)),
             x = "Measurements",
             y = "Density") +
        theme_minimal()

    } else {
      plot.new()
      text(0.5, 0.5, "No measurements available", cex = 1.5)
    }

  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_percent_above_005 <- renderText({
        req(non_para_data_stored(), input$non_para_norm, input$non_para_trans, input$non_para_sca)
        joined_data <- non_para_data_stored()
        percent_above_005 <- mean(joined_data$p_value > 0.05, na.rm = TRUE) * 100
        paste0("% of P-Values Above 0.05: ", round(percent_above_005, 6), " %")
      })
    } else {
      output$non_para_percent_above_005 <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_highest_p_val <- renderText({
        req(non_para_data_stored(), input$non_para_norm, input$non_para_trans, input$non_para_sca)
        joined_data <- non_para_data_stored()
        high_p_val <- joined_data %>%
          filter(p_value == max(p_value, na.rm = TRUE)) %>%
          dplyr::select(Compound, p_value)
        paste0("Highest p-value: ", as.character(high_p_val$Compound), ", ", round(as.numeric(high_p_val$p_value), 6))
      })
    } else {
      output$non_para_highest_p_val <- renderText({
        ""
      })
    }
  })

  observe({
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests") {
      output$non_para_lowest_p_val <- renderText({
        req(non_para_data_stored(), input$non_para_norm, input$non_para_trans, input$non_para_sca)
        joined_data <- non_para_data_stored()
        low_p_val <- joined_data %>%
          filter(p_value == min(p_value, na.rm = TRUE)) %>%
          dplyr::select(Compound, p_value)
        paste0("Lowest p-value: ", as.character(low_p_val$Compound), ", ", round(as.numeric(low_p_val$p_value), 6))
      })
    } else {
      output$non_para_lowest_p_val <- renderText({
        ""
      })
    }
  })

  extract_dunn_results <- function(dunn_list) {
    dunn_df <- do.call(rbind, lapply(names(dunn_list), function(compound) {
      dunn_result <- dunn_list[[compound]]
      dunn_result$Compound <- compound
      return(dunn_result)
    }))
    rownames(dunn_df) <- NULL
    return(dunn_df)
  }

  output$dunn_results_table <- DT::renderDT({
    req(dunn_summary_df_download(), input$non_tukey_check)
    DT::datatable(
      dunn_summary_df_download(),
      options = list(pageLength = 10, scrollX = TRUE))
  })

  observe({
    req(input$non_tukey_check)
    if (!is.null(processed_data()) && active_tab() == "Non-Parametric tests" && input$non_tukey_check == TRUE) {
      output$non_tukey_results <- renderPlot({
        req(non_tukey_results_list_stored(), processed_data(), input$non_para_hover, non_para_data_stored())

        if (is.null(non_tukey_results_list_stored()) || length(non_tukey_results_list_stored()) == 0) return()

        tukey_results_df <- extract_dunn_results(non_tukey_results_list_stored())

        hover <- input$non_para_hover
        stat_data <- processed_data()
        non_para_data <- non_para_data_stored()
        plot_data <- non_para_data %>%
          dplyr::select(Compound, 5:ncol(.))

        para_items_per_page <- 50
        page_offset <- (non_para_current_page() - 1) * para_items_per_page
        index_on_page <- para_items_per_page - round(hover$y) + 1
        index <- page_offset + index_on_page
        if (index < 1 || index > nrow(non_para_data)) return()

        hovered_compound <- non_para_data[index, "Compound"]

        if (nrow(tukey_results_df) == 0) return()

        tukey_results_df <- tukey_results_df %>%
          dplyr::filter(Compound == hovered_compound)

        if (nrow(tukey_results_df) == 0) return()

        ggplot(tukey_results_df, aes(x = Comparison, y = Z)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_text(aes(label = paste("p-value: ", round(`P.adj`, 4))), vjust = -0.5) +
          labs(title = "Dunn Test: Mean Differences Between Groups",
               x = "Comparison", y = "Mean Difference") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          expand_limits(y = max(tukey_results_df$Z) * 1.1)
      })
    } else if (input$non_tukey_check == FALSE) {
      output$non_tukey_results <- renderUI({
        div()
      })
    }
  })


}
shinyApp(ui = ui, server = server)
}

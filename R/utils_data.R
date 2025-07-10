#' Create a Custom Shiny Modal with Adjustable Size and Class
#'
#' Found on https://stackoverflow.com/questions/63882483/how-to-adjust-shiny-modaldialog-width-to-a-dt-object-to-fully-show-the-table-i
#'
#' This utility function extends Shiny's modal dialog by allowing custom CSS classes
#' to be added to the `modal-dialog` container, enabling precise control over styling
#' (e.g., modal width). Use this instead of `modalDialog()` when you need more layout control.
#'
#' @param ... UI elements to include inside the modal body.
#' @param title Title of the modal. Can be a character string or UI tag (e.g., `div(...)`).
#' @param footer Footer content, typically buttons. Defaults to a dismiss button.
#' @param size Modal size: one of `"s"`, `"m"`, or `"l"` (small, medium, large).
#' @param easyClose Logical. If `TRUE`, modal can be closed by clicking outside or pressing ESC.
#' @param fade Logical. If `TRUE`, modal fades in/out.
#' @param idcss Optional custom CSS class to add to the `modal-dialog` element for fine-tuned styling.
#'
#' @return A modal dialog as a `shiny.tag` to be passed into `showModal()`.
#'
#' @examples
#' showModal(
#'   mymodal(
#'     idcss = "wide-modal",
#'     title = "Custom Modal",
#'     div("Modal body content"),
#'     footer = tagList(modalButton("Cancel"), actionButton("ok", "OK"))
#'   )
#' )
#'
#' @export
mymodal <- function (..., title = NULL, footer = modalButton("Dismiss"),
                     size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE, idcss = "") {
  size <- match.arg(size)
  cls <- if (fade) "modal fade" else "modal"

  div(
    id = "shiny-modal",
    class = cls,
    tabindex = "-1",
    `data-backdrop` = if (!easyClose) "static",
    `data-keyboard` = if (!easyClose) "false",
    div(
      class = paste("modal-dialog", idcss),
      class = switch(size,
                     s = "modal-sm",
                     m = NULL,
                     l = "modal-lg"),
      div(
        class = "modal-content",
        if (!is.null(title))
          div(class = "modal-header", tags$h4(class = "modal-title", title)),
        div(class = "modal-body", ...),
        if (!is.null(footer))
          div(class = "modal-footer", footer)
      )
    ),
    tags$script("$('#shiny-modal').modal().focus();")
  )
}

#' Show drag and drop modal for group setting
#'
#' @description A utils function
#'
#' @param loader A data frame to load
#' @param group_names Names of data groups
#' @return A drag and drop modal
#'
#' @export
#' @importFrom sortable add_rank_list
#' @importFrom sortable bucket_list
#' @noRd
show_manual_group_dnd_modal <- function(loader, ns, group_names) {
  df <- loader$get_data_excl_metadata()
  sample_cols <- setdiff(names(df), loader$get_compound_col())

  # Drag-and-drop buckets
  rank_lists <- lapply(seq_along(group_names), function(i) {
    labels <- if (i == 1) sample_cols else NULL
    sortable::add_rank_list(group_names[i], input_id = ns(paste0("group_", i)), labels = labels)
  })

  # Add "Remove" and "Other" buckets
  rank_lists <- append(rank_lists, list(
    sortable::add_rank_list("Remove", input_id = ns("group_exclude"), labels = NULL),
    sortable::add_rank_list("Other", input_id = ns("group_other"), labels = NULL)
  ))

  showModal(
    mymodal(
      idcss = "dnd-modal",  # applies to modal-dialog
      title = div("Manually Assign Groups (Drag & Drop)", class = "dnd-modal-title"),
      sortable::bucket_list(
        header = "Drag sample names into group buckets below:",
        group_name = "group_assign",
        orientation = "horizontal",
        !!!rank_lists
      ),
      footer = tagList(
        actionButton(ns("confirm_manual_groups"), "Confirm"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    )
  )
}

#' Show compound column and metadata modal
#'
#' @description A utils function
#'
#' @param column_choices A list of column names
#' @return A compound and metadata modal
#'
#' @export
#' @importFrom shinyjs disable
#' @noRd
show_compound_and_metadata_modal <- function(column_choices, ns) {
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
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == false", ns("has_metadata")),
        tagList(
          numericInput(ns("num_groups"), "Number of Groups", value = 2, min = 2, max = 8, step = 1),
          uiOutput(ns("custom_group_names_ui"))
        )
      )

    ),
    footer = tagList(
      actionButton(ns("confirm_compound_col"), "Confirm", class = "btn-primary"),
      modalButton("Cancel")
    ),
    size = "m",
    easyClose = FALSE
  ))
  shinyjs::disable("confirm_compound_col")
}

#' Transpose a data frame with proper row/column names
#'
#' @description A utils function
#'
#' @param data A data frame to transpose
#' @return A transposed data frame with cleaned headers
#'
#' @export
#' @importFrom tibble rownames_to_column
#' @importFrom janitor row_to_names
#' @importFrom dplyr %>%
#' @noRd
transpose_data <- function(data) {
  T_data <- data.frame(t(data)) %>%
    tibble::rownames_to_column() %>%
    janitor::row_to_names(row_number = 1)
  rownames(T_data) <- NULL
  return(T_data)
}

#' Check that all columns are numeric
#'
#' @description Ensures all columns in the data frame are numeric.
#'
#' @param data_loader A data frame to check
#'
#' @return The original data frame if valid; throws error otherwise
#'
#' @export
check_and_handle_non_numeric <- function(data_loader, ns) {
  compound_col <- data_loader()$get_compound_col()
  df <- data_loader()$get_data_excl_metadata()
  cols_to_check <- setdiff(names(df), compound_col)

  is_non_numeric <- !sapply(df[cols_to_check], is.numeric)
  non_numeric_cols <- cols_to_check[is_non_numeric]
  non_numeric_indices <- which(names(df) %in% non_numeric_cols)

  if (length(non_numeric_cols) == 0) return(NULL)

  column_choices <- lapply(non_numeric_cols, function(col) {
    selectInput(
      inputId = ns(paste0("action_", col)),
      label = col,
      choices = c("Remove column" = "remove", "Do nothing" = "none", "Convert to numeric" = "convert"),
      selected = "remove"
    )
  })

  showModal(modalDialog(
    title = "Non-Numeric Features Detected",
    size = "m",
    tagList(
      "Some features are not numeric. Choose how to handle each one:",
      tagList(column_choices)
    ),
    footer = tagList(
      actionButton(ns("apply_column_fixes"), "Apply Fixes"),
      modalButton("Cancel")
    )
  ))

  return(list(
    names = non_numeric_cols,
    indices = non_numeric_indices
  ))
}

#' Check for missing values in a data frame
#'
#' @description Ensures no values in the dataset are missing
#'
#' @param data_loader A data frame to check
#'
#' @return The original data frame if valid; throws error otherwise
#'
#' @export
#' @importFrom dplyr %>%
check_and_handle_nas <- function(data_loader, ns) {
  df <- data_loader()$get_data_excl_metadata()
  compound_col <- data_loader()$get_compound_col()
  cols_to_check <- setdiff(names(df), compound_col)

  na_summary <- sapply(df[cols_to_check], function(col) sum(is.na(col)))
  na_cols <- names(na_summary[na_summary > 0])

  if (length(na_cols) == 0) return(NULL)

  column_choices <- lapply(na_cols, function(col) {
    selectInput(
      inputId = ns(paste0("na_action_", col)),
      label = paste0(col, " (", na_summary[col], " NA)"),
      choices = c("Convert NA to 0" = "convert_zero", "Remove column" = "remove_col"),
      #, "Remove rows" = "remove_row"),
      selected = "convert_zero"
    )
  })

  showModal(modalDialog(
    title = "Missing Values Detected",
    size = "m",
    tagList(
      "Some features contain missing (NA) values. Choose how to handle them:",
      tagList(column_choices)
    ),
    footer = tagList(
      actionButton(ns("apply_na_fixes"), "Apply Fixes"),
      modalButton("Cancel")
    )
  ))

  return(na_cols)
}

#' Check for negative values in a data frame
#'
#' @description Ensures no values in the dataset are negative.
#'
#' @param data_loader A data loader object
#' @param ns The namespace function for UI elements
#'
#' @return NULL if no negatives found; otherwise returns affected columns
#'
#' @export
#' @importFrom dplyr %>%
check_no_negatives <- function(data_loader, ns) {
  df <- data_loader()$get_data_excl_metadata()
  compound_col <- data_loader()$get_compound_col()
  cols_to_check <- setdiff(names(df), compound_col)

  negative_summary <- sapply(df[cols_to_check], function(col) sum(col < 0, na.rm = TRUE))
  neg_cols <- names(negative_summary[negative_summary > 0])

  if (length(neg_cols) == 0) return(NULL)

  column_choices <- lapply(neg_cols, function(col) {
    selectInput(
      inputId = ns(paste0("neg_action_", col)),
      label = paste0(col, " (", negative_summary[col], " negatives)"),
      choices = c("Convert negatives to 0" = "convert_zero", "Remove column" = "remove_col"),
      selected = "convert_zero"
    )
  })

  showModal(modalDialog(
    title = "Negative Values Detected",
    size = "m",
    tagList(
      "Some features contain negative values. Choose how to handle them:",
      tagList(column_choices)
    ),
    footer = tagList(
      actionButton(ns("apply_neg_fixes"), "Apply Fixes"),
      modalButton("Cancel")
    )
  ))

  return(neg_cols)
}

#' Check for duplicated compound names
#'
#' @description Identifies and optionally allows renaming of duplicated entries in the compound column.
#'
#' @param data_loader A data loader object
#' @param ns The namespace function for UI elements
#'
#' @return Named character vector of new names (or NULL if no duplicates found)
#'
#' @export
check_and_handle_duplicates <- function(data_loader, ns) {
  df <- data_loader()$get_data_excl_metadata()
  compound_col <- data_loader()$get_compound_col()

  compound_names <- df[[compound_col]]
  dup_names <- unique(compound_names[duplicated(compound_names)])

  if (length(dup_names) == 0) return(NULL)

  rename_inputs <- list()
  input_counter <- 1

  for (name in dup_names) {
    indices <- which(compound_names == name)
    # Skip first occurrence
    for (j in 2:length(indices)) {
      input_id <- ns(paste0("rename_dup_", input_counter))
      rename_inputs[[input_counter]] <- textInput(
        inputId = input_id,
        label = paste0("Rename duplicate: ", name, " (occurrence ", j, ")"),
        value = paste0(name, "_", j)
      )
      input_counter <- input_counter + 1
    }
  }

  showModal(modalDialog(
    title = "Duplicated Compounds Detected",
    size = "m",
    tagList(
      "The following compound names are duplicated. Please provide unique replacements:",
      tagList(rename_inputs)
    ),
    footer = tagList(
      actionButton(ns("apply_dup_renames"), "Apply Renames"),
      modalButton("Cancel")
    )
  ))

  return(dup_names)
}

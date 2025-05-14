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
  non_numeric_cols <- cols_to_check[!sapply(df[cols_to_check], is.numeric)]

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

  return(non_numeric_cols)
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
      choices = c("Remove rows" = "remove_row", "Remove column" = "remove_col", "Convert NA to 0" = "convert_zero"),
      selected = "remove_row"
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
#' @param df A data frame to check
#'
#' @return The original data frame if valid; throws error otherwise
#'
#' @export
#' @importFrom assertr insist within_bounds error_stop
#' @importFrom dplyr %>%
check_no_negatives <- function(df) {
  df %>%
    insist(within_bounds(0, Inf), everything(), error_fun = error_stop)
}



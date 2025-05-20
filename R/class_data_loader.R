#' DataLoader: A class for loading CSV/XLSX data
#'
#' This R6 class detects file type and loads data accordingly.
#' For CSV, it auto-detects delimiter. For XLSX, it loads the first sheet
#' or stores sheet names if multiple are found.
#'
#' @docType class
#' @format R6Class object
#' @export
#'
#' @field data A reactive value holding the loaded dataset.
#' @field sheets Names of sheets in the Excel file (if applicable).
#' @field file_type The file extension (e.g., "csv", "xlsx").
#' @field file_path The full path to the uploaded file.
#' @field compound_col Name of the Compound column.
#' @field metadata_info Metadata group index.
DataLoader <- R6::R6Class("DataLoader",
  public = list(
    data = NULL,
    sheets = NULL,
    file_type = NULL,
    file_path = NULL,
    compound_col = NULL,
    metadata_info = NULL,

    #' @description
    #' Initialize the DataLoader instance.
    #' @param path Optional file path to a CSV file.
    initialize = function(path = NULL) {
      self$data <- reactiveVal()  # initialize as reactive

      if (!is.null(path)) {
        self$file_path <- path
        ext <- tools::file_ext(path)
        self$file_type <- ext

        if (ext %in% c("csv", "txt")) {
          self$data(private$read_delim_auto(path))
        } else if (ext %in% c("xls", "xlsx")) {
          self$sheets <- readxl::excel_sheets(path)
          if (length(self$sheets) == 1) {
            self$data(readxl::read_excel(path, sheet = 1))
          }
        } else {
          stop("Unsupported file type: ", ext)
        }
      }
    },

    #' @description
    #' Load a specific Excel sheet by name.
    #' @param sheet A string, the name of the sheet to load.
    load_sheet = function(sheet) {
      self$data(readxl::read_excel(self$file_path, sheet = sheet))
    },

    #' @description
    #' Load example data from inst/extdata/example_data.csv.
    load_example = function() {
      self$data(read.table(system.file("extdata", "example_data.csv", package = "MetaboHelpeR"), header = TRUE))
    },

    #' @description
    #' Return the currently loaded data as a data frame.
    get_data = function() {
      self$data()
    },

    #' @description
    #' Set the internal data.
    #' @param df A data frame to replace the current dataset.
    set_data = function(df) {
      self$data(df)
    },

    #' @description
    #' Designate the compound column.
    #' @param colname A string, the name of the compound column.
    set_compound_col = function(colname) {
      if (!colname %in% names(self$get_data())) {
        stop("Invalid compound column name")
      }
      self$compound_col <- colname
    },

    #' @description
    #' Return the compound column.
    get_compound_col = function() {
      self$compound_col
    },

    #' @description
    #' Designate metadata index.
    #' @param index Numeric, the metadata index number.
    set_metadata_info = function(index) {
      self$metadata_info <- list(index = index)

      df <- self$get_data()
      metadata_row <- df[index, , drop = TRUE]

      # Store group labels
      colnames_df <- names(metadata_row)
      compound_col_name <- self$compound_col
      compound_col_index <- which(colnames_df == compound_col_name)

      # Exclude compound column safely and preserve names
      if (length(compound_col_index) == 1) {
        group_vector <- metadata_row[-compound_col_index]
        group_vector <- as.character(group_vector)
        names(group_vector) <- names(metadata_row)[-compound_col_index]
      } else {
        warning("Compound column not found in metadata row; including all columns in group vector.")
        group_vector <- as.character(metadata_row)
        names(group_vector) <- names(metadata_row)
      }

      self$metadata_info$group_vector <- group_vector

      # Extract group names and counts
      unique_groups <- unique(group_vector)

      self$metadata_info$unique_groups <- unique_groups
      self$metadata_info$group_counts <- table(group_vector)
      self$metadata_info$group_indices <- lapply(split(seq_along(group_vector), group_vector), function(idxs) idxs + 1)
      self$metadata_info$group_colors <- setNames(
        scales::hue_pal()(length(unique_groups)),
        unique_groups
      )
    },

    #' @description
    #' Return metadata index
    get_metadata_info = function() {
      if (is.null(self$metadata_info)) return(NULL)
      self$metadata_info
    },

    #' @description
    #' Return group vector
    get_group_vector = function() {
      if (is.null(self$metadata_info)) return(NULL)
      self$metadata_info$group_vector
    },

    #' @description
    #' Return group indices
    get_group_indices = function() {
      if (is.null(self$metadata_info)) return(NULL)
      self$metadata_info$group_indices
    },

    #' @description
    #' Return group colors
    get_group_colors = function() {
      if (is.null(self$metadata_info)) return(NULL)
      self$metadata_info$group_colors
    },

    #' @description
    #' Return data without metadata.
    get_data_excl_metadata = function() {
      df <- self$get_data()
      if (is.null(self$metadata_info)) return(df)

      if (!is.null(self$metadata_info$index)) {
        df <- df[-self$metadata_info$index, , drop = FALSE]
      }

      # Try to convert all columns to numeric where possible
      df[] <- lapply(df, function(col) {
        if (is.character(col) || is.factor(col)) {
          suppressWarnings(num_col <- as.numeric(as.character(col)))
          if (all(!is.na(num_col) | is.na(col))) return(num_col)
        }
        return(col)
      })

      df
    },

    #' @description
    #' Return group metadata as a data frame (if applicable).
    get_group_metadata_df = function() {
      df <- self$get_data()
      info <- self$metadata_info
      if (is.null(info)) return(NULL)
      return(data.frame(df[info$index, , drop = TRUE]))
    },

    #' @description
    #' Set group metadata based on manual assignment
    #' @param sample_to_group A named character vector where names are sample column names, and values are group names
    #' @param group_colors A color vector
    set_manual_group_mapping = function(sample_to_group, group_colors = NULL) {
      self$metadata_info <- list(index = NULL)

      group_vector <- sample_to_group
      sample_names <- names(group_vector)

      if (!is.null(self$compound_col)) {
        group_vector <- group_vector[sample_names != self$compound_col]
      }

      self$metadata_info$group_vector <- group_vector
      self$metadata_info$unique_groups <- unique(group_vector)
      self$metadata_info$group_counts <- table(group_vector)

      # Convert sample names to column indices for group_indices
      col_indices <- match(names(group_vector), names(self$get_data()))
      self$metadata_info$group_indices <- split(col_indices, group_vector)

      self$metadata_info$group_colors <- if (!is.null(group_colors)) {
        group_colors
      } else {
        setNames(scales::hue_pal()(length(unique(group_vector))), unique(group_vector))
      }
    },


    #' @description
    #' Return numeric matrix of the data excluding metadata and compound column.
    get_data_matrix = function() {
      df <- self$get_data_excl_metadata()

      # Remove compound column if defined
      if (!is.null(self$compound_col) && self$compound_col %in% names(df)) {
        df <- df[ , setdiff(names(df), self$compound_col), drop = FALSE]
      }

      # Coerce to matrix, ensuring numeric conversion
      mat <- suppressWarnings({
        as.matrix(sapply(df, function(col) as.numeric(as.character(col))))
      })

      storage.mode(mat) <- "numeric"
      return(mat)
    }

  ),

  private = list(
    read_delim_auto = function(path) {
      first_line <- readLines(path, n = 1)
      delimiters <- c("," = ",", ";" = ";", "\t" = "\t", " " = " ")
      counts <- sapply(delimiters, function(d) length(strsplit(first_line, d)[[1]]))
      best_delim <- names(which.max(counts))
      read.delim(path, sep = best_delim, header = TRUE, stringsAsFactors = FALSE)
    }
  )
)

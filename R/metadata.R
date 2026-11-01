#' Read and validate sample metadata
#'
#' Reads the sample metadata CSV file and validates that all required
#' columns are present, sample identifiers are unique, and required
#' metadata values are not missing or blank.
#'
#' @param path Character string giving the path to the sample metadata CSV file.
#'
#' @return A data.frame containing validated sample metadata.
#'
#' @examples
#' read_sample_metadata()
#'
#' @export
read_sample_metadata <- function(path = "config/samples.csv") {
  if (!file.exists(path)) {
    stop("Sample metadata file does not exist: ", path)
  }

  metadata <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_columns <- c(
    "sample_id",
    "donor",
    "condition",
    "replicate",
    "sex",
    "data_dir"
  )

  missing_columns <- setdiff(
    required_columns,
    names(metadata)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Sample metadata is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (anyDuplicated(metadata$sample_id) > 0) {
    stop("sample_id values must be unique.")
  }

  required_metadata <- metadata[required_columns]

  has_missing_values <- anyNA(required_metadata)

  has_blank_values <- any(
    vapply(
      required_metadata,
      function(column) {
        any(trimws(as.character(column)) == "", na.rm = TRUE)
      },
      logical(1)
    )
  )

  if (has_missing_values || has_blank_values) {
    stop("Sample metadata contains missing or blank values.")
  }

  metadata
}
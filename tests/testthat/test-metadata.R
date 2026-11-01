source(
  testthat::test_path("..", "..", "R", "metadata.R")
)
# Successful input --------
testthat::test_that("sample metadata loads correctly", {
  metadata_path <- testthat::test_path(
    "..",
    "..",
    "config",
    "samples.csv"
  )

  metadata <- read_sample_metadata(metadata_path)

  testthat::expect_s3_class(metadata, "data.frame")
  testthat::expect_equal(nrow(metadata), 4)

  testthat::expect_true(
    all(
      c(
        "sample_id",
        "donor",
        "condition",
        "replicate",
        "sex",
        "data_dir"
      ) %in% names(metadata)
    )
  )
})

# Invalid input ---------
testthat::test_that("duplicate sample IDs are rejected", {
  metadata <- data.frame(
    sample_id = c("sample_1", "sample_1"),
    donor = c("D1", "D2"),
    condition = c("COVID", "Healthy"),
    replicate = c("T1", "T1"),
    sex = c("F", "M"),
    data_dir = c("data/raw/sample_1", "data/raw/sample_2")
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(metadata, temp_file, row.names = FALSE)

  testthat::expect_error(
    read_sample_metadata(temp_file),
    "sample_id values must be unique"
  )
})


testthat::test_that("missing required columns are rejected", {
  metadata <- data.frame(
    sample_id = "sample_1",
    donor = "D1",
    condition = "COVID"
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(metadata, temp_file, row.names = FALSE)

  testthat::expect_error(
    read_sample_metadata(temp_file),
    "missing required columns"
  )
})


testthat::test_that("missing metadata values are rejected", {
  metadata <- data.frame(
    sample_id = "sample_1",
    donor = "D1",
    condition = NA,
    replicate = "T1",
    sex = "F",
    data_dir = "data/raw/sample_1"
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(metadata, temp_file, row.names = FALSE)

  testthat::expect_error(
    read_sample_metadata(temp_file),
    "contains missing or blank values"
  )
})

testthat::test_that("blank metadata values are rejected", {
  metadata <- data.frame(
    sample_id = "sample_1",
    donor = "D1",
    condition = "COVID",
    replicate = "T1",
    sex = "",
    data_dir = "data/raw/sample_1"
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(metadata, temp_file, row.names = FALSE)

  testthat::expect_error(
    read_sample_metadata(temp_file),
    "contains missing or blank values"
  )
})
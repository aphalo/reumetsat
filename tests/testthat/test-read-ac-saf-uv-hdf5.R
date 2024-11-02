test_that("reads one grid data file", {

  one.file.name <-
    system.file("extdata", "O3MOUV_L3_20240621_v02p02.HDF5",
                package = "surfaceuv", mustWork = TRUE)

  all.variables.no.QC <- c("Date", "Longitude", "Latitude", "DailyDoseUva", "DailyDoseUvb",
                              "DailyMaxDoseRateUva", "DailyMaxDoseRateUvb")
  all.variables <-
    c(all.variables.no.QC, "QualityFlags")

  grid.range <- data.frame(Longitude = c(-10.75, -4.75),
                           Latitude = c(35.25, 43.25))

  expect_equal(vars_AC_SAF_UV_hdf5(one.file.name), all.variables)
  expect_equal(vars_AC_SAF_UV_hdf5(one.file.name, keep.QC = FALSE),
               all.variables.no.QC)

  expect_equal(grid_AC_SAF_UV_hdf5(one.file.name), grid.range)

  expanded.grid <- grid_AC_SAF_UV_hdf5(one.file.name, expand = TRUE)
  expect_s3_class(expanded.grid, "data.frame", exact = TRUE)
  expect_named(expanded.grid, c("Longitude", "Latitude"))
  expect_equal(nrow(expanded.grid), 221)
  expect_equal(range(expanded.grid$Longitude), grid.range$Longitude)
  expect_equal(range(expanded.grid$Latitude), grid.range$Latitude)

  expect_equal(names(date_AC_SAF_UV_hdf5(one.file.name)), basename(one.file.name))
  expect_equal(unname(date_AC_SAF_UV_hdf5(one.file.name)), as.Date("2024-06-21"))
  expect_equal(date_AC_SAF_UV_hdf5(one.file.name, use.names = FALSE), as.Date("2024-06-21"))

  test1.df <- read_AC_SAF_UV_hdf5(one.file.name, verbose = FALSE)
  expect_s3_class(test1.df, "data.frame", exact = TRUE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1.df$Date)), 1)
  expect_equal(range(test1.df$Longitude), grid.range$Longitude)
  expect_equal(range(test1.df$Latitude), grid.range$Latitude)
  expect_equal(colnames(test1.df), all.variables)
  expect_equal(nrow(test1.df), 221)
  expect_equal(sum(is.na(test1.df)), 0)

  test1a.df <-
    read_AC_SAF_UV_hdf5(one.file.name, keep.QC = FALSE, verbose = FALSE)
  expect_s3_class(test1a.df, "data.frame", exact = TRUE)
  expect_s3_class(test1a.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1a.df$Date)), 1)
  expect_equal(range(test1a.df$Longitude), grid.range$Longitude)
  expect_equal(range(test1a.df$Latitude), grid.range$Latitude)
  expect_equal(colnames(test1a.df), all.variables.no.QC)
  expect_equal(nrow(test1a.df), 221)
  expect_equal(sum(is.na(test1a.df)), 0)

  vars.to.read <- c("DailyDoseUva", "DailyDoseUvb")

  test2.df <- read_AC_SAF_UV_hdf5(one.file.name,
                                vars.to.read = vars.to.read, verbose = FALSE)
  expect_s3_class(test2.df, "data.frame", exact = TRUE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test2.df$Date)), 1)
  expect_equal(colnames(test2.df),
               c("Date", "Longitude", "Latitude", vars.to.read))
  expect_equal(nrow(test2.df), 221)
  expect_equal(sum(is.na(test2.df)), 0)

})

test_that("reads two consistent grid data files", {

  two.file.names <-
    system.file("extdata",
                c("O3MOUV_L3_20240621_v02p02.HDF5",
                  "O3MOUV_L3_20240622_v02p02.HDF5"),
                package = "surfaceuv", mustWork = TRUE)

  all.variables <-
    c("Date", "Longitude", "Latitude", "DailyDoseUva", "DailyDoseUvb",
      "DailyMaxDoseRateUva", "DailyMaxDoseRateUvb", "QualityFlags")

  grid.range <- data.frame(Longitude = c(-10.75, -4.75),
                           Latitude = c(35.25, 43.25))

  expect_silent(vars_AC_SAF_UV_hdf5(two.file.names))
  expect_equal(vars_AC_SAF_UV_hdf5(two.file.names), all.variables)

  expect_silent(grid_AC_SAF_UV_hdf5(two.file.names))
  expect_equal(grid_AC_SAF_UV_hdf5(two.file.names), grid.range)

  expect_equal(names(date_AC_SAF_UV_hdf5(two.file.names)),
               basename(two.file.names))
  expect_equal(unname(date_AC_SAF_UV_hdf5(two.file.names)),
               as.Date(c("2024-06-21", "2024-06-22")))
  expect_equal(date_AC_SAF_UV_hdf5(two.file.names, use.names = FALSE),
               as.Date(c("2024-06-21", "2024-06-22")))

  test1.df <- read_AC_SAF_UV_hdf5(two.file.names, verbose = FALSE)
  expect_s3_class(test1.df, "data.frame", exact = TRUE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1.df$Date)), 2)
  expect_equal(colnames(test1.df), all.variables)
  expect_equal(nrow(test1.df), 442)
  expect_equal(sum(is.na(test1.df)), 0)

  vars.to.read <- c("DailyDoseUva", "DailyDoseUvb")

  test2.df <- read_AC_SAF_UV_hdf5(two.file.names,
                                vars.to.read = vars.to.read, verbose = FALSE)
  expect_s3_class(test2.df, "data.frame", exact = TRUE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test2.df$Date)), 2)
  expect_equal(colnames(test2.df),
               c("Date", "Longitude", "Latitude", vars.to.read))
  expect_equal(nrow(test2.df), 442)
  expect_equal(sum(is.na(test2.df)), 0)

})

test_that("reads two different grid data files", {

  two.file.names <-
    system.file("extdata",
                c("O3MOUV_L3_20240621_v02p02.HDF5",
                  "O3MOUV_L3_20241021_v02p02.HDF5"),
                package = "surfaceuv", mustWork = TRUE)

  all.variables <-
    c("Date", "Longitude", "Latitude", "DailyDoseUva", "DailyDoseUvb",
      "DailyMaxDoseRateUva", "DailyMaxDoseRateUvb", "QualityFlags",
      "DailyDoseDna", "DailyDoseEry", "DailyDosePlant", "DailyDoseVitd")

  shared.variables <-
    c("Date", "Longitude", "Latitude", "DailyDoseUva", "DailyDoseUvb",
      "QualityFlags")

  grid.range <- data.frame(Longitude = c(-10.75, -4.75),
                           Latitude = c(35.25, 43.25))

  expect_message(vars_AC_SAF_UV_hdf5(two.file.names))
  expect_setequal(vars_AC_SAF_UV_hdf5(two.file.names), shared.variables)
  expect_setequal(vars_AC_SAF_UV_hdf5(two.file.names, set.oper = "intersect"), shared.variables)
  expect_setequal(vars_AC_SAF_UV_hdf5(two.file.names, set.oper = "union"), all.variables)
  expect_error(vars_AC_SAF_UV_hdf5(two.file.names, set.oper = "setdiff"))

  expect_equal(grid_AC_SAF_UV_hdf5(two.file.names), grid.range)

  expect_equal(names(date_AC_SAF_UV_hdf5(two.file.names)),
               basename(two.file.names))
  expect_equal(unname(date_AC_SAF_UV_hdf5(two.file.names)),
               as.Date(c("2024-06-21", "2024-10-21")))
  expect_equal(date_AC_SAF_UV_hdf5(two.file.names, use.names = FALSE),
               as.Date(c("2024-06-21", "2024-10-21")))

  test1.df <- read_AC_SAF_UV_hdf5(two.file.names, verbose = FALSE,
                                  vars.to.read = vars_AC_SAF_UV_hdf5(two.file.names))
  expect_s3_class(test1.df, "data.frame", exact = TRUE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1.df$Date)), 2)
  expect_setequal(colnames(test1.df), shared.variables)
  expect_equal(nrow(test1.df), 442)
  expect_equal(sum(is.na(test1.df)), 0)

  vars.to.read <- c("DailyDoseUva", "DailyDoseUvb")

  test2.df <- read_AC_SAF_UV_hdf5(two.file.names,
                                  vars.to.read = vars.to.read, verbose = FALSE)
  expect_s3_class(test2.df, "data.frame", exact = TRUE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test2.df$Date)), 2)
  expect_setequal(colnames(test2.df),
               c("Date", "Longitude", "Latitude", vars.to.read))
  expect_equal(nrow(test2.df), 442)
  expect_equal(sum(is.na(test2.df)), 0)

})

test_that("errors are triggered", {

  one.file.name <-
    system.file("extdata", "O3MOUV_L3_20240621_v02p02.HDF5",
                package = "surfaceuv", mustWork = TRUE)

  # errors early with not accessible files
  expect_error(read_AC_SAF_UV_hdf5(c("missing-file1", one.file.name, "missing-file2"),
                                   data.product = "Surface UV",
                                   verbose = FALSE))
  expect_error(vars_AC_SAF_UV_hdf5(c("missing-file1", one.file.name, "missing-file2")))
  expect_error(grid_AC_SAF_UV_hdf5(c("missing-file1", one.file.name, "missing-file2")))
  expect_error(date_AC_SAF_UV_hdf5(c("missing-file1", one.file.name, "missing-file2")))

  expect_error(read_AC_SAF_UV_hdf5("missing-file", verbose = FALSE))
  expect_error(read_AC_SAF_UV_hdf5("O3MOUV_missing-file", verbose = FALSE))
  expect_error(read_AC_SAF_UV_hdf5(c(one.file.name, "missing-file"), verbose = FALSE))
  expect_error(read_AC_SAF_UV_hdf5(1L, verbose = FALSE))

  # fails early with wrong data product name
  expect_warning(read_AC_SAF_UV_hdf5(one.file.name,
                                     data.product = "bad-product-name",
                                     verbose = FALSE))
  # case insensitive
  expect_no_error(z <- read_AC_SAF_UV_hdf5(one.file.name,
                                           data.product = "surface uv",
                                           verbose = FALSE))

  expect_no_error(z <- read_AC_SAF_UV_hdf5(one.file.name,
                                           data.product = "O3MOUV",
                                           verbose = FALSE))

  expect_no_error(z <- read_AC_SAF_UV_hdf5(one.file.name,
                                           data.product = "O3moUV",
                                           verbose = FALSE))

  # verbose works
  expect_no_error(z <- read_AC_SAF_UV_hdf5(one.file.name, verbose = TRUE))

})

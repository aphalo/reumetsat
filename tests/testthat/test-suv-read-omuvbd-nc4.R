test_that("reads one OMI grid data file", {

  one.file.name <-
    system.file("extdata", "OMI-Aura_L3-OMUVBd_2023m1001_v003.nc4",
                package = "surfaceuv", mustWork = TRUE)

  all.variables <- c("CloudOpticalThickness", "ErythemalDailyDose",
                     "ErythemalDoseRate", "Irradiance305", "Irradiance310",
                     "Irradiance324", "Irradiance380", "UVindex",
                     "Longitude", "Latitude", "Date")

  grid.range <- data.frame(Longitude = c(24.5, 26.5),
                           Latitude = c(58.5, 60.5))

  expect_equal(sUV_vars_OMUVBd_nc4(one.file.name), all.variables)

  expect_equal(sUV_grid_OMUVBd_nc4(one.file.name), grid.range)

  expanded.grid <- sUV_grid_OMUVBd_nc4(one.file.name, expand = TRUE)
  expect_s3_class(expanded.grid, "data.frame", exact = FALSE)
  expect_named(expanded.grid, c("Longitude", "Latitude"))
  expect_equal(nrow(expanded.grid), 9)
  expect_equal(range(expanded.grid$Longitude), grid.range$Longitude)
  expect_equal(range(expanded.grid$Latitude), grid.range$Latitude)

  expect_equal(names(sUV_date_OMUVBd_nc4(one.file.name)), basename(one.file.name))
  expect_equal(unname(sUV_date_OMUVBd_nc4(one.file.name)), as.Date("2023-10-01"))
  expect_equal(sUV_date_OMUVBd_nc4(one.file.name, use.names = FALSE), as.Date("2023-10-01"))

  test1.df <- sUV_read_OMUVBd_nc4(one.file.name, verbose = FALSE)
  expect_s3_class(test1.df, "data.frame", exact = FALSE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1.df$Date)), 1)
  expect_equal(range(test1.df$Longitude), grid.range$Longitude)
  expect_equal(range(test1.df$Latitude), grid.range$Latitude)
  expect_equal(colnames(test1.df), all.variables)
  expect_equal(nrow(test1.df), 9)
  expect_equal(sum(is.na(test1.df)), 0)

  vars.to.read <- c("CloudOpticalThickness", "UVindex")

  test2.df <- sUV_read_OMUVBd_nc4(one.file.name,
                                  vars.to.read = vars.to.read, verbose = FALSE)
  expect_s3_class(test2.df, "data.frame", exact = FALSE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test2.df$Date)), 1)
  expect_equal(colnames(test2.df),
               c(vars.to.read, "Longitude", "Latitude", "Date"))
  expect_equal(nrow(test2.df), 9)
  expect_equal(sum(is.na(test2.df)), 0)

})

test_that("reads multiple consistent NetCDF4 grid data files", {

  path.to.files <-
    system.file("extdata",
                package = "surfaceuv", mustWork = TRUE)

  file.names <- list.files(path.to.files, pattern = "*.nc4$", full.names = TRUE)

  all.variables <- c("CloudOpticalThickness", "ErythemalDailyDose",
                     "ErythemalDoseRate", "Irradiance305", "Irradiance310",
                     "Irradiance324", "Irradiance380", "UVindex",
                     "Longitude", "Latitude", "Date")

  grid.range <- data.frame(Longitude = c(24.5, 26.5),
                           Latitude = c(58.5, 60.5))

  expect_silent(sUV_vars_OMUVBd_nc4(file.names))
  expect_equal(sUV_vars_OMUVBd_nc4(file.names), all.variables)

  expect_silent(sUV_grid_OMUVBd_nc4(file.names))
  expect_equal(sUV_grid_OMUVBd_nc4(file.names), grid.range)

  expect_equal(names(sUV_date_OMUVBd_nc4(file.names)),
               basename(file.names))
  expect_equal(unname(sUV_date_OMUVBd_nc4(file.names)),
               as.Date(c("2023-10-01", "2023-10-02", "2023-10-03")))
  expect_equal(sUV_date_OMUVBd_nc4(file.names, use.names = FALSE),
               as.Date(c("2023-10-01", "2023-10-02", "2023-10-03")))

  test1.df <- sUV_read_OMUVBd_nc4(file.names, verbose = FALSE)
  expect_s3_class(test1.df, "data.frame", exact = FALSE)
  expect_s3_class(test1.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test1.df$Date)), 3)
  expect_equal(colnames(test1.df), all.variables)
  expect_equal(nrow(test1.df), 27)
  expect_equal(sum(is.na(test1.df)), 0)

  vars.to.read <- c("CloudOpticalThickness", "UVindex")

  test2.df <- sUV_read_OMUVBd_nc4(file.names,
                                  vars.to.read = vars.to.read, verbose = FALSE)
  expect_s3_class(test2.df, "data.frame", exact = FALSE)
  expect_s3_class(test2.df$Date, "Date", exact = TRUE)
  expect_equal(length(unique(test2.df$Date)), 3)
  expect_equal(colnames(test2.df),
               c(vars.to.read, "Longitude", "Latitude", "Date"))
  expect_equal(nrow(test2.df), 27)
  expect_equal(sum(is.na(test2.df)), 0)

})

test_that("errors are triggered", {

  one.file.name <-
    system.file("extdata", "OMI-Aura_L3-OMUVBd_2023m1001_v003.nc4",
                package = "surfaceuv", mustWork = TRUE)

  # errors early with not accessible files
  expect_error(sUV_read_OMUVBd_nc4(c("missing-file1", one.file.name, "missing-file2"),
                                   data.product = "Surface UV",
                                   verbose = FALSE))
  expect_error(sUV_vars_OMUVBd_nc4(c("missing-file1", one.file.name, "missing-file2")))
  expect_error(sUV_vars_OMUVBd_nc4(one.file.name, set.oper = "bad.oper"))
  expect_error(sUV_grid_OMUVBd_nc4(c("missing-file1", one.file.name, "missing-file2")))
  expect_error(sUV_date_OMUVBd_nc4(c("missing-file1", one.file.name, "missing-file2")))

  expect_error(sUV_read_OMUVBd_nc4("missing-file", verbose = FALSE))
  expect_error(sUV_read_OMUVBd_nc4(c(one.file.name, "missing-file"), verbose = FALSE))
  expect_error(sUV_read_OMUVBd_nc4(1L, verbose = FALSE))

  # verbose works
  expect_no_error(z <- sUV_read_OMUVBd_nc4(one.file.name, verbose = TRUE))

})
